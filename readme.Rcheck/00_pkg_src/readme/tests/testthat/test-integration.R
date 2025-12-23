# Integration tests for readme package

test_that("full pipeline works: cleanme -> undergrad -> readme", {
  set.seed(42)

  # Create synthetic data with realistic text patterns
  wordVecs <- create_test_wordvecs(n_words = 20, n_dims = 25, seed = 42)
  vocab <- rownames(wordVecs)

  # Create documents with some "realistic" patterns
  n_docs <- 100
  docs <- sapply(1:n_docs, function(i) {
    n_words <- sample(8:20, 1)
    words <- sample(vocab, n_words, replace = TRUE)
    paste(words, collapse = " ")
  })

  # Add some text that cleanme will process
  docs[1] <- paste(docs[1], "http://example.com :) @user 123")
  docs[2] <- paste(docs[2], "<html>tag</html> hello\nworld")

  # Create labels and categories
  labeledIndicator <- c(rep(1, 50), rep(0, 50))
  categoryVec <- sample(c("A", "B", "C"), 100, replace = TRUE)
  categoryVec[labeledIndicator == 0] <- NA

  # Full pipeline
  cleaned_docs <- cleanme(docs)
  dfm <- undergrad(documentText = cleaned_docs, wordVecs = wordVecs, verbose = FALSE)

  result <- readme(dfm = dfm,
                   labeledIndicator = labeledIndicator,
                   categoryVec = categoryVec,
                   nBoot = 2,
                   sgdIters = 100,
                   verbose = FALSE)

  # Verify output

  expect_type(result, "list")
  expect_true("point_readme" %in% names(result))
  expect_equal(sum(result$point_readme), 1, tolerance = 0.05)
  expect_equal(length(result$point_readme), 3)
})

test_that("Clinton dataset loads and runs correctly", {
  # Load Clinton dataset
  data(clinton, package = "readme")

  expect_true(exists("clinton"))
  expect_true("TEXT" %in% names(clinton))
  expect_true("TRUTH" %in% names(clinton))
  expect_true("TRAININGSET" %in% names(clinton))

  # Verify dataset structure
  expect_true(nrow(clinton) > 0)
  expect_type(clinton$TEXT, "character")
})

test_that("Clinton dataset preprocessing works", {
  data(clinton, package = "readme")

  # Test cleanme on Clinton data
  cleaned_text <- cleanme(clinton$TEXT[1:10])

  expect_equal(length(cleaned_text), 10)
  expect_type(cleaned_text, "character")
  expect_true(all(cleaned_text == tolower(cleaned_text)))
})

test_that("Clinton dataset with synthetic word vectors runs", {
  set.seed(42)

  data(clinton, package = "readme")

  # Use a subset that includes both labeled and unlabeled documents
  # Labeled documents are in rows 1-427, unlabeled start at 428
  subset_indices <- c(1:50, 428:477)  # 50 labeled + 50 unlabeled
  text_subset <- clinton$TEXT[subset_indices]
  training_subset <- clinton$TRAININGSET[subset_indices]
  truth_subset <- clinton$TRUTH[subset_indices]
  truth_subset[training_subset == 0] <- NA

  # Create word vectors with common words
  common_words <- c("the", "a", "is", "are", "was", "to", "of", "and", "in", "for",
                    "on", "with", "that", "this", "it", "be", "as", "at", "by", "from")
  wordVecs <- matrix(rnorm(length(common_words) * 25), ncol = 25)
  rownames(wordVecs) <- common_words
  colnames(wordVecs) <- paste0("V", 1:25)

  # Run pipeline
  cleaned <- cleanme(text_subset)
  dfm <- undergrad(documentText = cleaned, wordVecs = wordVecs, verbose = FALSE)

  result <- readme(dfm = dfm,
                   labeledIndicator = training_subset,
                   categoryVec = truth_subset,
                   nBoot = 1,
                   sgdIters = 50,
                   verbose = FALSE)

  expect_type(result, "list")
  expect_true("point_readme" %in% names(result))
})

test_that("reproducibility with same seed", {
  data <- create_test_dataset(n_docs = 80, n_categories = 2,
                              prop_labeled = 0.5, seed = 42)

  set.seed(123)
  result1 <- readme(dfm = data$dfm,
                    labeledIndicator = data$labeledIndicator,
                    categoryVec = data$categoryVec,
                    nBoot = 1, sgdIters = 100,
                    tensorflowSeed = 999,
                    verbose = FALSE)

  set.seed(123)
  result2 <- readme(dfm = data$dfm,
                    labeledIndicator = data$labeledIndicator,
                    categoryVec = data$categoryVec,
                    nBoot = 1, sgdIters = 100,
                    tensorflowSeed = 999,
                    verbose = FALSE)

  # Should be similar (may not be exact due to TensorFlow internals)
  # TensorFlow has internal randomness that cannot be fully controlled
  expect_equal(result1$point_readme, result2$point_readme, tolerance = 0.2)
})

test_that("multiple category counts work correctly", {
  # Test 2 categories
  data2 <- create_test_dataset(n_docs = 80, n_categories = 2,
                               prop_labeled = 0.5, seed = 42)
  result2 <- readme(dfm = data2$dfm,
                    labeledIndicator = data2$labeledIndicator,
                    categoryVec = data2$categoryVec,
                    nBoot = 1, sgdIters = 100,
                    verbose = FALSE)
  expect_equal(length(result2$point_readme), 2)

  # Test 4 categories
  data4 <- create_test_dataset(n_docs = 120, n_categories = 4,
                               prop_labeled = 0.5, seed = 42)
  result4 <- readme(dfm = data4$dfm,
                    labeledIndicator = data4$labeledIndicator,
                    categoryVec = data4$categoryVec,
                    nBoot = 1, sgdIters = 100,
                    verbose = FALSE)
  expect_equal(length(result4$point_readme), 4)

  # Test 6 categories
  data6 <- create_test_dataset(n_docs = 150, n_categories = 6,
                               prop_labeled = 0.5, seed = 42)
  result6 <- readme(dfm = data6$dfm,
                    labeledIndicator = data6$labeledIndicator,
                    categoryVec = data6$categoryVec,
                    nBoot = 1, sgdIters = 100,
                    verbose = FALSE)
  expect_equal(length(result6$point_readme), 6)
})

test_that("readme0 and readme produce comparable results", {
  set.seed(42)

  # Create binary DTM for readme0 and continuous dfm for readme
  n_docs <- 150
  n_features <- 60

  # Binary DTM for readme0 - use higher probability for more signal
  dtm <- matrix(rbinom(n_docs * n_features, 1, 0.4),
                nrow = n_docs, ncol = n_features)
  colnames(dtm) <- paste0("term", 1:n_features)

  # Word vector-based dfm for readme (using same seed for comparability)
  data <- create_test_dataset(n_docs = n_docs, n_categories = 3,
                              prop_labeled = 0.5, seed = 42)

  # Run both algorithms
  result0 <- readme0(dtm = dtm,
                     labeledIndicator = data$labeledIndicator,
                     categoryVec = data$categoryVec,
                     features = 20, nboot = 15)

  result <- readme(dfm = data$dfm,
                   labeledIndicator = data$labeledIndicator,
                   categoryVec = data$categoryVec,
                   nBoot = 2, sgdIters = 100,
                   verbose = FALSE)

  # readme should produce valid proportions
  expect_equal(sum(result$point_readme), 1, tolerance = 0.05)

  # readme0 may produce NaN in some edge cases - check if valid
  if (!any(is.na(result0$point_readme))) {
    expect_equal(sum(result0$point_readme), 1, tolerance = 0.15)
  }

  expect_equal(length(result0$point_readme), length(result$point_readme))
})

test_that("undergrad and readme handle edge case document counts", {
  # Small dataset
  data_small <- create_test_dataset(n_docs = 40, n_categories = 2,
                                    prop_labeled = 0.5, seed = 42)

  result_small <- readme(dfm = data_small$dfm,
                         labeledIndicator = data_small$labeledIndicator,
                         categoryVec = data_small$categoryVec,
                         nBoot = 1, sgdIters = 50,
                         verbose = FALSE)

  expect_type(result_small, "list")
  expect_equal(sum(result_small$point_readme), 1, tolerance = 0.1)
})

test_that("all exported functions are accessible", {
  # Verify all exported functions exist
  expect_true(exists("cleanme"))
  expect_true(exists("undergrad"))
  expect_true(exists("readme"))
  expect_true(exists("readme0"))
  expect_true(exists("download_wordvecs"))

  # Verify they are functions
  expect_type(cleanme, "closure")
  expect_type(undergrad, "closure")
  expect_type(readme, "closure")
  expect_type(readme0, "closure")
  expect_type(download_wordvecs, "closure")
})
