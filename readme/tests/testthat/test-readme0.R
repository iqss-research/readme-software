# Tests for readme0() function (legacy algorithm)

test_that("readme0 returns valid proportion estimates", {
  set.seed(42)

  # Create test data
  n_docs <- 100
  n_features <- 30

  dtm <- create_test_dtm(n_docs = n_docs, n_features = n_features)
  labeledIndicator <- create_test_labels(n_docs = n_docs, prop_labeled = 0.6)
  categoryVec <- create_test_categories(n_docs = n_docs, n_categories = 3)

  result <- readme0(dtm = dtm,
                    labeledIndicator = labeledIndicator,
                    categoryVec = categoryVec,
                    features = 10,
                    nboot = 5)

  expect_type(result, "list")
  expect_true("point_readme" %in% names(result))

  # Proportions should sum to approximately 1
  expect_equal(sum(result$point_readme), 1, tolerance = 0.1)

  # All proportions should be non-negative (or NA)
  expect_true(all(result$point_readme >= 0 | is.na(result$point_readme)))
})

test_that("readme0 errors when features > ncol(dtm)", {
  set.seed(42)

  dtm <- create_test_dtm(n_docs = 50, n_features = 10)
  labeledIndicator <- create_test_labels(n_docs = 50)
  categoryVec <- create_test_categories(n_docs = 50, n_categories = 2)

  expect_error(
    readme0(dtm = dtm, labeledIndicator = labeledIndicator,
            categoryVec = categoryVec, features = 20),
    "features.*greater"
  )
})

test_that("readme0 handles different nboot values", {
  set.seed(42)

  dtm <- create_test_dtm(n_docs = 80, n_features = 25)
  labeledIndicator <- create_test_labels(n_docs = 80, prop_labeled = 0.5)
  categoryVec <- create_test_categories(n_docs = 80, n_categories = 2)

  # Small nboot
  result1 <- readme0(dtm = dtm, labeledIndicator = labeledIndicator,
                     categoryVec = categoryVec, features = 8, nboot = 3)

  # Larger nboot
  result2 <- readme0(dtm = dtm, labeledIndicator = labeledIndicator,
                     categoryVec = categoryVec, features = 8, nboot = 15)

  expect_equal(length(result1$point_readme), length(result2$point_readme))
})

test_that("readme0 handles probWt parameter", {
  set.seed(42)

  dtm <- create_test_dtm(n_docs = 80, n_features = 25)
  labeledIndicator <- create_test_labels(n_docs = 80)
  categoryVec <- create_test_categories(n_docs = 80, n_categories = 2)

  # Default probWt = 1
  result1 <- readme0(dtm = dtm, labeledIndicator = labeledIndicator,
                     categoryVec = categoryVec, features = 8, probWt = 1)

  # probWt = 0 (uniform)
  result2 <- readme0(dtm = dtm, labeledIndicator = labeledIndicator,
                     categoryVec = categoryVec, features = 8, probWt = 0)

  expect_type(result1, "list")
  expect_type(result2, "list")
})

test_that("readme0 output has correct category names", {
  set.seed(42)

  n_docs <- 60
  dtm <- create_test_dtm(n_docs = n_docs, n_features = 20)
  labeledIndicator <- c(rep(1, 30), rep(0, 30))  # First 30 labeled

  # Create categories - make sure labeled set has all categories
  categoryVec <- rep(c("TypeA", "TypeB", "TypeC"), each = 20)

  result <- readme0(dtm = dtm, labeledIndicator = labeledIndicator,
                    categoryVec = categoryVec, features = 8, nboot = 5)

  # Should have categories from the labeled set
  labeled_cats <- unique(categoryVec[labeledIndicator == 1])
  expect_true(all(names(result$point_readme) %in% labeled_cats))
})

test_that("readme0 handles two categories", {
  set.seed(42)

  dtm <- create_test_dtm(n_docs = 60, n_features = 20)
  labeledIndicator <- create_test_labels(n_docs = 60)
  categoryVec <- create_test_categories(n_docs = 60, n_categories = 2)

  result <- readme0(dtm = dtm, labeledIndicator = labeledIndicator,
                    categoryVec = categoryVec, features = 8, nboot = 5)

  expect_equal(length(result$point_readme), 2)
})

test_that("readme0 handles four categories", {
  set.seed(42)

  dtm <- create_test_dtm(n_docs = 100, n_features = 30)
  labeledIndicator <- create_test_labels(n_docs = 100)
  categoryVec <- create_test_categories(n_docs = 100, n_categories = 4)

  result <- readme0(dtm = dtm, labeledIndicator = labeledIndicator,
                    categoryVec = categoryVec, features = 10, nboot = 5)

  expect_equal(length(result$point_readme), 4)
})

test_that("readme0 is reproducible with set.seed", {
  dtm <- create_test_dtm(n_docs = 80, n_features = 25, seed = 123)
  labeledIndicator <- create_test_labels(n_docs = 80, seed = 123)
  categoryVec <- create_test_categories(n_docs = 80, n_categories = 3, seed = 123)

  set.seed(999)
  result1 <- readme0(dtm = dtm, labeledIndicator = labeledIndicator,
                     categoryVec = categoryVec, features = 8, nboot = 10)

  set.seed(999)
  result2 <- readme0(dtm = dtm, labeledIndicator = labeledIndicator,
                     categoryVec = categoryVec, features = 8, nboot = 10)

  expect_equal(result1$point_readme, result2$point_readme)
})
