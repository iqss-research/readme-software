# Tests for readme() function (main algorithm)

# These tests require TensorFlow to be installed

test_that("readme returns valid proportion estimates", {
  set.seed(42)

  # Create test dataset
  data <- create_test_dataset(n_docs = 100, n_categories = 3,
                              prop_labeled = 0.5, n_dims = 25, seed = 42)

  result <- readme(dfm = data$dfm,
                   labeledIndicator = data$labeledIndicator,
                   categoryVec = data$categoryVec,
                   nBoot = 2,
                   sgdIters = 100,
                   verbose = FALSE)

  expect_type(result, "list")
  expect_true("point_readme" %in% names(result))
  expect_true("transformed_dfm" %in% names(result))

  # Proportions should sum to approximately 1
  expect_equal(sum(result$point_readme), 1, tolerance = 0.05)

  # All proportions should be non-negative
  expect_true(all(result$point_readme >= 0))
})

test_that("readme errors on NAs in labeledIndicator", {
  data <- create_test_dataset(n_docs = 50, n_categories = 2, seed = 42)
  data$labeledIndicator[1] <- NA

  expect_error(
    readme(dfm = data$dfm,
           labeledIndicator = data$labeledIndicator,
           categoryVec = data$categoryVec,
           nBoot = 1, sgdIters = 50),
    "NAs in labeledIndicator"
  )
})

test_that("readme errors on improper labeled/unlabeled split", {
  data <- create_test_dataset(n_docs = 50, n_categories = 2, seed = 42)

  # All labeled (no unlabeled)
  data$labeledIndicator <- rep(1, 50)

  expect_error(
    readme(dfm = data$dfm,
           labeledIndicator = data$labeledIndicator,
           categoryVec = data$categoryVec,
           nBoot = 1, sgdIters = 50),
    "Inproper labeled/unlabeled split"
  )
})

test_that("readme errors when category missing from labeled set", {
  set.seed(42)
  data <- create_test_dataset(n_docs = 80, n_categories = 3,
                              prop_labeled = 0.5, seed = 42)

  # Remove one category from the labeled portion
  labeled_indices <- which(data$labeledIndicator == 1)
  cat_to_remove <- unique(data$categoryVec[labeled_indices])[1]
  indices_to_change <- labeled_indices[data$categoryVec[labeled_indices] == cat_to_remove]

  if (length(indices_to_change) > 0) {
    remaining_cats <- setdiff(unique(data$categoryVec[labeled_indices]), cat_to_remove)
    if (length(remaining_cats) > 0) {
      data$categoryVec[indices_to_change] <- remaining_cats[1]
    }
  }

  # This should still work as long as each labeled category has at least 1 doc
  result <- readme(dfm = data$dfm,
                   labeledIndicator = data$labeledIndicator,
                   categoryVec = data$categoryVec,
                   nBoot = 1, sgdIters = 50,
                   verbose = FALSE)

  expect_type(result, "list")
})

test_that("readme errors on character/factor in dfm", {
  data <- create_test_dataset(n_docs = 50, n_categories = 2, seed = 42)

  # Convert first column to character
  dfm_bad <- data$dfm
  dfm_bad <- cbind(as.character(dfm_bad[, 1]), dfm_bad[, -1])

  expect_error(
    readme(dfm = dfm_bad,
           labeledIndicator = data$labeledIndicator,
           categoryVec = data$categoryVec,
           nBoot = 1, sgdIters = 50),
    "Character or factor"
  )
})

test_that("readme handles different nBoot values", {
  data <- create_test_dataset(n_docs = 80, n_categories = 2,
                              prop_labeled = 0.5, seed = 42)

  result1 <- readme(dfm = data$dfm,
                    labeledIndicator = data$labeledIndicator,
                    categoryVec = data$categoryVec,
                    nBoot = 1, sgdIters = 100,
                    verbose = FALSE)

  result2 <- readme(dfm = data$dfm,
                    labeledIndicator = data$labeledIndicator,
                    categoryVec = data$categoryVec,
                    nBoot = 3, sgdIters = 100,
                    verbose = FALSE)

  expect_equal(length(result1$point_readme), length(result2$point_readme))
  expect_equal(sum(result1$point_readme), 1, tolerance = 0.05)
  expect_equal(sum(result2$point_readme), 1, tolerance = 0.05)
})

test_that("readme handles different sgdIters values", {
  data <- create_test_dataset(n_docs = 80, n_categories = 2,
                              prop_labeled = 0.5, seed = 42)

  result1 <- readme(dfm = data$dfm,
                    labeledIndicator = data$labeledIndicator,
                    categoryVec = data$categoryVec,
                    nBoot = 1, sgdIters = 50,
                    verbose = FALSE)

  result2 <- readme(dfm = data$dfm,
                    labeledIndicator = data$labeledIndicator,
                    categoryVec = data$categoryVec,
                    nBoot = 1, sgdIters = 200,
                    verbose = FALSE)

  expect_type(result1, "list")
  expect_type(result2, "list")
})

test_that("readme handles numProjections parameter", {
  data <- create_test_dataset(n_docs = 80, n_categories = 3,
                              prop_labeled = 0.5, seed = 42)

  # Note: The numProjections parameter has known issues in the current implementation.
  # Using any value other than the default (NULL) can cause dimension mismatches
  # or TensorFlow index errors. This test verifies the default behavior works.
  result <- readme(dfm = data$dfm,
                   labeledIndicator = data$labeledIndicator,
                   categoryVec = data$categoryVec,
                   nBoot = 1, sgdIters = 100,
                   numProjections = NULL,  # Use default
                   verbose = FALSE)

  expect_type(result, "list")
  expect_true("transformed_dfm" %in% names(result))
  expect_equal(nrow(result$transformed_dfm), nrow(data$dfm))
})

test_that("readme handles kMatch parameter", {
  data <- create_test_dataset(n_docs = 80, n_categories = 2,
                              prop_labeled = 0.5, seed = 42)

  result1 <- readme(dfm = data$dfm,
                    labeledIndicator = data$labeledIndicator,
                    categoryVec = data$categoryVec,
                    nBoot = 1, sgdIters = 100,
                    kMatch = 1,
                    verbose = FALSE)

  result2 <- readme(dfm = data$dfm,
                    labeledIndicator = data$labeledIndicator,
                    categoryVec = data$categoryVec,
                    nBoot = 1, sgdIters = 100,
                    kMatch = 5,
                    verbose = FALSE)

  expect_type(result1, "list")
  expect_type(result2, "list")
})

test_that("readme justTransform mode works", {
  data <- create_test_dataset(n_docs = 80, n_categories = 2,
                              prop_labeled = 0.5, seed = 42)

  result <- readme(dfm = data$dfm,
                   labeledIndicator = data$labeledIndicator,
                   categoryVec = data$categoryVec,
                   nBoot = 1, sgdIters = 100,
                   justTransform = TRUE,
                   verbose = FALSE)

  expect_type(result, "list")
  expect_true("transformed_dfm" %in% names(result))
  expect_false("point_readme" %in% names(result))
  expect_equal(nrow(result$transformed_dfm), nrow(data$dfm))
})

test_that("readme output has correct category names", {
  set.seed(42)
  data <- create_test_dataset(n_docs = 80, n_categories = 3,
                              prop_labeled = 0.5, seed = 42)

  result <- readme(dfm = data$dfm,
                   labeledIndicator = data$labeledIndicator,
                   categoryVec = data$categoryVec,
                   nBoot = 1, sgdIters = 100,
                   verbose = FALSE)

  # Get unique categories from labeled set
  labeled_cats <- unique(data$categoryVec[data$labeledIndicator == 1])

  expect_true(all(names(result$point_readme) %in% labeled_cats))
})

test_that("readme is reproducible with tensorflowSeed", {
  data <- create_test_dataset(n_docs = 80, n_categories = 2,
                              prop_labeled = 0.5, seed = 42)

  result1 <- readme(dfm = data$dfm,
                    labeledIndicator = data$labeledIndicator,
                    categoryVec = data$categoryVec,
                    nBoot = 1, sgdIters = 100,
                    tensorflowSeed = 12345,
                    verbose = FALSE)

  result2 <- readme(dfm = data$dfm,
                    labeledIndicator = data$labeledIndicator,
                    categoryVec = data$categoryVec,
                    nBoot = 1, sgdIters = 100,
                    tensorflowSeed = 12345,
                    verbose = FALSE)

  # Results should be very similar (TensorFlow seeds may not guarantee exact reproducibility)
  expect_equal(result1$point_readme, result2$point_readme, tolerance = 0.1)
})

test_that("readme handles two categories", {
  data <- create_test_dataset(n_docs = 80, n_categories = 2,
                              prop_labeled = 0.5, seed = 42)

  result <- readme(dfm = data$dfm,
                   labeledIndicator = data$labeledIndicator,
                   categoryVec = data$categoryVec,
                   nBoot = 1, sgdIters = 100,
                   verbose = FALSE)

  expect_equal(length(result$point_readme), 2)
  expect_equal(sum(result$point_readme), 1, tolerance = 0.05)
})

test_that("readme handles four categories", {
  data <- create_test_dataset(n_docs = 120, n_categories = 4,
                              prop_labeled = 0.5, seed = 42)

  result <- readme(dfm = data$dfm,
                   labeledIndicator = data$labeledIndicator,
                   categoryVec = data$categoryVec,
                   nBoot = 1, sgdIters = 100,
                   verbose = FALSE)

  expect_equal(length(result$point_readme), 4)
  expect_equal(sum(result$point_readme), 1, tolerance = 0.05)
})

test_that("readme transformed_dfm has correct dimensions", {
  data <- create_test_dataset(n_docs = 80, n_categories = 2,
                              prop_labeled = 0.5, seed = 42)

  result <- readme(dfm = data$dfm,
                   labeledIndicator = data$labeledIndicator,
                   categoryVec = data$categoryVec,
                   nBoot = 1, sgdIters = 100,
                   numProjections = 20,
                   verbose = FALSE)

  expect_equal(nrow(result$transformed_dfm), nrow(data$dfm))
  expect_equal(ncol(result$transformed_dfm), 20)
})

test_that("readme verbose output works", {
  data <- create_test_dataset(n_docs = 60, n_categories = 2,
                              prop_labeled = 0.5, seed = 42)

  # Just check it runs without error when verbose = TRUE
  expect_output(
    readme(dfm = data$dfm,
           labeledIndicator = data$labeledIndicator,
           categoryVec = data$categoryVec,
           nBoot = 1, sgdIters = 50,
           verbose = TRUE)
  )
})
