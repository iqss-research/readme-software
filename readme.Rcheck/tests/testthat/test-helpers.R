# Tests for helper functions (internal utilities)

# Note: These functions are internal but critical to the package functionality

test_that("vec2prob returns valid probabilities", {
  # Access the internal function
  vec2prob <- readme:::vec2prob

  # Basic case
  input <- c("A", "A", "B", "B", "B", "C")
  result <- vec2prob(input)

  expect_s3_class(result, "table")
  expect_equal(sum(result), 1)
  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
  expect_equal(as.numeric(result["A"]), 2/6)
  expect_equal(as.numeric(result["B"]), 3/6)
  expect_equal(as.numeric(result["C"]), 1/6)
})

test_that("vec2prob handles single category", {
  vec2prob <- readme:::vec2prob

  input <- c("A", "A", "A")
  result <- vec2prob(input)

  expect_equal(sum(result), 1)
  expect_equal(as.numeric(result["A"]), 1)
})

test_that("vec2prob handles factors", {
  vec2prob <- readme:::vec2prob

  input <- factor(c("cat1", "cat2", "cat1", "cat3"))
  result <- vec2prob(input)

  expect_equal(sum(result), 1)
  expect_equal(length(result), 3)
})

test_that("Winsorize_values returns correct bounds", {
  Winsorize_values <- readme:::Winsorize_values

  # Simple case with known outliers
  set.seed(42)
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100)  # 100 is an outlier

  result <- Winsorize_values(x)

  expect_equal(length(result), 2)
  expect_true(result[1] < result[2])  # Lower bound < upper bound
  # The outlier 100 should be outside the upper bound
  expect_true(100 > result[2])
})

test_that("Winsorize_values handles uniform data", {
  Winsorize_values <- readme:::Winsorize_values

  x <- rep(5, 10)
  result <- Winsorize_values(x)

  expect_equal(length(result), 2)
  # For uniform data, Q1 = Q3 = 5, so IQR = 0, bounds are both 5
  expect_equal(as.numeric(result[1]), as.numeric(result[2]))
})

test_that("f2n converts factors and characters to numeric", {
  f2n <- readme:::f2n

  # Factor input
  input_factor <- factor(c("1.5", "2.5", "3.5"))
  result_factor <- f2n(input_factor)
  expect_equal(result_factor, c(1.5, 2.5, 3.5))
  expect_type(result_factor, "double")

  # Character input
  input_char <- c("10", "20", "30")
  result_char <- f2n(input_char)
  expect_equal(result_char, c(10, 20, 30))
  expect_type(result_char, "double")

  # Numeric input (should pass through)
  input_num <- c(1, 2, 3)
  result_num <- f2n(input_num)
  expect_equal(result_num, c(1, 2, 3))
})

test_that("colSds calculates column standard deviations", {
  colSds <- readme:::colSds

  # Create a matrix with known properties
  set.seed(42)
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  center <- colMeans(mat)

  result <- colSds(mat, center = center)

  expect_equal(length(result), 10)
  expect_true(all(result > 0))

  # Compare to base R sd for each column
  expected <- apply(mat, 2, sd)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("FastScale standardizes columns correctly", {
  FastScale <- readme:::FastScale

  set.seed(42)
  mat <- matrix(rnorm(100, mean = 10, sd = 5), nrow = 20, ncol = 5)

  result <- FastScale(mat)

  # Check that columns have mean ~0 and sd ~1
  col_means <- colMeans(result)
  col_sds <- apply(result, 2, sd)

  expect_equal(col_means, rep(0, 5), tolerance = 1e-10)
  expect_equal(col_sds, rep(1, 5), tolerance = 1e-10)
})

test_that("FastScale accepts pre-computed center and scale", {
  FastScale <- readme:::FastScale

  set.seed(42)
  mat <- matrix(rnorm(100), nrow = 20, ncol = 5)

  # Custom center and scale
  custom_center <- rep(0, 5)
  custom_sd <- rep(2, 5)

  result <- FastScale(mat, cm = custom_center, csd = custom_sd)

  # Result should be mat / 2
  expected <- mat / 2
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("readme_est_fxn returns valid proportions", {
  readme_est_fxn <- readme:::readme_est_fxn

  # Create a simple linear system
  # X is a 3x2 matrix (3 features, 2 categories)
  X <- matrix(c(0.6, 0.3, 0.1,
                0.2, 0.5, 0.3), nrow = 3, ncol = 2)
  # Y is the observed feature proportions
  Y <- c(0.4, 0.4, 0.2)

  result <- readme_est_fxn(X, Y)

  # Result should sum to 1
  expect_equal(sum(result), 1, tolerance = 1e-6)

  # All proportions should be non-negative
  expect_true(all(result >= -1e-10))  # Small tolerance for numerical precision
})

test_that("readme_est_fxn handles edge cases", {
  readme_est_fxn <- readme:::readme_est_fxn

  # Two category case
  X <- matrix(c(0.7, 0.3,
                0.3, 0.7), nrow = 2, ncol = 2)
  Y <- c(0.5, 0.5)

  result <- readme_est_fxn(X, Y)

  expect_equal(sum(result), 1, tolerance = 1e-6)
  expect_equal(length(result), 2)
})
