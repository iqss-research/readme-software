# Tests for download_wordvecs() function

test_that("download_wordvecs errors when file already exists", {
  # Create a temporary directory with an existing file
  temp_dir <- tempdir()
  existing_file <- file.path(temp_dir, "glove.6B.200d.zip")

  # Create the file
  file.create(existing_file)
  on.exit(unlink(existing_file), add = TRUE)

  expect_error(
    download_wordvecs(targetDir = temp_dir),
    "already exists"
  )
})

test_that("download_wordvecs uses correct default URL", {
  # We can check that the function constructs the correct path
  # without actually downloading by creating the file first

  temp_dir <- tempdir()
  test_subdir <- file.path(temp_dir, "download_test")
  dir.create(test_subdir, showWarnings = FALSE)
  on.exit(unlink(test_subdir, recursive = TRUE), add = TRUE)

  existing_file <- file.path(test_subdir, "glove.6B.200d.zip")
  file.create(existing_file)

  # Should error mentioning the correct file path
  expect_error(
    download_wordvecs(targetDir = test_subdir),
    "glove.6B.200d.zip.*already exists"
  )
})

test_that("download_wordvecs accepts custom targetDir", {
  # Test that the function accepts a custom directory
  temp_dir <- tempdir()
  custom_dir <- file.path(temp_dir, "custom_wordvecs")
  dir.create(custom_dir, showWarnings = FALSE)
  on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)

  # Create a file to trigger the "already exists" error
  # This confirms the function is looking in the custom directory
  existing_file <- file.path(custom_dir, "glove.6B.200d.zip")
  file.create(existing_file)

  expect_error(
    download_wordvecs(targetDir = custom_dir),
    "already exists"
  )
})

test_that("download_wordvecs accepts custom URL", {
  temp_dir <- tempdir()
  test_subdir <- file.path(temp_dir, "custom_url_test")
  dir.create(test_subdir, showWarnings = FALSE)
  on.exit(unlink(test_subdir, recursive = TRUE), add = TRUE)

  # Create file with custom name
  custom_filename <- "my_custom_vectors.zip"
  existing_file <- file.path(test_subdir, custom_filename)
  file.create(existing_file)

  # Custom URL should result in looking for custom filename
  expect_error(
    download_wordvecs(url = paste0("http://example.com/", custom_filename),
                      targetDir = test_subdir),
    "already exists"
  )
})

# Note: We skip actual download tests to avoid network dependencies
# and long test times. The function behavior is tested via error conditions.

test_that("download_wordvecs function exists and is exported", {
  expect_true(exists("download_wordvecs"))
  expect_type(download_wordvecs, "closure")
})

test_that("download_wordvecs has correct parameters", {
  fn_args <- names(formals(download_wordvecs))
  expect_true("url" %in% fn_args)
  expect_true("targetDir" %in% fn_args)
})
