# Tests for TensorFlow backend setup helpers

.capture_messages <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(
    force(expr),
    message = function(message) {
      messages <<- c(messages, conditionMessage(message))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, messages = messages)
}

test_that("build_backend creates conda environment and installs default TensorFlow", {
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    conda_create = function(envname, conda, python_version) {
      calls$conda_create <- list(
        envname = envname,
        conda = conda,
        python_version = python_version
      )
      invisible(envname)
    },
    py_install = function(packages, envname, conda, pip) {
      calls$py_install <- list(
        packages = packages,
        envname = envname,
        conda = conda,
        pip = pip
      )
      invisible(TRUE)
    },
    .package = "reticulate"
  )

  captured <- .capture_messages(
    build_backend(conda_env = "test_env", conda = "test_conda")
  )

  expect_equal(captured$value, "test_env")
  expect_true(any(grepl("Creating conda environment 'test_env'", captured$messages)))
  expect_true(any(grepl("Installing tensorflow", captured$messages)))
  expect_true(any(grepl("Backend environment 'test_env' is ready", captured$messages)))
  expect_equal(calls$conda_create$envname, "test_env")
  expect_equal(calls$conda_create$conda, "test_conda")
  expect_equal(calls$conda_create$python_version, "3.11")
  expect_equal(calls$py_install$packages, "tensorflow")
  expect_equal(calls$py_install$envname, "test_env")
  expect_true(calls$py_install$pip)
})

test_that("build_backend honors TensorFlow version override", {
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    conda_create = function(...) invisible(TRUE),
    py_install = function(packages, envname, conda, pip) {
      calls$packages <- packages
      calls$envname <- envname
      invisible(TRUE)
    },
    .package = "reticulate"
  )

  captured <- .capture_messages(
    build_backend(conda_env = "versioned_env", tensorflow_version = "2.13.0")
  )

  expect_true(any(grepl("tensorflow==2.13.0", captured$messages)))
  expect_equal(calls$packages, "tensorflow==2.13.0")
  expect_equal(calls$envname, "versioned_env")
})

test_that("build_backend errors clearly when reticulate is unavailable", {
  original_require_namespace <- base::requireNamespace

  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE, ...) {
      if (package == "reticulate") {
        return(FALSE)
      }
      original_require_namespace(package, quietly = quietly, ...)
    },
    .package = "base"
  )

  expect_error(build_backend(), "Package 'reticulate' is required")
})

test_that("initialize_tensorflow loads requested environment and returns version", {
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    use_condaenv = function(condaenv, required) {
      calls$use_condaenv <- list(condaenv = condaenv, required = required)
      invisible(TRUE)
    },
    import = function(name) {
      calls$import_name <- name
      list(`__version__` = "2.20.0")
    },
    .package = "reticulate"
  )

  captured <- .capture_messages(
    initialize_tensorflow(
      conda_env = "test_env",
      conda_env_required = FALSE,
      verbose = TRUE
    )
  )

  expect_equal(captured$value, "2.20.0")
  expect_true(any(grepl("TensorFlow 2.20.0 initialized from environment 'test_env'", captured$messages)))
  expect_equal(calls$use_condaenv$condaenv, "test_env")
  expect_false(calls$use_condaenv$required)
  expect_equal(calls$import_name, "tensorflow")
})

test_that("initialize_tensorflow wraps TensorFlow import errors", {
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    use_condaenv = function(condaenv, required) {
      calls$condaenv <- condaenv
      invisible(TRUE)
    },
    import = function(name) {
      stop("import failed")
    },
    .package = "reticulate"
  )

  expect_error(
    initialize_tensorflow(conda_env = "missing_tf", verbose = FALSE),
    "Failed to import TensorFlow"
  )
  expect_equal(calls$condaenv, "missing_tf")
})

test_that("initialize_tensorflow errors clearly when reticulate is unavailable", {
  original_require_namespace <- base::requireNamespace

  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE, ...) {
      if (package == "reticulate") {
        return(FALSE)
      }
      original_require_namespace(package, quietly = quietly, ...)
    },
    .package = "base"
  )

  expect_error(initialize_tensorflow(), "Package 'reticulate' is required")
})

test_that("tensorflow_available reflects import availability", {
  testthat::local_mocked_bindings(
    import = function(name, delay_load = FALSE) {
      expect_equal(name, "tensorflow")
      expect_true(delay_load)
      list(`__version__` = "2.20.0")
    },
    .package = "reticulate"
  )

  expect_true(readme::tensorflow_available())
})

test_that("tensorflow_available returns false on failed import and missing reticulate", {
  testthat::local_mocked_bindings(
    import = function(name, delay_load = FALSE) {
      stop("not available")
    },
    .package = "reticulate"
  )

  expect_false(readme::tensorflow_available())

  original_require_namespace <- base::requireNamespace
  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE, ...) {
      if (package == "reticulate") {
        return(FALSE)
      }
      original_require_namespace(package, quietly = quietly, ...)
    },
    .package = "base"
  )

  expect_false(readme::tensorflow_available())
})
