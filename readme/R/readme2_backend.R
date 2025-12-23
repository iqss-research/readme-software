#' Build TensorFlow Backend
#'
#' Creates a conda environment with TensorFlow installed for use with readme.
#' This function sets up the Python backend required for the readme() function.
#'
#' @param conda_env Name of the conda environment to create (default: "readme_env")
#' @param conda Path to conda executable (default: "auto" to auto-detect)
#' @param python_version Python version to install (default: "3.11")
#' @param tensorflow_version TensorFlow version to install (default: NULL for latest)
#'
#' @return Invisibly returns the conda environment name
#' @export
#'
#' @examples
#' \dontrun{
#' # Build the default environment
#' build_backend()
#'
#' # Build with a custom environment name
#' build_backend(conda_env = "my_readme_env")
#' }
build_backend <- function(conda_env = "readme_env",
                          conda = "auto",
                          python_version = "3.11",
                          tensorflow_version = NULL) {

  # Check if reticulate is available
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install it with: install.packages('reticulate')")
  }

  message(sprintf("Creating conda environment '%s' with Python %s...", conda_env, python_version))

  # Create conda environment
  reticulate::conda_create(
    envname = conda_env,
    conda = conda,
    python_version = python_version
  )

  # Determine TensorFlow package specification
  tf_package <- if (is.null(tensorflow_version)) {
    "tensorflow"
  } else {
    sprintf("tensorflow==%s", tensorflow_version)
  }

  message(sprintf("Installing %s...", tf_package))

  # Install TensorFlow
  reticulate::py_install(
    packages = tf_package,
    envname = conda_env,
    conda = conda,
    pip = TRUE
  )

  message(sprintf("Backend environment '%s' is ready with TensorFlow.", conda_env))
  invisible(conda_env)
}

#' Initialize TensorFlow Backend
#'
#' Loads the conda environment and initializes TensorFlow for use with readme.
#' This function should be called before using the readme() function.
#'
#' @param conda_env Name of the conda environment (default: "readme_env")
#' @param conda_env_required Whether the conda environment is required (default: TRUE).
#'   If TRUE and the environment doesn't exist, an error is raised.
#' @param verbose Whether to print status messages (default: TRUE)
#'
#' @return Invisibly returns the TensorFlow version string
#' @export
#'
#' @examples
#' \dontrun{
#' # Initialize the default environment
#' initialize_tensorflow()
#'
#' # Initialize a custom environment
#' initialize_tensorflow(conda_env = "my_readme_env")
#' }
initialize_tensorflow <- function(conda_env = "readme_env",
                                   conda_env_required = TRUE,
                                   verbose = TRUE) {

  # Check if reticulate is available
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install it with: install.packages('reticulate')")
  }

  # Load the conda environment
  reticulate::use_condaenv(condaenv = conda_env, required = conda_env_required)

  # Import TensorFlow
  tf <- tryCatch({
    reticulate::import("tensorflow")
  }, error = function(e) {
    stop(sprintf(
      "Failed to import TensorFlow. Run build_backend() first to set up the environment.\nError: %s",
      e$message
    ))
  })

  tf_version <- tf$`__version__`

  if (verbose) {
    message(sprintf("TensorFlow %s initialized from environment '%s'.", tf_version, conda_env))
  }

  invisible(tf_version)
}

#' Check TensorFlow Availability
#'
#' Checks whether TensorFlow is available in the current Python environment.
#'
#' @return Logical indicating whether TensorFlow is available
#' @export
#'
#' @examples
#' \dontrun{
#' if (tensorflow_available()) {
#'   result <- readme(dfm, labeledIndicator, categoryVec)
#' }
#' }
tensorflow_available <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }

  tryCatch({
    tf <- reticulate::import("tensorflow", delay_load = TRUE)
    TRUE
  }, error = function(e) {
    FALSE
  })
}
