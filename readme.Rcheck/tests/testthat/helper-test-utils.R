# Helper functions and fixtures for readme package tests

# Initialize TensorFlow environment for tests
tryCatch({
  library(reticulate)
  use_condaenv("readme_env", required = TRUE)
}, error = function(e) {
  message("Note: Could not initialize readme_env conda environment: ", e$message)
})

#' Create synthetic word vectors for testing
#' @param n_words Number of words in vocabulary
#' @param n_dims Number of dimensions per word vector
#' @param seed Random seed for reproducibility
#' @return Matrix with word vectors as rows
create_test_wordvecs <- function(n_words = 20, n_dims = 25, seed = 42) {
  set.seed(seed)
  wv <- matrix(rnorm(n_words * n_dims), ncol = n_dims)
  rownames(wv) <- c("the", "a", "is", "are", "was", "were", "have", "has",
                    "this", "that", "with", "from", "they", "we", "you",
                    "it", "be", "to", "of", "and")[1:n_words]
  colnames(wv) <- paste0("V", 1:n_dims)
  wv
}

#' Create synthetic documents for testing
#' @param n_docs Number of documents
#' @param vocab Vector of vocabulary words
#' @param words_per_doc Range of words per document
#' @param seed Random seed for reproducibility
#' @return Character vector of documents
create_test_documents <- function(n_docs = 100, vocab = NULL,
                                  words_per_doc = c(5, 15), seed = 42) {
  set.seed(seed)
  if (is.null(vocab)) {
    vocab <- c("the", "a", "is", "are", "was", "were", "have", "has",
               "this", "that", "with", "from", "they", "we", "you",
               "it", "be", "to", "of", "and")
  }
  sapply(1:n_docs, function(i) {
    n_words <- sample(words_per_doc[1]:words_per_doc[2], 1)
    paste(sample(vocab, n_words, replace = TRUE), collapse = " ")
  })
}

#' Create synthetic labeled/unlabeled split
#' @param n_docs Total number of documents
#' @param prop_labeled Proportion of labeled documents
#' @param seed Random seed for reproducibility
#' @return Binary vector (1 = labeled, 0 = unlabeled)
create_test_labels <- function(n_docs = 100, prop_labeled = 0.5, seed = 42) {
  set.seed(seed)
  n_labeled <- round(n_docs * prop_labeled)
  c(rep(1, n_labeled), rep(0, n_docs - n_labeled))
}

#' Create synthetic category vector
#' @param n_docs Total number of documents
#' @param n_categories Number of categories
#' @param labeled_indicator Binary vector indicating labeled docs
#' @param seed Random seed for reproducibility
#' @return Factor vector of categories (NA for unlabeled)
create_test_categories <- function(n_docs = 100, n_categories = 4,
                                   labeled_indicator = NULL, seed = 42) {
  set.seed(seed)
  categories <- paste0("C", 1:n_categories)
  cat_vec <- sample(categories, n_docs, replace = TRUE)
  if (!is.null(labeled_indicator)) {
    cat_vec[labeled_indicator == 0] <- NA
  }
  cat_vec
}

#' Create a complete test dataset
#' @param n_docs Total number of documents
#' @param n_categories Number of categories
#' @param prop_labeled Proportion of labeled documents
#' @param n_dims Number of word vector dimensions
#' @param seed Random seed for reproducibility
#' @return List with dfm, labeledIndicator, categoryVec, wordVecs
create_test_dataset <- function(n_docs = 100, n_categories = 4,
                                prop_labeled = 0.5, n_dims = 25, seed = 42) {
  set.seed(seed)

  # Create word vectors
  wordVecs <- create_test_wordvecs(n_words = 20, n_dims = n_dims, seed = seed)
  vocab <- rownames(wordVecs)

  # Create documents
  docs <- create_test_documents(n_docs = n_docs, vocab = vocab, seed = seed)

  # Create labels
  labeledIndicator <- create_test_labels(n_docs = n_docs,
                                         prop_labeled = prop_labeled,
                                         seed = seed)

  # Create categories
  categoryVec <- create_test_categories(n_docs = n_docs,
                                        n_categories = n_categories,
                                        labeled_indicator = labeledIndicator,
                                        seed = seed)

  # Create document-feature matrix using undergrad
  dfm <- undergrad(documentText = docs, wordVecs = wordVecs, verbose = FALSE)

  list(
    dfm = dfm,
    labeledIndicator = labeledIndicator,
    categoryVec = categoryVec,
    wordVecs = wordVecs,
    documents = docs
  )
}

#' Create a simple DTM for readme0 testing
#' @param n_docs Total number of documents
#' @param n_features Number of features
#' @param seed Random seed for reproducibility
#' @return Binary document-term matrix
create_test_dtm <- function(n_docs = 100, n_features = 50, seed = 42) {
  set.seed(seed)
  dtm <- matrix(rbinom(n_docs * n_features, 1, 0.3),
                nrow = n_docs, ncol = n_features)
  colnames(dtm) <- paste0("term", 1:n_features)
  dtm
}

#' Check if TensorFlow is available
#' @return TRUE if tensorflow is available, FALSE otherwise
tensorflow_available <- function() {
  tryCatch({
    requireNamespace("tensorflow", quietly = TRUE) &&
      !inherits(try(tensorflow::tf$`__version__`, silent = TRUE), "try-error")
  }, error = function(e) FALSE)
}
