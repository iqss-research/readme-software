#' Clinton Email Dataset
#'
#' A dataset of email documents from the Clinton email corpus, with category labels
#' for a subset of documents. This dataset is used for demonstrating text
#' quantification methods.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{ROWID}{Numeric. Row identifier.}
#'   \item{TEXT}{Character. The raw email text content.}
#'   \item{TRAININGSET}{Numeric. Indicator for whether document is labeled (1) or unlabeled (0).
#'     Labeled documents are in rows 1-427, unlabeled documents start at row 428.}
#'   \item{TRUTH}{Character. The true category label for each document.}
#' }
#'
#' @source Derived from the Clinton email corpus for text quantification research.
#'
#' @examples
#' data(clinton)
#' head(clinton)
#'
#' # See structure
#' str(clinton)
#'
#' # Count labeled vs unlabeled
#' table(clinton$TRAININGSET)
#'
"clinton"
