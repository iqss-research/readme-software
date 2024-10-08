#' A algorithm for quantification that harnesses the Law of Total Expectations in an optimal feature space
#'
#'
#'
#' @description An R package for estimating category proportions in an unlabeled set of documents given a labeled set, by implementing the method described in Jerzak, King, and Strezhnev (2018, copy at \url{http://GaryKing.org/words}). This method is meant to improve on the ideas in Hopkins and King (2010), which introduced a quantification algorithm that harnesses the Law of Total Expectation. We apply this law in a feature space we craft minimizes the error of the resulting estimate. Automatic differentiation, stochastic gradient descent, and batch re-normalization are used to carry out the optimization. Other pre-processing functions are available, as well as an interface to the earlier version of the algorithm for comparison. The package also provides users with the ability to extract the generated features for other tasks.
#'
#'
#' The package provides two main functions: \code{undergrad} and \code{readme}.
#'
#'\itemize{
#'   \item \code{undergrad} takes as an input a word vector corpus (or pointer to such a corpus) and
#'   a vector housing cleaned text for cross-referencing with the vector corpus. It returns document-level
#'   summaries of each of the dimensions of the word vectors (\code{10th}, \code{50th}, and \code{90th} quantiles of each dimension within each document are calculated).
#'   Options also exist for generating a document-term matrix from the text. Useful for those wanting control over the linkup between
#'   documents and word vector corpus.
#'
#'   \item \code{readme} takes as an input raw text (or optionally, the output from \code{undergrad}).
#'   It also takes as an input an indicator vector denoting which documents are labeled and
#'   a vector indicating category membership (\code{NA}s for unlabeled documents).
#'   The algorithm then generates an optimal projection for harnessing the Law of Total Expectation in calculating the
#'   estimated category proportions in the unlabeled set.
#' }
#'
#' @section Usage:
#' For advice on usage, see \strong{Examples}. Many users will just interface with the \code{readme} function, as this
#' approach takes care of much of the pre-processing in an automatic fashion. Some users may want more control
#' over the linkup between the word vector corpus and the raw text; in that case, combining \code{undergrad} with \code{readme}
#' is a good option.
#'
#' For bug reports or support, please contact \code{<connor.jerzak@gmail.com>}.
#'
#' @section Authors:
#' \itemize{
#'   \item Connor Jerzak, Anton Strezhnev, and Gary King.
#'   \item Maintainer: Connor Jerzak <cjerzak@gmail.com>
#' }
#'
#' @section References:
#' \itemize{
#' \item Hopkins, Daniel, and King, Gary (2010),
#' A Method of Automated Nonparametric Content Analysis for Social Science,
#' \emph{American Journal of Political Science}, Vol. 54, No. 1, January 2010, p. 229-247.
#'
#' \item Jerzak, Connor, King, Gary, and Strezhnev, Anton. (2023),
#' An Improved Method of Automated Nonparametric Content Analysis for Social Science,
#' \emph{Political Analysis}, Vol. 31, No. 1, p. 42-58.
#' \url{https://doi.org/10.1017/pan.2021.36}
#' }
#'
#' @examples
#' #set seed
#' set.seed(1)
#'
#' #Generate synthetic 25-d word vector corpus.
#' my_wordVecs <- matrix(rnorm(11*25), ncol = 25)
#' row.names(my_wordVecs) <- c("the","true", "thine", "stars", "are" , "fire", ".", "to", "own", "self", "be")
#'
#' #Generate 100 ``documents'' of 5-10 words each.
#' my_documentText <- replicate(100,
#'                              paste(sample(row.names(my_wordVecs),
#'                                           sample(5:10, 1),
#'                                           replace = T),
#'                                    collapse = " ") )
#'
#' #Assign labeled/unlabeled sets. The first 50 will be labeled; the rest unlabeled.
#' my_labeledIndicator <- rep(1, times = 100)
#' my_labeledIndicator[51:100] <- 0
#'
#' #Assign category membership randomly
#' my_categoryVec <- sample(c("C1", "C2", "C3", "C4"), 100, replace = T)
#' true_unlabeled_pd <- prop.table(table(my_categoryVec[my_labeledIndicator==0]))
#' my_categoryVec[my_labeledIndicator == 0] <- NA
#'
#' #Get word vector summaries
#' my_dfm <- undergrad(documentText = my_documentText, wordVecs = my_wordVecs)
#'
#' #perform estimation
#' readme_results <- readme(dfm = my_dfm,
#'                          labeledIndicator = my_labeledIndicator,
#'                          categoryVec = my_categoryVec,
#'                          nBoot = 2)
#'print(readme_results$point_readme)
#'
#' @docType package
#' @name readme-package
NULL
