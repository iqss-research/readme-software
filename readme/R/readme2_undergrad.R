#' undergrad
#' 
#' Preprocessing for \code{readme} function - creates a document-feature matrix 
#' (saved as a data frame in output) to be passed to \code{readme}. Users can either input word-specific vectors 
#' using the \code{wordVecs_corpus} or \code{wordVecs_corpusPointer} parameters. Primarily intended for users 
#' wanting control over the pre-processing protocol. 
#'
#' @param documentText A vector in which each entry corresponds to a ``clean'' document. 
#' Note that the function will take as a ``word'' all whitespace-separated elements in each vector entry. For example,
#' \code{"star."} would have to have an exact analogue in the vector corpus, otherwise
#' it will be dropped in the calculations. It will be more common to space separate punctuation marks (i.e. 
#' \code{"star."} would become \code{"star ."}), since punctuation marks often have their own entries in the vector database. 
#' 
#' @param wordVecs A matrix where each row denotes a word and each column a word vector. Words should be stored as the rownames of the matrix.
#' 
#' @param word_quantiles A numeric vector denoting the quantiles (0-1) used to summarize each word vector dimension. Defaults to 10th, 50th and 90th quantiles.
#' 
#' @param replace_missing If TRUE, attempts to match terms missing from the wordVec corpus with alternate representations.
#' 
#' @param unique_terms If TRUE, removes duplicate terms from each document - each document is represented only by the presence or absence of a term.
#' 
#' @param verbose If TRUE, prints updates as function runs
#' 
#' @return A data.frame consisting of the \code{word_quantiles} quantiles of the word vectors by document.
#'  Each row corresonds to a document, and the columns to a particular summary of a particular word vector dimension. 
#' 
#' @examples 
#' #set seed 
#' set.seed(1) 
#' 
#' #Generate synthetic word vector corpus. 
#' my_wordVecs_corpus <- data.frame(matrix(rnorm(11*50), ncol = 50))
#' my_wordVecs_corpus <- cbind(c("the","true", "thine", "stars", "are" ,
#'                               "fire", ".", "to", "own", "self", "be"), 
#'                           my_wordVecs_corpus)
#' my_wordVecs_corpus <- data.table::as.data.table(my_wordVecs_corpus)
#' 
#' #Setup ``documents'' 
#' my_documents <- c(
#' "the stars are fire .", #document 1 
#' "to thine own self be true ", #document 2 
#' "true stars be true ." #document 3
#' )
#' 
#' #Get document-level word vector summaries. 
#' my_dfm <- undergrad(documentText = my_documents, wordVecs_corpus = my_wordVecs_corpus)
#' print( my_dfm ) 
#' 
#' @export 
#' 
#' @import tokenizers

undergrad <- function(documentText, wordVecs = NULL, word_quantiles = c(.1, .5, .9), replace_missing = T, unique_terms = T, verbose=T){ 
   
    if(is.null(wordVecs)){ 
     stop("NOTE: No word vector matrix specified in 'wordVecs' -  Stoping undergrad.\n
          In order to use the word vector summaries, please provide a data frame containing the word vectors.\n
          We recommend using a GloVe corpus from https://nlp.stanford.edu/projects/glove/\n")
    }
    ## Sanity check the wordVecs
    if (!is.matrix(wordVecs)){
      stop("Error: 'wordVecs' is not a matrix")
    }
  

  
    ## Tokenize the documents using whitespace splits
    tokenized_docs <- tokenize_regex(documentText)
      
    ### Consider only unique terms
    if (unique_terms == T){
      tokenized_docs <- lapply(tokenized_docs), function(x) return(  unique(x[x!=""])) )
    }
    
    ## Drop any documents that have zero terms
    num_terms <- sapply(tokenized_docs, function(x) length(x))
    if (length(which(num_terms == 0)) > 0){
      cat(paste("WARNING: Document ", which(num_terms == 0), " has no terms, dropping from analysis...\n", collapse = "", sep = ""))
    }

    ### wordVec terms
    wordVec_terms <- rownames(wordVecs)
    
    ### Number of unique stems
    unique_stems <- unique(unlist(tokenized_docs))
    
    if(verbose == T){
      cat(paste("Processing...\n"))
      cat(paste("Number of documents: ", length(documentText), "\n", sep=""))
      cat(paste("Number of unique word stems: ", length(unique_stems), "\n", sep=""))
      cat(paste("Number of word vector terms: ", nrow(wordVecs), "\n", sep=""))
    }
    
    ### How many of the unique stems match the word vector matrix
    unique_stem_match <- match(unique_stems, wordVec_terms)
    names(unique_stem_match) <- unique_stems
    ### 
    if (verbose == T){
      cat(paste("First attempt: Matched ", sum(!is.na(unique_stem_match)), " of ",  length(unique_stems), " ", "(",
                round(sum(!is.na(unique_stem_match))/length(unique_stems), 3)*100, "%) ", "terms to word vectors\n", sep=""))
    }
    
    ### If user wants to retry to match some of the missing terms
    if (replace_missing == T){
      
      ### Which terms didn't get a match
      missing_stems <- unique_stems[is.na(unique_stem_match)]
      
      ### Missing matches
      match_missing <- rep(NA, length(missing_stems))
      
      ## Attempt 1:
      # Did some of the terms have hashtags, drop them
      match_missing[is.na(match_missing)] <- match(gsub(missing_stems[is.na(match_missing)], pattern = "\\#", replace =""), wordVec_terms)
      
      # Attempt 2:
      # Are they just hashtags?
      match_missing[is.na(match_missing)] <- match(gsub(missing_stems[is.na(match_missing)], pattern = "#\\S+", replace = "<hashtag>"), wordVec_terms)
      
      # Attempt 3:
      # Not sure what this one's doing
      match_missing[is.na(match_missing)] <- match(gsub(missing_stems[is.na(match_missing)],  pattern = '(\\w)\\1{2, }', replace =  '\\1'), wordVec_terms)
      
      # Attempt 4:
      # Remaining punctuation?
      match_missing[is.na(match_missing)] <- match(gsub(missing_stems[is.na(match_missing)],  pattern = '[[:punct:]]+',replace =  ''), wordVec_terms)
      
      # Attempt 5:
      # Drop ending "ing"
      match_missing[is.na(match_missing)] <- match(gsub(missing_stems[is.na(match_missing)], pattern = 'ing\\b',replace =  ''), wordVec_terms)
      
      # Attempt 6:
      # Drop ending "ies" -> "y"
      match_missing[is.na(match_missing)]  <- match(gsub(missing_stems[is.na(match_missing)], pattern = 'ies\\b',replace =  'y'), wordVec_terms)
      
      # Attempt 7:
      # Drop plural ending?
      match_missing[is.na(match_missing)]  <- match(gsub(missing_stems[is.na(match_missing)],  pattern = 's\\b',replace =  ''), wordVec_terms) 
      
      # Attempt 8:
      # Drop past tense ending?
      match_missing[is.na(match_missing)]  <- match(gsub(missing_stems[is.na(match_missing)], pattern = 'ed\\b',replace =  ''), wordVec_terms) 
      
      ## Save the substituted matches
      unique_stem_match[missing_stems] <- match_missing
    }
    
    if (verbose == T){
      cat(paste("Second attempt: Matched ", sum(!is.na(unique_stem_match)), " of ",  length(unique_stems), " ", "(",
                round(sum(!is.na(unique_stem_match))/length(unique_stems), 3)*100, "%) ", "terms to word vectors\n", sep=""))
    }
    
    ### Map each document to the relevant row of the wordVecs matrix
    matched_terms <- lapply(tokenized_docs, function(x) unique_stem_match[x])
    
    ### Check if any documents had zero matches
    num_terms_matched <- sapply(matched_terms, function(x) sum(!is.na(x)))
    
    ### Drop missing word vectors
    matched_terms_noNA <- lapply(matched_terms, function(x) x[!is.na(x)])
    
    ### Document-vector matrices
    document_matrices <- lapply(matched_terms_noNA, function(x) as.matrix(wordVecs[x,]))
    
    if (verbose == T){
      cat("Computing word vector summaries for each document...\n")
    }

    ### For each summary quantile, calculate the summary
    document_summaries <- list()
    for (indic in 1:length(word_quantiles)){
      
      if (verbose == T){
        cat(paste("Summarizing document word vectors: ", 100*word_quantiles[indic], "%",  " quantile\n", sep=""))
      }
      
      quant <- word_quantiles[indic]
      document_summary <- do.call(rbind, sapply(document_matrices, function(x) apply(x, 2, function(z) quantile(z, quant))))
      colnames(document_summary) <- paste(colnames(document_summary), "-", 100*quant,"%", sep="")
      document_summaries[[indic]] <- document_summary
    }
    
    ### Merge all summaries into a document term matrix
    dfm <- do.call(cbind, document_summaries)
     
    ### Are any columns zero-variance?
    column_sds <- apply(dfm, 2, sd)
    
    ###
    if (length(which(column_sds == 0)) > 0){
      
      if (verbose == T){
        cat(paste("WARNING: Feature matrix ", which(column_sds == 0), " has zero variance, dropping from analysis...\n", collapse = "", sep = ""))
      }
      
    }
    
    
    
    return(dfm[,column_sds != 0])
}
