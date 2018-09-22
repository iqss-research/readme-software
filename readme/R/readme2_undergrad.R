#' undergrad
#' 
#' Preprocessing for \code{readme} function - creates a document-feature matrix 
#' (saved as a data frame in output) to be passed to \code{readme}. Users can either input word-specific vectors 
#' using the \code{wordVecs_corpus} or \code{wordVecs_corpusPointer} parameters. Primarily intended for users 
#' wanting control over the pre-processing protocol. 
#'
#' @param documentText A vector in which each entry corresponds to a ``clean'' document. 
#' Note that the function will take as a ``word'' all space-separated elements in each vector entry. For example,
#' \code{"star."} would have to have an exact analogue in the vector corpus, otherwise
#' it will be dropped in the calculations. It will be more common to space separate punctuation marks (i.e. 
#' \code{"star."} would become \code{"star ."}), since punctuation marks often have their own entries in the vector database. 
#' 
#' @param wordVecs_corpus A data.table object in which the first column holds the text of each word, 
#' and in which the remaining columns contain the numerical representation. Either \code{wordVecs_corpus} or 
#' \code{wordVecs_corpusPointer} should be null. If \code{wordVecs_corpus} and \code{wordVecs_corpusPointer} are \code{NULL}, 
#' \code{undergrade} will download and use the \code{GloVe} 50-dimensional embeddings trained on Wikipedia. 
#' 
#' @param wordVecs_corpusPointer A character string denoting where to find the \code{wordVecs_corpus} for loading into memory as a 
#' data.table. If \code{wordVecs_corpus} and \code{wordVecs_corpusPointer} are \code{NULL}, 
#' \code{undergrade} will download and use the \code{GloVe} 50-dimensional embeddings trained on Wikipedia. 
#' 
#' @return A data.frame consisting of the \code{10th}, \code{50th}, and \code{90th} quantiles of the word vectors by document.
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

undergrad <- function(documentText, wordVecs_corpus = NULL, word_quantiles = c(.1, .5, .9), replace_missing = T, unique_terms = T){ 
   
   if(is.null(wordVecs_corpus)){ 
     cat("NOTE: No word vector corpus specified in 'wordVecs_corpus' - Returning a regular document-term matrix.\n")
     cat("In order to use the word vector summaries, please provide a data frame containing the word vectors.\n")
     cat("We recommend using a GloVe corpus from https://nlp.stanford.edu/projects/glove/\n")
   }
  
   #if(is.null(wordVecs_corpus)){ 
    #  print("Downloading GloVe corpus trained on Wikipedia...(large file!)")
    #  download.file("nlp.stanford.edu/data/glove.6B.zip", destfile = "./GLOVE_TRAINED_ON_WIKIPEDIA.zip")
    #  unzip("./GLOVE_TRAINED_ON_WIKIPEDIA.zip", files = 'glove.6B.50d.txt')
    #  try(file.remove("./GLOVE_TRAINED_ON_WIKIPEDIA.zip"), T)  
    #  wordVecs_corpus <- data.table::fread("glove.6B.50d.txt")
    #  try(file.remove("./glove.6B.50d.txt"), T)  
    #} 
    
    #### NOTE: CLEAN THIS PART UP!
  
    ## Tokenize the documents
    documentText_orig <- documentText
    tokenized_docs <- tokenize_words(documentText, strip_punct=F)
      
    ### Consider only unique terms
    if (unique_terms == T){
      tokenized_docs <- lapply(tokenized_docs), function(x) return(  unique(x[x!=""])) )
    }
    
    ### Match each to a word-vector 
    
    
    { 
      len_to_index_FXN <- function(len_vec){ 
        cumsum_vec <- cumsum(len_vec)
        return(  sapply(1:length(len_vec),function(xs){ 
          starting_v <- 1+cumsum_vec[xs-1]; if(length(starting_v) == 0){starting_v <- 1}
          ending_v <- cumsum_vec[xs];
          return(  list((starting_v):ending_v  )   ) 
      } ) )  }  
      indices_list <- len_to_index_FXN(unlist( lapply(documentText, length) ) )
      
      documentText_unlisted_new <- documentText_unlisted_orig <- unlist(documentText)
      bad_indicator <- !documentText_unlisted_new %in% wordVecs_corpus[[1]]
      
      if(sum(bad_indicator) > 0){ 
        documentText_unlisted_new[bad_indicator] <- gsub(documentText_unlisted_new[bad_indicator], pattern = "\\#", replace ="")
        TEMP_SPLIT = strsplit(documentText_unlisted_new[bad_indicator], split = " ")
        bad_indicator_new <- !unlist(TEMP_SPLIT) %in% wordVecs_corpus[[1]]
        bad_indicator_new <- unlist(  lapply(len_to_index_FXN(lapply(TEMP_SPLIT, length)), function(ta){ all(bad_indicator_new[ta]) } ) ) 
        documentText_unlisted_new[bad_indicator][bad_indicator_new] <- documentText_unlisted_orig[bad_indicator][bad_indicator_new]
        bad_indicator[bad_indicator][!bad_indicator_new] <- F
      } 
    
      if(sum(bad_indicator) > 0){ 
        documentText_unlisted_new[bad_indicator] <- gsub(documentText_unlisted_new[bad_indicator], pattern = "#\\S+", replace = " <hashtag> ")
        TEMP_SPLIT = strsplit(documentText_unlisted_new[bad_indicator], split = " ")
        bad_indicator_new <- !unlist(TEMP_SPLIT) %in% wordVecs_corpus[[1]]
        bad_indicator_new <- unlist(  lapply(len_to_index_FXN(lapply(TEMP_SPLIT, length)), function(ta){ all(bad_indicator_new[ta]) } ) ) 
        documentText_unlisted_new[bad_indicator][bad_indicator_new] <- documentText_unlisted_orig[bad_indicator][bad_indicator_new]
        bad_indicator[bad_indicator][!bad_indicator_new] <- F
      } 
      
      if(sum(bad_indicator) > 0){ 
        documentText_unlisted_new[bad_indicator] <- gsub(documentText_unlisted_new[bad_indicator], pattern = '(\\w)\\1{2, }', replace =  '\\1')
        TEMP_SPLIT = strsplit(documentText_unlisted_new[bad_indicator], split = " ")
        bad_indicator_new <- !unlist(TEMP_SPLIT) %in% wordVecs_corpus[[1]]
        bad_indicator_new <- unlist(  lapply(len_to_index_FXN(lapply(TEMP_SPLIT, length)), function(ta){ all(bad_indicator_new[ta]) } ) ) 
        documentText_unlisted_new[bad_indicator][bad_indicator_new] <- documentText_unlisted_orig[bad_indicator][bad_indicator_new]
        bad_indicator[bad_indicator][!bad_indicator_new] <- F
      } 
      
      if(sum(bad_indicator) > 0){ 
        documentText_unlisted_new[bad_indicator] <- gsub(documentText_unlisted_new[bad_indicator], pattern = '[[:punct:]]+',replace =  ' ')
        TEMP_SPLIT = strsplit(documentText_unlisted_new[bad_indicator], split = " ")
        bad_indicator_new <- !unlist(TEMP_SPLIT) %in% wordVecs_corpus[[1]]
        bad_indicator_new <- unlist(  lapply(len_to_index_FXN(lapply(TEMP_SPLIT, length)), function(ta){ all(bad_indicator_new[ta]) } ) ) 
        documentText_unlisted_new[bad_indicator][bad_indicator_new] <- documentText_unlisted_orig[bad_indicator][bad_indicator_new]
        bad_indicator[bad_indicator][!bad_indicator_new] <- F
      } 
      
      if(sum(bad_indicator) > 0){  
        documentText_unlisted_new[bad_indicator] <- gsub(documentText_unlisted_new[bad_indicator], pattern = 'ing\\b',replace =  '')
        TEMP_SPLIT = strsplit(documentText_unlisted_new[bad_indicator], split = " ")
        TEMP_SPLIT_INDICES = len_to_index_FXN(lapply(TEMP_SPLIT, length))
        bad_indicator_new <- !unlist(TEMP_SPLIT) %in% wordVecs_corpus[[1]]
        bad_indicator_new <- unlist(  lapply(TEMP_SPLIT_INDICES, function(ta){ all(bad_indicator_new[ta]) } ) ) 
        documentText_unlisted_new[bad_indicator][bad_indicator_new] <- documentText_unlisted_orig[bad_indicator][bad_indicator_new]
        bad_indicator[bad_indicator][!bad_indicator_new] <- F
      } 
      
      if(sum(bad_indicator) > 0){ 
        documentText_unlisted_new[bad_indicator] <- gsub(documentText_unlisted_new[bad_indicator], pattern = 'ies\\b',replace =  'y')
        TEMP_SPLIT = strsplit(documentText_unlisted_new[bad_indicator], split = " ")
        TEMP_SPLIT_INDICES = len_to_index_FXN(lapply(TEMP_SPLIT, length))
        bad_indicator_new <- !unlist(TEMP_SPLIT) %in% wordVecs_corpus[[1]]
        bad_indicator_new <- unlist(  lapply(TEMP_SPLIT_INDICES, function(ta){ all(bad_indicator_new[ta]) } ) ) 
        documentText_unlisted_new[bad_indicator][bad_indicator_new] <- documentText_unlisted_orig[bad_indicator][bad_indicator_new]
        bad_indicator[bad_indicator][!bad_indicator_new] <- F
      } 
      
      if(sum(bad_indicator) > 0){ 
        documentText_unlisted_new[bad_indicator] <- gsub(documentText_unlisted_new[bad_indicator], pattern = 's\\b',replace =  '')
        TEMP_SPLIT = strsplit(documentText_unlisted_new[bad_indicator], split = " ")
        TEMP_SPLIT_INDICES = len_to_index_FXN(lapply(TEMP_SPLIT, length))
        bad_indicator_new <- !unlist(TEMP_SPLIT) %in% wordVecs_corpus[[1]]
        bad_indicator_new <- unlist(  lapply(TEMP_SPLIT_INDICES, function(ta){ all(bad_indicator_new[ta]) } ) ) 
        documentText_unlisted_new[bad_indicator][bad_indicator_new] <- documentText_unlisted_orig[bad_indicator][bad_indicator_new]
        bad_indicator[bad_indicator][!bad_indicator_new] <- F
      } 
      
      if(sum(bad_indicator) > 0){ 
        documentText_unlisted_new[bad_indicator] <- gsub(documentText_unlisted_new[bad_indicator], pattern = 'ed\\b',replace =  '')
        TEMP_SPLIT = strsplit(documentText_unlisted_new[bad_indicator], split = " ")
        TEMP_SPLIT_INDICES = len_to_index_FXN(lapply(TEMP_SPLIT, length))
        bad_indicator_new <- !unlist(TEMP_SPLIT) %in% wordVecs_corpus[[1]]
        bad_indicator_new <- unlist(  lapply(TEMP_SPLIT_INDICES, function(ta){ all(bad_indicator_new[ta]) } ) ) 
        documentText_unlisted_new[bad_indicator][bad_indicator_new] <- documentText_unlisted_orig[bad_indicator][bad_indicator_new]
        bad_indicator[bad_indicator][!bad_indicator_new] <- F
      } 
      
      documentText <- lapply(indices_list, function(index_i){  X___ <- unique(   unlist( strsplit(documentText_unlisted_new[index_i], split = " ")  )  ); X___[X___!=""] } ) 
    } 
    
    wordsUsed_inCorpus_unique <- intersect( unique(  unlist(documentText) ), wordVecs_corpus[[1]] )
    wordVecs_corpus <- wordVecs_corpus[which(wordVecs_corpus[[1]] %in% wordsUsed_inCorpus_unique),]
    wordsUsed_inCorpus_unique_pointers <- 1:nrow(wordVecs_corpus)
    names(wordsUsed_inCorpus_unique_pointers) <- wordVecs_corpus[[1]]
    
    #return diagnostics to user 
    percent_dropped_overall <- round(100*mean( !unlist(documentText) %in% names(wordsUsed_inCorpus_unique_pointers) ),2 )
    if(percent_dropped_overall > 0){ 
      print(sprintf("Warning: %s percent of text is dropped due to word absence in vector corpus", percent_dropped_overall   )  )
      print(head( sort( table( unlist(documentText)[!unlist(documentText) %in% names(wordsUsed_inCorpus_unique_pointers)] )  , decreasing = T), 10)  )
    } 
    
    temp <- lapply(documentText,function(x){ x_ <- (wordsUsed_inCorpus_unique_pointers[ x ] );if(is.na(sum(x_))){ x_ <- na.omit(x_) }; return(unname(x_) )  } )
    wordVecs_corpus_red <- as.matrix(  wordVecs_corpus[,-1] )
    
    docSummaries <- try(sapply(1:length(temp), function(qwer){ #
      doc_indices <- temp[[qwer]]
      if(length(doc_indices) > 0){ 
          doc_values <- wordVecs_corpus_red[doc_indices,]
          if(class(doc_values)!="matrix"){doc_values <- t(doc_values)}
          DocSummary <- c(apply(doc_values, 2, function(x){c(quantile(x,probs = word_quantiles,names = F, type = 5))}))
      }
      else{ DocSummary <- NA }
      return(  list( DocSummary )    )
      }), T)
    targetLen <- as.numeric(  names(sort( table( unlist( lapply(docSummaries, length) )   ), decreasing = T) )[1] )
    docSummaries <- lapply(docSummaries, function(summi){ if(length(summi) != targetLen){summi <- rep(NA, times = targetLen)};return( summi ) })
    docSummaries <- do.call(rbind, docSummaries); docSummaries[is.na(docSummaries)] <- NA
    colnames(docSummaries) <- paste("V", 1:ncol(docSummaries), sep = "") 
    docSummaries <- apply(docSummaries, 2, function(x){ x[is.na(x)] <- median(x, na.rm = T); return(x) })

    return( docSummaries[,colSds(docSummaries, colMeans(docSummaries))>0] )
}
