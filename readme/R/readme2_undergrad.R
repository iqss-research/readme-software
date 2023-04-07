#' undergrad
#'
#' Preprocessing for \code{readme} function - creates a document-feature matrix
#' (saved as a data frame in output) to be passed to \code{readme}. Users can either input word-specific vectors
#' using the \code{wordVecs_corpus} or \code{wordVecs_corpusPointer} parameters. Primarily intended for users
#' wanting control over the pre-processing protocol. \code{numericization_method} controls whether word vector summaries
#' (the default, \code{numericization_method = "vector_based"}) or transformer-based document features (if using \code{"transformer_based"}).
#'
#' @param documentText A vector in which each entry corresponds to a ``clean'' document.
#' Note that the function will take as a ``word'' all whitespace-separated elements in each vector entry. For example,
#' \code{"star."} would have to have an exact analogue in the vector corpus, otherwise
#' it will be dropped in the calculations.
#'
#' @param wordVecs A matrix where each row denotes a word and each column a word vector. Words should be stored as the rownames of the matrix.
#'
#' @param word_quantiles A numeric vector denoting the quantiles (0-1) used to summarize each word vector dimension. Defaults to 0.10th, 0.50th and 0.90th quantiles.
#'
#' @param reattempt If TRUE, attempts to match terms missing from the wordVec corpus with alternate representations.
#'
#' @param reattempt_regex A list of character vectors containing regular expression pairs to be used for generating alternate representations of words to attempt
#' to match with the wordVec corpus when terms initially cannot be matched. Order matters.
#'
#' @param unique_terms If TRUE, removes duplicate terms from each document - each document is represented only by the presence or absence of a term.

#' @param numericization_method Determines whether word vector summaries are used for documents (faster, specified by
#' `numericization_method = "vector_summaries"`), or whether transformer-based document features are used (slower, specified by `numericization_method = "transformer_based"`)
#'
#' @param textEmbed_control A list with elements denoting the parameters passed to `text::textEmbed` if `numericization_method = "transformer_based"`. Default is
#' `list(tokenizer_parallelism = T,
#'       model = "bert-base-multilingual-uncased",
#'       layers = -2L)`)
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
#' my_wordVecs <- matrix(rnorm(11*50), ncol = 50)
#' row.names(my_wordVecs) <- c("the","true", "thine", "stars", "are" ,
#'                               "fire", ".", "to", "own", "self", "be")
#'
#' #Setup ``documents''
#' my_documentText <- c(
#' "the stars are fire .", #document 1
#' "to thine own self be true ", #document 2
#' "true stars be true ." #document 3
#' )
#'
#' #Get document-level word vector summaries.
#' my_dfm <- undergrad(documentText = my_documentText, wordVecs = my_wordVecs)
#' print( my_dfm )
#'
#' @export
#'
undergrad <- function(documentText,
                      wordVecs = NULL,
                      word_quantiles = c(.1, .5, .9),
                      reattempt = T,
                      reattempt_regex = list(c("\\#",""), c("#\\S+", "<hashtag>"),
                                            c("[[:punct:]]+", ''),c('ing\\b',''),
                                            c('s\\b', ''),c('ed\\b', ''),c('ies\\b', 'y')),
                      unique_terms = T, verbose=T,
                      numericization_method = "vector_summaries",
                      textEmbed_control = list(tokenizer_parallelism = T,
                                               model = "bert-base-multilingual-uncased",
                                               layers = -2L)){
    if(numericization_method == "transformer_based"){

      # initialize transfer learner protocol via text
      library(text)
      textrpp_initialize()

      # get text embeddings, time series dimension modeled via transformer layers
      print("Getting transformer-based features via text::textEmbed, batch size = 5")
      print("Warning: Transformer-based vectorization can be slower than word2vec summaries, especially for long documents.")
      textEmbed_optionsText <- "";if(!is.null(textEmbed_control)){
        textEmbed_optionsText <- paste("",sapply(1:length(textEmbed_control),function(zr){
          if(class(textEmbed_control[[zr]]) == "character"){textEmbed_control[[zr]] <- paste("'",textEmbed_control[[zr]],"'",sep="")}
          paste(names(textEmbed_control)[[zr]],"=",
                textEmbed_control[[zr]] )}),collapse=",")
        textEmbed_optionsText <- paste(",",textEmbed_optionsText,collapse="")
      }
      textEmbed_batches <- cumsum((0:(length(documentText)-1) %% 1000) == 0)
      #textEmbed_batches[] <- 1
      dfm <- tapply(1:length(textEmbed_batches),textEmbed_batches,function(in_){
        eval(parse(text = sprintf("dfm_ <- textEmbed(texts = documentText[in_],
                         keep_token_embeddings = F,
                         logging_level = 'info' %s)$texts[[1]]", textEmbed_optionsText)))
        print(sprintf("%.2f%% percent done getting transformer-based document embeddings",
                      100*(max(in_) / length(textEmbed_batches))))
        return( dfm_ )
      })
      dfm <- as.matrix( do.call(rbind, dfm) )

      print("Done getting transformer-based document features!")
      print("Note: If you created a new conda environment upon running text::textrpp_install, you'll have to save the dfm to disk via write.csv, restart R, and proceed to estimation via readme::readme to avoid errors due to conflict between the conda environment specified in readme() and the one used by text::textEmbed.")
    }

    if(numericization_method == "vector_summaries"){
    if(is.null(wordVecs)){
      cat("NOTE: No word vector matrix specified in 'wordVecs' - Searching for default wordvecs in install directory\n
Searching for: 'glove.6B.200d.txt'\n")
      targetDir = find.package("readme")
      if (file.exists(file.path(targetDir, "glove.6B.200d.txt"))){
        wordVecs_corpus <- data.table::fread(file.path(targetDir, "glove.6B.200d.txt"))
        wordVecs_keys <- wordVecs_corpus[[1]]## first row is the name of the term
        wordVecs_corpus <- as.matrix (  wordVecs_corpus[,-1] )  #
        row.names(wordVecs_corpus) <- wordVecs_keys
        wordVecs <- wordVecs_corpus
        rm(wordVecs_corpus)
        rm(wordVecs_keys)## Remove the original loaded table to save space

      }else{
        stop("Could not find default word vector summaries. Please run download_wordvecs() to obtain the default word vector dictionary\n
          or manually provide a data frame containing the word vectors.\n
             We recommend using a GloVe corpus from https://nlp.stanford.edu/projects/glove/\n")

      }


    }
    ## Sanity check the wordVecs
    if (!is.matrix(wordVecs)){
      stop("Error: 'wordVecs' is not a matrix")
    }

    ## Tokenize the documents using whitespace splits
    tokenized_docs <- tokenizers::tokenize_regex(documentText)

    ### Consider only unique terms
    if (unique_terms == T){
      tokenized_docs <- lapply(tokenized_docs, function(x) return(  unique(x[x!=""])) )
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
    if (reattempt == T){
      ### Which terms didn't get a match
      missing_stems <- unique_stems[is.na(unique_stem_match)]

      ### Missing matches
      match_missing <- rep(NA, length(missing_stems))

      ### For each reg-ex pair in reattempt_regex
      for (indx in 1:length(reattempt_regex)){
        ## Attempt to match given the substitution
        match_missing[is.na(match_missing)] <- match(gsub(missing_stems[is.na(match_missing)],
                                                          pattern = reattempt_regex[[indx]][1],
                                                          replace = reattempt_regex[[indx]][2]),
                                                     wordVec_terms)

      }

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
    document_matrices <- lapply(matched_terms_noNA, function(x) {
                                  if(length(x) > 1){ docMatrix = wordVecs[x,] }
                                  if(length(x) == 1){ docMatrix = t(wordVecs[x,]) }
                                  if(length(x) == 0){ docMatrix = t(rep(NA, times = ncol(wordVecs))) }
                                return( docMatrix )
                                })

    if (verbose == T){
      cat("Computing word vector summaries for each document...\n")
    }

    ### Calculate the summary
    dfm <- do.call(rbind, lapply(document_matrices, function(x){
                                    c(apply(x, 2, function(x_col){
                                      quantile(x_col, c(word_quantiles), na.rm = T)
                                  }))}))
    dfm = apply(dfm, 2, function(x){
      x[is.na(x)] <- mean(x,na.rm = T)
      return( x )
    })
    colnames(dfm) <- c(sapply(colnames(wordVecs),
                              function(z){ paste(z, "_", round(100*word_quantiles), "th_Quantile", sep = "") }))
    }

    ### Are any columns zero-variance?
    column_sds <- apply(dfm, 2, sd)

    ###
    if (length(which(column_sds == 0)) > 0){

      if (verbose == T){
        cat(paste("WARNING: Feature matrix ", colnames(dfm)[which(column_sds == 0)], " has zero variance, dropping from analysis...\n", collapse = "", sep = ""))
      }
    }

    return(dfm[,column_sds != 0])
}
