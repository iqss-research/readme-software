vec2prob <- function(.){x <- table(.); x/sum(x)}

#' cleanText
#' 
#' Standard preprocessing code for ASCII texts. Removes HTML tags, URLs, linebreaks. Converts standard emoticons to tokens. Removes non-informative punctuation.
#' 
#' @param my_text Vector of character strings containing the raw document texts.
#' 
#' @return A vector of character strings with the processed texts, each token is separated by a space.
#' 
#' @export 
cleanText <- function(my_text){ 
  finalEncoding <- "ASCII"
  
  ### Convert to ASCII encoding
  my_text <- sapply(my_text, 
                    function(x){ startingEncoding <- Encoding(x)
                      if(startingEncoding == "unknown"){ x<-iconv(x, from = finalEncoding, to = finalEncoding, sub = '', mark = T) } 
                      if(startingEncoding != "unknown"){ x<-iconv(x, from = startingEncoding, to = finalEncoding, sub = '', mark = T) }
                      return(x) })
  names(my_text) <- NULL 
  ## Remove HTML/XML tags
  my_text <- gsub(my_text, pattern = "<.*?>", replace = " <html> ") 
  
  ## Remove newline, carriage return and tabs
  my_text <- gsub(my_text, pattern = "\n", replace = " ") 
  my_text <- gsub(my_text, pattern = "\r", replace = " ") 
  my_text <- gsub(my_text, pattern = "\t", replace = " ") 
  
  ## Convert URLs to generic <url> feature
  url_pattern1 <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  url_pattern2 <- " ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)"
  my_text <- gsub(my_text, pattern = url_pattern1, replace = " <url> ") 
  my_text <- gsub(my_text, pattern = url_pattern2, replace = " <url> ") 
  
  ## Convert standard emoticons
  my_text <- gsub(my_text, pattern = ":-\\)", replace = " <smile> ")
  my_text <- gsub(my_text, pattern = "\\^_\\^", replace = " <smile> ")
  my_text <- gsub(my_text, pattern = ":\\)", replace = " <smile> ")
  my_text <- gsub(my_text, pattern = "\\(:", replace = " <smile> ")
  my_text <- gsub(my_text, pattern = "\\(-:", replace = " <smile> ")
  my_text <- gsub(my_text, pattern = ":D", replace = " <smile> ")
  my_text <- gsub(my_text, pattern = ":-\\(", replace = " <sadface> ")
  my_text <- gsub(my_text, pattern = ":\\(", replace = " <sadface> ")
  my_text <- gsub(my_text, pattern = "\\):", replace = " <sadface> ")
  my_text <- gsub(my_text, pattern = "\\)-:", replace = " <sadface> ")
  my_text <- gsub(my_text, pattern = "D:", replace = " <sadface> ")
  my_text <- gsub(my_text, pattern = "<3", replace = " ♥ ")
  
  ## Remove punctuation
  my_text <- gsub(my_text, pattern = "\\s*-\\B|\\B-\\s*", replace = " ") 
  my_text <- gsub(my_text, pattern = " \\- ", replace = " ") 
  my_text <- gsub(my_text, pattern = "\\--", replace = " ") 
  my_text <- gsub(my_text, pattern = "@\\S+", replace = " <user> ") ## Twitter-specific, any @text converted to a generic <user> feature
  
  my_text <- gsub(my_text, pattern = "[[:digit:]]+", replace = " <number> ") ## Convert numbers to generic <number> feature
  
  ## More removal of punctuation
  my_text <- gsub(my_text, pattern = '\\*', replace = " ")
  my_text <- gsub(my_text, pattern = "\\.\\.\\.", replace = " … ")
  my_text <- gsub(my_text, pattern = "\\.", replace = " ") 
  my_text <- gsub(my_text, pattern = "\\:", replace = " ") 
  my_text <- gsub(my_text, pattern = "\\;", replace = " ") 
  my_text <- gsub(my_text, pattern = "\\,", replace = " ") 
  my_text <- gsub(my_text, pattern = "\\(", replace = " ") 
  my_text <- gsub(my_text, pattern = "\\)", replace = " ") 
  my_text <- gsub(my_text, pattern = '\\]', replace = " ")
  my_text <- gsub(my_text, pattern = '\\[', replace = " ")
  my_text <- gsub(my_text, pattern = "\\/", replace = " ") 
  my_text <- gsub(my_text, pattern = "\\\\", replace = " ") 
  my_text <- gsub(my_text, pattern = '\\"', replace = " ") 
  my_text <- gsub(my_text, pattern = '\\¨', replace = " ")
  my_text <- gsub(my_text, pattern = "\\ô", replace = " ") 
  
  ## Informative punctuation become their own features
  my_text <- gsub(my_text, pattern = '\\=', replace = " = ")
  my_text <- gsub(my_text, pattern = '\\+', replace = " + ")
  my_text <- gsub(my_text, pattern = "\\!", replace = " ! ") 
  my_text <- gsub(my_text, pattern = "\\?", replace = " ? ") 
  my_text <- gsub(my_text, pattern = "\\õ", replace = " ' ") 
  my_text <- gsub(my_text, pattern = "\\õ", replace = " ' ")
  
  ## Contractions are split out
  my_text <- gsub(my_text, pattern = "\\'s", replace = " \\'s ")
  my_text <- gsub(my_text, pattern = "n\\'t ", replace = " n\\'t ")
  my_text <- gsub(my_text, pattern = "\\'d ", replace = " \\'d ") 
  my_text <- gsub(my_text, pattern = "\\'re ", replace = " \\'re ") 
  my_text <- gsub(my_text, pattern = "\\'ve ", replace = " \\'ve ") 
  my_text <- gsub(my_text, pattern = "\\'ll ", replace = " \\'ll ") 
  my_text <- gsub(my_text, pattern = "\\'m ", replace = " \\'m ") 
  
  ## Standard abbreviations
  my_text <- gsub(my_text, pattern = "gov\\'t", replace = " government ") 
  
  ### Non-separating punctuation
  my_text <- gsub(my_text, pattern = "%", replace = "") 
  my_text <- gsub(my_text, pattern = ">>", replace = "") 
  my_text <- gsub(my_text, pattern = "<<", replace = "") 
  my_text <- gsub(my_text, pattern = "__", replace = "") 
  
  my_text <- gsub(my_text, pattern = " -", replace = " ") 
  my_text <- gsub(my_text, pattern = "- ", replace = " ") 

  ## Informative punctuation (brackets, ampersands, etc...)  
  my_text <- gsub(my_text, pattern = "\\{", replace = " \\{ ")
  my_text <- gsub(my_text, pattern = "\\}", replace = " \\} ")
  my_text <- gsub(my_text, pattern = "\\&", replace = " \\& ") 
  my_text <- gsub(my_text, pattern = "\\$", replace = " $ ") 
  
  # Tildes and carats - non-informative punctuation
  my_text <- gsub(my_text, pattern = "\\^\\^", replace = "") 
  my_text <- gsub(my_text, pattern = "\\~", replace = " ") 
  my_text <- gsub(my_text, pattern = "  ", replace = " ")
  
  # Make lower case
  my_text <- tolower(my_text)
  
  # Return output
  return( my_text )  
}

f2n <- function(.){as.numeric(as.character(.))}

knn_adapt <- function(reweightSet = NULL, fixedSet = NULL, k=1, distMat = NULL){
  library2("optmatch", loadin = F)
  if(is.null(distMat)){ 
    fixedSet <- as.data.frame(  cbind(1, fixedSet)) ; colnames(fixedSet)[1] <- "fixed_indicator"
    reweightSet <- as.data.frame(  cbind(0, reweightSet))  ; colnames(reweightSet)[1] <- "fixed_indicator"
    distMat <- eval(parse(text = sprintf('optmatch::match_on(%s, 
                                          data = rbind(reweightSet, fixedSet),
                                          method = "euclidean")', 
                                          sprintf("fixed_indicator~%s", paste(colnames(reweightSet)[-1], collapse = "+"))) ) )
  }
  match_indices_list <- apply(distMat, 1, function(x){ list(which(x <= sort(x)[k] )[1:k]) }) #we must assume that the data are scrambled
  max_matched_dists <- sapply(1:nrow(distMat),function(x){  max(distMat[x,match_indices_list[[x]][[1]]]) })
  addback_radius <- summary(max_matched_dists)[2] 
  
  within_radius_indices <- which( colSums(distMat < addback_radius) > 0)
  matched_indices <- unlist(match_indices_list)
  return_indices <-   c(matched_indices,within_radius_indices[!within_radius_indices %in% matched_indices])
  return(list(return_indices=return_indices,
              distMat = distMat))
}


colSds <- function (x, center = NULL, dim. = dim(x)){ n <- dim.[1]; x <- x * x; x <- colMeans(x); x <- (x - center^2); sqrt (  x * (n/(n - 1)) )  }
FastScale <- function(x,cm=NULL,csd=NULL){
  if(is.null(cm)){cm = .colMeans(x, m = nrow(x), n = ncol(x))}# Get the column means
  if(is.null(csd)){csd = colSds(x, center = cm)}# Get the column sd
  return( t( (t(x) - cm) / csd ) )
} 

readme_est_fxn <- function(X, Y){
  solution_ <- try(limSolve::lsei(A = X, B=Y, E=matrix(nrow=1, ncol=ncol(X), data=rep(1., ncol(X))), 
                                                                          F=c(1.), G=diag(rep(1, ncol(X))), H = rep(0., ncol(X)))$X, TRUE)
  return( solution_ )  
  }

scale_fxn <- function(old_seq, newmin, newmax){(newmax - newmin) * (old_seq - min(old_seq)) / (max(old_seq) - min(old_seq)) + newmin}

