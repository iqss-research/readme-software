#' cleanme
#' 
#' Standard preprocessing code for ASCII texts. Removes HTML tags, URLs, linebreaks. Converts standard emoticons to tokens. Removes non-informative punctuation.
#' 
#' @param my_text Vector of character strings containing the raw document texts.
#' 
#' @param finalEncoding A character string indicating the desired encoding for the text vector (default = "ASCII")
#' 
#' @return A vector of character strings with the processed texts, each token is separated by a space.
#' 
#' @export 
#' 
#' 
cleanme <- function(my_text,finalEncoding = "ASCII"){ 
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
  my_text_ <- try(gsub(my_text, pattern = "<3", replace = " ♥ "),T)
  if(!class(my_text_) == 'try-error'){my_text <- my_text_}
  rm(my_text_)
  
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

#' download_wordvecs
#' 
#' Downloads default word vector dictionary to 'readme' install directory. 
#' 
#' @param url URL of word vector file. Defaults to the pre-trained GloVe Wikipedia 200-dimensional vectors.
#' @param targetDir Target directory to download files. If NULL uses readme installation directory.
#' 
#' @return Path to downloaded word vector dictionary
#' 
#' @export 
#' 
#' 
download_wordvecs <- function(url = "http://gking-projects.iq.harvard.edu/files/glove.6B.200d.zip", targetDir=NULL){
  ## Get the target directory if NULL
  if (is.null(targetDir)){
    targetDir = find.package("readme")
  }
  
  ## Get the filename of the url
  filename = basename(url)
  
  ## Does the file exist already
  if (file.exists(file.path(targetDir, filename))){
    stop(paste(file.path(targetDir, filename), "already exists"))
  }else{
    ## Download the file
    download.file(url, destfile=file.path(targetDir, filename))
    cat("Download complete.\n")
    ## If the file is a zip, unpack it
    if (tools::file_ext(file.path(targetDir, filename)) == "zip"){
      ## Unpack the file
      cat("Unpacking zip archive.\n")
      ## Unzipping
      unzippedFiles = unzip(file.path(targetDir, filename), exdir = targetDir)
      cat("Complete.\n")
      return(unzippedFiles)
    }else{
      return(file.path(targetDir, filename))
    }
    
  }
  
}


Winsorize_values <- function(x){ 
  sum_x <- summary(x); qr_ <- 1.5*diff(sum_x[c(2,5)]);
  return( c( sum_x[2] - qr_,  sum_x[5]+qr_) )  
}

vec2prob <- function(.){x <- table(.); x/sum(x)}

f2n <- function(.){as.numeric(as.character(.))}

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
