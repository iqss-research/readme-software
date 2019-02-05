Winsorize_fxn <- function(x){ 
  sum_x <- summary(x); qr_ <- 1.5*diff(sum_x[c(2,5)]);
  x[x < sum_x[2]- qr_] <-sum_x[2]- qr_; x[x > sum_x[5]+qr_] <- sum_x[5] + qr_
  return(x)
}

toDTM <- function(myText){
  myText_ = tokenizers::tokenize_word_stems(myText)
  myText_ = lapply(myText_, function(x){unique(x)})
  myStems_tab = table(unlist(myText_))
  myStems_keep <- names( myStems_tab[myStems_tab > 0.01 * length(myText)] )
  myText_ = lapply(myText_, function(x){x[x %in% myStems_keep]})
  dfm_ = as.data.frame( matrix(0, nrow = length(myText), ncol = length(myStems_keep)) ) 
  colnames(dfm_) <- myStems_keep
  for(iaa in 1:length(myText_)){ 
    dfm_[iaa,myText_[[iaa]]] <- 1 
  }
  return( dfm_ )               
}

f2n <- function(.){as.numeric(as.character(.))}
colSds <- function (x, center = NULL, dim. = dim(x)){ n <- dim.[1]; x <- x * x; x <- colMeans(x); x <- (x - center^2); sqrt (  x * (n/(n - 1)) )  }
FastScale <- function(x,cm=NULL,csd=NULL){
  if(is.null(cm)){cm = .colMeans(x, m = nrow(x), n = ncol(x))}# Get the column means
  if(is.null(csd)){csd = colSds(x, center = cm)}# Get the column sd
  return( t( (t(x) - cm) / csd ) )
} 
