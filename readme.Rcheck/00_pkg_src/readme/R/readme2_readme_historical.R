
#' readme0
#' Internal
#' @keywords internal
#' @export
readme0 <- function(dtm, labeledIndicator, categoryVec, features = 15, nboot = 10, probWt = 1){
  dtm <- dtm[,colSums(  dtm[labeledIndicator ==1,] ) > 0]
  #assign feature sampling weights
  if (length(probWt)==1|is.null(probWt)) {
    if(probWt == 1){
      freq <- apply(dtm[labeledIndicator ==1,], 2, sum)/nrow(dtm[labeledIndicator ==1,]);
      binweight <- freq*(1-freq); rm(freq)
      probWt <- binweight/sum(binweight); rm(binweight)
    } else  if (probWt==0){ probWt<-NULL }
  }

  #remove invarianet columns
  invariant.cols <- colnames(dtm)[colMeans(dtm[labeledIndicator == 1,]) == 0]
  if (length(invariant.cols) > 0){
    if (length(probWt) > 1){ probWt <- probWt[!(colnames(dtm) %in% invariant.cols)] }
    dtm <- dtm[,!(colnames(dtm) %in% invariant.cols)]
  }
  if (features > ncol(dtm)){ stop("'features' is greater than the actual number of features") }

  #setup
  categoryVec_unlabeled <- categoryVec[labeledIndicator == 0]
  categoryVec_labeled <- categoryVec[labeledIndicator == 1]
  labeled_pd <- vec2prob(categoryVec_labeled)
  unlabeled_pd <- vec2prob( categoryVec_unlabeled )
  cat_names <- names(labeled_pd)
  dtm_labeled <- dtm[labeledIndicator ==1,]
  dtm_unlabeled <- dtm[labeledIndicator==0,]
  boot_readme <- matrix(nrow=nboot, ncol=length(cat_names))
  colnames(boot_readme) <- cat_names

  ### Placeholder for coefficients (in bootstrap where some categories may not appear)
  hold_coef <- rep(0, length(cat_names)); names(hold_coef) <- cat_names

  for(iter_i in 1:nboot){
    # get P(S) and P(S|D) matrices
    sampled_features <- try(sample(colnames(dtm_labeled), features, replace=FALSE, prob=probWt), T)
    labeled_profiles <- apply(dtm_labeled[,sampled_features], 1, function(x) paste(x, collapse = "") )
    unlabeled_profiles <- apply(dtm_unlabeled[,sampled_features], 1, function(x) paste(x, collapse = "") )
    my_loop_intersection <- intersect(unlabeled_profiles, labeled_profiles)
    x.use <- table(labeled_profiles, by = categoryVec_labeled)
    x.use <- x.use[my_loop_intersection,]
    y.use <- table(unlabeled_profiles)[ my_loop_intersection ]

    y.use <- y.use / sum( y.use )
    x.use <- try(apply(x.use, 2, function(x) x / sum( x ) ) , T)

    # Do the regression
    est_readme <- readme_est_fxn(X = x.use, Y=y.use)
    if(any(est_readme < 0)){ est_readme[1:length(est_readme)] <- NA }

    if(!"try-error" %in% class(est_readme)){
      temp_est_readme <- hold_coef
      temp_est_readme[names(est_readme)] <- est_readme
      boot_readme[iter_i,names(temp_est_readme)] <- temp_est_readme
    }
  }

  boot_readme[apply(boot_readme, 1, function(x) any(x < 0)),] <- NA
  point_readme <- colMeans( boot_readme, na.rm = T )
  return(list(point_readme=point_readme))
}
