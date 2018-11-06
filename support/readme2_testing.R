readme2_testing <- function(dtm, dfm, labeledIndicator, categoryVec, nboot = 10,  
                          compareClassifiers = T, compareQuantifiers = T, 
                          features = 16, verbose=T)
{ 
  colnames(dtm) <- paste(colnames(dtm), 1:length(colnames(dtm)[-c(1:3)]), sep = "_")
  
  ### Find word stems that are non-invariant and remove
  
  #########################################
  if (verbose){ cat("Initializing...\n") }

  #### Initialize the data
  categoryVec_unlabeled <- as.factor(categoryVec)[labeledIndicator == 0]
  categoryVec_labeled <- as.factor(categoryVec)[labeledIndicator == 1]
  labeled_pd <- table(categoryVec_labeled);labeled_pd<-labeled_pd/sum(labeled_pd)
  unlabeled_pd <- table( categoryVec_unlabeled );unlabeled_pd <- unlabeled_pd/sum(unlabeled_pd)
  cat_names <- names(labeled_pd)
  dtm_labeled <- dtm[labeledIndicator ==1,]; dfm_labeled <- dfm[labeledIndicator==1,]
  dtm_unlabeled <- dtm[labeledIndicator==0,]; dfm_unlabeled <- dfm[labeledIndicator==0,]
  nCat <- length(labeled_pd)

  ###################
  point_readme <- readme0(dtm = dtm, labeledIndicator = labeledIndicator, 
                       categoryVec = categoryVec, nboot = 100)$point_readme
  readme2_list <- readme(dfm = dfm, labeledIndicator = labeledIndicator, categoryVec = categoryVec, nboot = nboot, verbose = verbose, diagnostics = T)
  point_readme2 <- readme2_list$point_readme
  
  ##################
  #Compare Classifiers
  est_classifier_continuous <- labeled_pd; est_classifier_continuous[] <- NA
  est_classifier <- est_classifier_continuous
  ErrorResultsEnsemble_discrete <- ErrorResultsEnsemble_continuous <- c(data.frame(ensemble = NA, NaiveBayes_est = NA, Regression_est = NA, 
                                                                                   SVM_est = NA, Forest_est = NA))
  ErrorResultsEnsemble_discrete_count <- ErrorResultsEnsemble_continuous_count <- c(data.frame(ensemble_count = NA, NaiveBayes_est_count = NA, Regression_est_count = NA, 
                                                                                               SVM_est_count = NA, Forest_est_count = NA))
  if(compareClassifiers == T){ 
    EnsembleList_discrete <- EnsembleMethod(train_cats = as.character(categoryVec_labeled), 
                                            train_feat = dtm_labeled, test_feat = dtm_unlabeled, 
                                            labeled_pd = labeled_pd)
    ErrorResultsEnsemble_discrete <- lapply(EnsembleList_discrete$final_list, function(xj){ sum(abs(xj[names(unlabeled_pd)] - unlabeled_pd))})
    ErrorResultsEnsemble_discrete_count <- lapply(EnsembleList_discrete$final_list_count, function(xj){ sum(abs(xj[names(unlabeled_pd)] - unlabeled_pd))})
  
    EnsembleList_continuous <- EnsembleMethod(train_cats = as.character(categoryVec_labeled), labeled_pd = labeled_pd, train_feat = dfm_labeled, 
                                              test_feat = dfm_unlabeled)
    ErrorResultsEnsemble_continuous <- lapply(EnsembleList_continuous$final_list, function(xj){ sum(abs(xj[names(unlabeled_pd)] - unlabeled_pd))})
    ErrorResultsEnsemble_continuous_count <- lapply(EnsembleList_continuous$final_list_count, function(xj){ sum(abs(xj[names(unlabeled_pd)] - unlabeled_pd))})
  }
  
  #Compare Quantifiers 
  error_quantify_names <- c("friedman", 'prob', "mixtureL2", "mixtureL1_QUANT", "mixtureHPMF", 
                            "adjCount", "medianSweep", "hdx", "va1","actual","naive" )
  error_quantify <- rep(NA, times = length(error_quantify_names))
  names(error_quantify) <- error_quantify_names
  va2_error <- NA
  if(compareQuantifiers == T){
    source("./support/readme2_va2_replication.R")
    source("./support/readme2_quantification_algos.R")
    termMatrix_use <- cbind(1:length(categoryVec), categoryVec, labeledIndicator, dtm)
    colnames(termMatrix_use)[1:3] <- c("FILENAME",  "categoryLabels", "TRAININGSET")
    va2_results <- try(va2(termMatrix=termMatrix_use,seed=ceiling(runif(1,1,1000)), nsymps=2), T)  
    set.seed(Sys.time())
    va2_error <- try(sum(abs(va2_results[names(unlabeled_pd)]-unlabeled_pd)), T)  
    
    categoryVec_labeled_factor <- as.factor(categoryVec)[labeledIndicator==1]
    categoryVec_unlabeled_factor <- as.factor(categoryVec)[labeledIndicator==0]
    QUANTIFY_ALL_RESULTS <- try(quantifyAll_QUANT(trainMatrix = as.data.frame(cbind(categoryVec_labeled_factor, dtm[labeledIndicator==1,])),
                                                  testMatrix = as.data.frame(cbind(categoryVec_unlabeled_factor, dtm[labeledIndicator==0,])),
                                                  baseClassifierModelFunction=liblinearModelFunction,
                                                  baseClassifierPredFunction=liblinearPredFunction,
                                                  trials=10), T)  
    error_quantify <- try(apply(QUANTIFY_ALL_RESULTS[,-1][,names(unlabeled_pd)], 1, function(x){ sum(abs(f2n(x) - unlabeled_pd)) }), T)
    if(length(error_quantify) != length(c(QUANTIFY_ALL_RESULTS[,1]))){browser()}
    names(error_quantify) <- QUANTIFY_ALL_RESULTS[,1]
    set.seed(Sys.time())
  }

  return(list(error_readme = sum(abs( point_readme[names(unlabeled_pd)]-unlabeled_pd) ) ,
              error_readme2 = sum(abs( point_readme2[names(unlabeled_pd)]-unlabeled_pd) ) ,
              
              #diagnostics 
              PrDDiv_withMatching = readme2_list$diagnostics$MatchedPrD_div, 
              PrDDiv = readme2_list$diagnostics$OrigPrD_div, 
              
              ESGivenDDiv_withMatching = readme2_list$diagnostics$MatchedESGivenD_div, 
              ESGivenDDiv = readme2_list$diagnostics$OrigESGivenD_div, 
              
              #quantification 
              error_quantify = list(error_quantify), 
              
              #other results 
              va2_error = va2_error, 
              ErrorResultsEnsemble_discrete = ErrorResultsEnsemble_discrete,
              ErrorResultsEnsemble_discrete_count = ErrorResultsEnsemble_discrete_count,
              ErrorResultsEnsemble_continuous = ErrorResultsEnsemble_continuous, 
              ErrorResultsEnsemble_continuous_count = ErrorResultsEnsemble_continuous_count   ) )  
}
