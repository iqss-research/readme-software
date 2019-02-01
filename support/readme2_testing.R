DiscreteTest <- function(dtm, labeledIndicator, categoryVec,compareClassifiers = T, compareQuantifiers = T, report_times = F){ 
  doVA2 = T; doQuantifiers <- F
  colnames(dtm) <- paste(colnames(dtm), 1:length(colnames(dtm)[-c(1:3)]), sep = "_")
  
  ### Find word stems that are non-invariant and remove
  #########################################
  dtm = dtm[,colSums(dtm)>0]
  
  #### Initialize the data
  categoryVec_labeled_factor <- as.factor(categoryVec)[labeledIndicator==1]
  categoryVec_unlabeled_factor <- as.factor(categoryVec)[labeledIndicator==0]
  unlabeled_pd <- prop.table(table( categoryVec_unlabeled_factor ))
  cat_names <- names(unlabeled_pd)
  nCat <- length(unlabeled_pd)
  
  #Storage for Classifiers Analysis 
  readme_error = NA
  est_classifier <- unlabeled_pd; 
  est_classifier[] <- NA
  ErrorResultsEnsemble_discrete <- c(data.frame(ensemble = NA, NaiveBayes_est = NA, Regression_est = NA, 
                                                                                   SVM_est = NA, Forest_est = NA))
  ErrorResultsEnsemble_discrete_count <- c(data.frame(ensemble_count = NA, NaiveBayes_est_count = NA, Regression_est_count = NA, 
                                                                                               SVM_est_count = NA, Forest_est_count = NA))
  ###################
  if(compareClassifiers == T){ 
  point_readme <- readme0(dtm = dtm, labeledIndicator = labeledIndicator, 
                          categoryVec = categoryVec, nboot = 50)$point_readme
  readme_error = sum(abs( point_readme[names(unlabeled_pd)]-unlabeled_pd) )
  EnsembleList_discrete <- EnsembleMethod(train_cats = as.character(categoryVec_labeled_factor), 
                                            train_feat = dtm[labeledIndicator ==1,], 
                                            test_feat = dtm[labeledIndicator==0,], 
                                            unlabeled_pd = unlabeled_pd)
  ErrorResultsEnsemble_discrete <- lapply(EnsembleList_discrete$final_list, function(xj){ sum(abs(xj[names(unlabeled_pd)] - unlabeled_pd))})
  ErrorResultsEnsemble_discrete_count <- lapply(EnsembleList_discrete$final_list_count, function(xj){ sum(abs(xj[names(unlabeled_pd)] - unlabeled_pd))})
  rm(EnsembleList_discrete)
  } 
  
  #Compare Quantifiers 
  error_quantify_names <- c("friedman", 'prob', "mixtureL2", "mixtureL1_QUANT", "mixtureHPMF", 
                            "adjCount", "medianSweep", "hdx", "va1","actual","naive" )
  error_quantify <- rep(NA, times = length(error_quantify_names))
  names(error_quantify) <- error_quantify_names
  va2_error <- NA
  if(compareQuantifiers == T){
    if(doQuantifiers == T){ 
    source("./support/readme2_quantification_algos.R", local = T)
    QUANTIFY_ALL_RESULTS <- try(quantifyAll_QUANT(trainMatrix = as.data.frame(cbind(categoryVec_labeled_factor, as.data.frame(dtm[labeledIndicator==1,]))),
                                                  testMatrix = as.data.frame(cbind(categoryVec_unlabeled_factor, as.data.frame(dtm[labeledIndicator==0,]))),
                                                  baseClassifierModelFunction=liblinearModelFunction,
                                                  baseClassifierPredFunction=liblinearPredFunction,
                                                  trials=5), T)  
    error_quantify <- try(apply(QUANTIFY_ALL_RESULTS[,-1][,names(unlabeled_pd)], 1, function(x){ sum(abs(f2n(x) - unlabeled_pd)) }), T)
    names(error_quantify) <- QUANTIFY_ALL_RESULTS[,1]
    rm(QUANTIFY_ALL_RESULTS)
    } 
    
    if(doVA2 == T){ 
      source("./support/readme2_va2_replication.R", local = T)
      dtm <- cbind(1:length(categoryVec), categoryVec, labeledIndicator, as.data.frame( dtm) ) 
      colnames(dtm)[1:3] <- c("FILENAME",  "categoryLabels", "TRAININGSET")
      va2_results <- try(va2(termMatrix=dtm,seed=runif(1,1,1e6), nsymps=2), T)  
      va2_error <- try(sum(abs(va2_results[names(unlabeled_pd)]-unlabeled_pd)), T)  
    }
    set.seed(Sys.time())
  }
  
  rm(dtm)#clear the dtm 
  
  #sort( sapply(ls(),function(x){object.size(get(x))})) 
  return(list(error_readme = readme_error ,
              
              #quantification 
              error_quantify = list(error_quantify), 
              
              #other results 
              va2_error = va2_error, 
              ErrorResultsEnsemble_discrete = ErrorResultsEnsemble_discrete,
              ErrorResultsEnsemble_discrete_count = ErrorResultsEnsemble_discrete_count) )
  
}

ContinuousTest <- function(dfm, labeledIndicator, categoryVec, nboot = 10)
{ 
  #########################################
  #### Initialize the data
  categoryVec_labeled_factor <- as.factor(categoryVec)[labeledIndicator==1]
  categoryVec_unlabeled_factor <- as.factor(categoryVec)[labeledIndicator==0]
  unlabeled_pd <- prop.table(table( categoryVec_unlabeled_factor ))
  cat_names <- names(unlabeled_pd)
  nCat <- length(unlabeled_pd)

  #Storage for Classifiers Analysis 
  est_classifier_continuous <- unlabeled_pd; 
  est_classifier_continuous[] <- NA; 
  ErrorResultsEnsemble_continuous <- c(data.frame(ensemble = NA, NaiveBayes_est = NA, Regression_est = NA, 
                                                                                   SVM_est = NA, Forest_est = NA))
  ErrorResultsEnsemble_continuous_count <- c(data.frame(ensemble_count = NA, NaiveBayes_est_count = NA, Regression_est_count = NA, 
                                                                                               SVM_est_count = NA, Forest_est_count = NA))
  
  EnsembleList_continuous <- EnsembleMethod(train_cats = as.character(categoryVec_labeled_factor), unlabeled_pd = unlabeled_pd, 
                                              train_feat = dfm[labeledIndicator==1,], 
                                              test_feat = dfm[labeledIndicator==0,])
  ErrorResultsEnsemble_continuous <- lapply(EnsembleList_continuous$final_list, function(xj){ sum(abs(xj[names(unlabeled_pd)] - unlabeled_pd))})
  ErrorResultsEnsemble_continuous_count <- lapply(EnsembleList_continuous$final_list_count, function(xj){ sum(abs(xj[names(unlabeled_pd)] - unlabeled_pd))})
  rm( EnsembleList_continuous )  
  
  #sort( sapply(ls(),function(x){object.size(get(x))})) 
  return(list(ErrorResultsEnsemble_continuous = ErrorResultsEnsemble_continuous, 
              ErrorResultsEnsemble_continuous_count = ErrorResultsEnsemble_continuous_count   ) )  
}
