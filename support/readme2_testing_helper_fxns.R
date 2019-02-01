f2n <- function(.){as.numeric(as.character(.))}
rdirichlet <- function(n, alpha) {
  normalize <- function(.) . / sum(.)
  samps <- vapply(alpha, function(al) stats::rgamma(n, al, 1), numeric(n))
  if(class(samps) == "matrix" ){ ret_q <- t(apply(samps, 1, normalize)) }
  if(class(samps) == "numeric" ){ ret_q <- normalize(samps) }
  return( ret_q ) 
}
liblinearModelFunction<-function(xTrain){
  LiblineaR::LiblineaR(data=xTrain[,-1],target=xTrain[,1], type=7,cost=1, bias=TRUE)
}
liblinearPredFunction<-function(PLT, testData){
  probs<-predict(PLT,testData, proba=T)$probabilities
  return(  probs ) 
}
naiveMethod<-function(trainMatrix,testMatrix,baseClassifierModelFunction=liblinearModelFunction,baseClassifierPredFunction=liblinearPredFunction){
  model<-baseClassifierModelFunction(trainMatrix)
  probs<-baseClassifierPredFunction(model,testMatrix)
}
EnsembleMethod <- function(train_cats, train_feat, test_feat, unlabeled_pd, report_times =F){ 
  KeepCols <- apply(test_feat, 2, sd) > 0 & apply(train_feat, 2, sd) > 0 
  train_feat <- train_feat[,KeepCols]
  test_feat  <- test_feat[,KeepCols]
  
  train_feat <- as.data.frame(  cbind( train_cats, train_feat) )    
  train_feat[,-1] <- apply(train_feat[,-1] , 2, f2n); colnames(train_feat)[1] <- "TRUTH"

  outer_start_time <- proc.time()[3]
  Regression_est <-  naiveMethod(train_feat, test_feat)
  Regression_est_count <- try( apply(naiveMethod(train_feat, test_feat), 
                                     1, function(zeta){ sample(names(zeta)[zeta==max(zeta)], 1) }), T)
  Regression_est_count <- prop.table(table( Regression_est_count ))
  Regression_est <- try(colMeans(  Regression_est, na.rm = T) , T)
  Regression_time = proc.time()[3]-outer_start_time
 
  outer_start_time <- proc.time()[3]
  Forest_est <- try( predict(randomForest::randomForest(x = apply(train_feat[,-1], 2, f2n), y = as.factor(train_feat[,1]), 
                                                                 ntree = 10)  , test_feat , type = "prob") , T)  
  Forest_est_count <- try(prop.table(table(colnames(Forest_est)[as.numeric(apply(Forest_est, 1, function(x){sample(as.character(which.max(x)), 1)}))])), T) 
  Forest_est <- try(colMeans( Forest_est, na.rm = T  ) , T)  
  Forest_time = proc.time()[3]-outer_start_time
  
  outer_start_time <- proc.time()[3]
  SVM_est <-  try(attr(predict(e1071::svm(x = train_feat[,-1], y = as.factor(train_feat[,1]), probability = T, tolerance = 0.01 ),
                                  test_feat, probability = T), "probabilities"),T)  
  SVM_est_count <- try(prop.table(table(colnames(SVM_est)[as.numeric(apply(SVM_est, 1, function(x){sample(as.character(which.max(x)), 1)}))])), T) 
  SVM_est <- try(colMeans(SVM_est,na.rm =  T), T)  
  SVM_time = proc.time()[3]-outer_start_time
  
  outer_start_time <- proc.time()[3]
  NaiveBayes_est <- try(predict(e1071::naiveBayes(x = train_feat[,-1], y = as.factor(train_feat[,1]), laplace = 1),
                                test_feat, type = c("raw") ), T)  
  NaiveBayes_est_count <- factor(apply(NaiveBayes_est, 1, function(x){names(which.max(x)[1]) }), 
                                  levels = unique(as.factor(train_feat[,1])))
  NaiveBayes_est_count <- try(table(NaiveBayes_est_count)/sum(table(NaiveBayes_est_count)))
  NaiveBayes_est <- try(colMeans(NaiveBayes_est), T)  
  Bayes_time = proc.time()[3]-outer_start_time
  
  method_names <- c("Regression", "Forest", "SVM", "NaiveBayes")
  for(method_name in method_names){ 
    eval(parse(text = sprintf("%s_est <- %s_est[names(unlabeled_pd)]; %s_est[is.na(%s_est)] <- 0; %s_est <- f2n(%s_est); 
                              names(%s_est) <- names(unlabeled_pd)", 
                              method_name, method_name, method_name, method_name, method_name, method_name, method_name) ))
    eval(parse(text = sprintf("if(all(%s_est==0)){%s_est[] <- NA}", method_name, method_name) ))
    eval(parse(text = sprintf("%s_est_count <- %s_est_count[names(unlabeled_pd)]; %s_est_count[is.na(%s_est_count)] <- 0 ;
                              %s_est_count <- f2n(%s_est_count); names(%s_est_count) <- names(unlabeled_pd)", 
                              method_name, method_name, method_name, method_name, method_name, method_name, method_name) ))
    eval(parse(text = sprintf("if(all(%s_est_count==0)){%s_est_count[] <- NA}", method_name, method_name) ))
  }
  
  EstMat <- rbind(Regression_est, Forest_est, SVM_est, NaiveBayes_est)
  EstMat <- apply(EstMat, 2, f2n)
  est_final <- colMeans(EstMat, na.rm = T) 
  
  EstMat_count <- rbind(Regression_est_count, Forest_est_count, SVM_est_count, NaiveBayes_est_count)
  EstMat_count <- apply(EstMat_count, 2, f2n)
  est_final_count <- colMeans(EstMat_count, na.rm = T) 
  
  if(report_times == F){ 
    final_list <- list(ensemble = est_final, 
                       NaiveBayes_est = NaiveBayes_est,
                       SVM_est = SVM_est, Forest_est = Forest_est, Regression_est = Regression_est)
    final_list_count <- list(ensemble_count = est_final_count, 
                             NaiveBayes_est_count = NaiveBayes_est_count,
                             SVM_est_count = SVM_est_count, 
                             Forest_est_count = Forest_est_count, Regression_est_count = Regression_est_count)
    #sort( sapply(ls(),function(x){object.size(get(x))})) 
    return( list(final_list=final_list,
                 final_list_count=final_list_count) )   
  }
  if(report_times == T){ 
    return( list(SVM_time=SVM_time,
                 Bayes_time = Bayes_time, 
                 Forest_time = Forest_time,
                 Regression_time=Regression_time) )   
  }
}
