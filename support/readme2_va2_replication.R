va2 <-
  function(termMatrix,seed, nsymps=2, ...) {
    # This is the function which runs all of the VA2 clusters and merges them into a single "ensemble" estimate.
    
    
    # Takes the term matrix which contains 
    ensemble_VA2<-function(termMatrix,seed, nsymptoms=2){
      # Measure running time
      startTimeOrg <- proc.time()
      ensembleUnits<-c("va2Stacked","va2BiasCorrection","va2NoiseMinimization","va2CentroidPrediction","va2NmPrediction","ensemble")
      timeList<-data.frame(algorithm=ensembleUnits,elapsedTime=NA)
      
      #log <- file("readme2.log", "w")
      #out <- file("results.csv", "w")
      
      if (is.na(seed)) seed<-getSeed()
      set.seed(seed)
      #cat("The random seed used in this run is", seed, "\n", file=log)
      #cat(paste(RNGkind()[1]), " is used to generate random numbers\n", file=log)

      data<-getData(termMatrix)
      
      #cat("Training Size(",paste(data$categoryLabels,collapse=","),")\n",file=log)
      #cat(paste(data$size,collapse=","),"\n",file=log)
      #cat("Term Matrix Test Size,",dim(data$test)[1],",",dim(data$test)[2],"\n",file=log)
      #cat("Running va2stacked\n",file=log)
      
      startTime <- proc.time()
      coefBase<-coefva2Stacked<-try(va2Stacked(data, nsymptoms=nsymptoms), T)  
      timeList[1,2]<-round((proc.time()-startTime)[3])
      coefList<-data.frame(array(dim=c(6,length(data$categoryLabels)+1)))
      colnames(coefList)<-c("algorithm",data$categoryLabels)
      coefList$algorithm<-ensembleUnits
      coefList[1,names(coefva2Stacked)]<-coefva2Stacked
      set.seed(seed)
      #cat("Running va2BiasCorrection\n",file=log)
      startTime <- proc.time()
      coefva2BiasCorrection<-try(va2BiasCorrection(data,coefBase), T)  
      timeList[2,2]<-round((proc.time()-startTime)[3])
      coefList[2,names(coefva2BiasCorrection)]<-coefva2BiasCorrection
      set.seed(seed)
      #cat("Running va2NoiseMinimization\n",file=log)
      startTime <- proc.time()
      coefva2NoiseMinimization<-try(va2NoiseMinimization(data,coefBase), T)  
      timeList[3,2]<-round((proc.time()-startTime)[3])
      coefList[3,names(coefva2NoiseMinimization)]<-coefva2NoiseMinimization
      set.seed(seed)
      #cat("Running va2CentroidPrediction\n",file=log)
      startTime <- proc.time()
      coefva2CentroidPrediction<-try(va2CentroidPrediction(data), T); coefva2CentroidPrediction <- try(table(coefva2CentroidPrediction)/sum(table(coefva2CentroidPrediction)), T)
      timeList[4,2]<-round((proc.time()-startTime)[3])
      coefList[4,names(coefva2CentroidPrediction)]<-coefva2CentroidPrediction
      set.seed(seed)
      #cat("Running va2NmPrediction\n",file=log)
      startTime <- proc.time()
      coefva2NmPrediction <- centroidClassifier(training=data$train[,-1],trainingLabels=data$train[,1], test=data$test[,-1],distanceMetric="euclidean")
      coefva2NmPrediction <- table(coefva2NmPrediction)/sum(table(coefva2NmPrediction))
      coefList[5,names(coefva2NmPrediction)]<- coefva2NmPrediction
      
      coefEnsembleRange<-try(apply(coefList[-6,-1],2,function(x) max(x,na.rm=T)-min(x,na.rm=T)), T)  
      coefEnsemble<-apply(coefList[-6,-1],2,mean,na.rm=T)
      
      #logInfo(out,cbind(names(coefEnsemble),coefEnsemble,coefEnsembleRange))
      coefList[6,-1]<-coefEnsemble
      #logInfo(log,coefList,header=paste("Coefs(",paste(names(coefEnsemble),collapse=","),")",sep=""))
      timeList[6,2]<-round((proc.time()-startTimeOrg)[3])
      
      #cat("Running SVM\n",file=log)
      startTime <- proc.time()
      coefSVMprediction<-try(svmPrediction(data), T)
      timeListSVM <-data.frame(algorithm=c("svm"),elapsedTime=NA)
      timeListSVM[1,2]<-round((proc.time()-startTime)[3])
      coefListSVM<-data.frame(array(dim=c(1,length(data$categoryLabels)+1)))
      colnames(coefListSVM)<-c("algorithm",data$categoryLabels)
      coefListSVM$algorithm <- c("svm")
      coefListSVM[1,names(coefSVMprediction)] <- coefSVMprediction
      coefList <- rbind(coefList, coefListSVM)
      timeList <- rbind(timeList, timeListSVM)
      
      #cat("Running Multinomial Logit\n", file=log)
      startTime <- proc.time()
      coefMNLprediction <- try(mnlPrediction(data), T)
      
      timeListMNL <- data.frame(algorithm=c("maxentMNL"), elapsedTime=NA)
      timeListMNL[1,2] <- round((proc.time()-startTime)[3])
      coefListMNL <-data.frame(array(dim=c(1,length(data$categoryLabels)+1)))
      colnames(coefListMNL)<-c("algorithm",data$categoryLabels)
      coefListMNL$algorithm <- c("maxentMNL")
      coefListMNL[1,names(coefMNLprediction)] <- coefMNLprediction
      coefList <- rbind(coefList, coefListMNL)
      timeList <- rbind(timeList, timeListMNL)
      
      
      #logInfo(log,timeList,header="Runtimes")
      #close(out)
      #close(log)
      list(functionName="va2",coefs=coefList,runTimes=timeList, seed=seed)
    }
    
    va2NoiseMinimization<-function(data,coef){ colMeans(getTopMinSSE(coef,getDist(data),topN=200,simN=5000,density=25)) } 
    
    va2BiasCorrection<-function(data,coef) { adjustCoef(applyBiasCorrectionFactor(getDist(data),coef)) } 
    
    va2Stacked<-function(data,Ntry=3, nsymptoms=nsymptoms){ rowMeans(replicate(Ntry,calculateRegression(getDist(data, nsymptoms=nsymptoms))),na.rm=TRUE) } 
    
    va2CentroidPrediction<-function(data) centroidClassifier(training = data$train[,-1], trainingLabels = as.matrix(data$train[,1]), test = data$test[,-1])
    #va2CentroidPrediction<-function(data)   predictUsingConfusionMatrixMultiIteration(data,centroidClassifier)
    
    va2NmPrediction<-function(data) predictUsingConfusionMatrixMultiIteration(data, nearestMeanClassifier)
    
    svmPrediction <- function(data) svmclassifier(data, "linear")
    
    mnlPrediction <- function(data) mnlclassifier(data)
    
    library(quadprog)
    
    normalizeColumnsToProbability <-function(aMatrix) sweep(data.matrix(aMatrix),2,margin.table(data.matrix(aMatrix),2),"/") 
    
    repmat<-function(x,n) matrix(rep(x,n),nrow=n,byrow=T)
    
    adjustCoef<-function(coef){ coef[coef<0]<-0.001; prop.table(coef)}
    
    logInfo<-function(log,logData,header=NULL){
      if (!is.null(header)) cat(header,"\n",file=log)
      apply(logData,1,function(x) cat(paste(x,collapse=","),"\n",file=log))
    }
    
    getSeed<-function() {
      sample(.Random.seed[-c(1:2)],1)
    }
    
    quad.constrain<-function(Y, X, W=1)
    { 
      p<-dim(X)[1]
      q<-dim(X)[2]
      ind <- matrix(0,q,2)
      rownames(ind) <- colnames(X)
      ind[,1] <- 1
      const<-1
      Y.new<-Y
      lbd <- rep(0,q)
      ubd <- rep(1,q)
      X0 <- X[,ind[,1]==1]
      if(is.null(dim(W))){
        Dmat<-t(X0)%*%X0
        dvec<-(Y.new)%*%X0
      } else {
        Dmat<-t(X0)%*%W%*%X0
        dvec<-(Y.new)%*%W%*%X0
      }
      q0<-ncol(X0)
      Amat<-matrix(0, q0,q0*2+1)
      Amat[,1]<-rep(1,q0)
      Amat[,2:(q0+1)]<-diag(1,q0)
      Amat[,(q0+2):(2*q0+1)]<-diag(-1,q0)
      bvec<-c(const, lbd[ind[,1]==1], -ubd[ind[,1]==1])
      res<-quadprog::solve.QP(Dmat,dvec, Amat, bvec, meq=1)$solution
      ind[(ind[,1]==1),2] <- res
      ind[,2]
    }
    
    getData<-function(termMatrix){
      tColSums<-colSums(termMatrix[termMatrix[,3]==1,4:ncol(termMatrix)])
      colsToRemove<-c(3+which(tColSums==0 | tColSums==nrow(termMatrix[termMatrix[,3]==1,])))
      if (length(colsToRemove)>0) termMatrix<-termMatrix[,-colsToRemove]
      tColSums<-colSums(termMatrix[termMatrix[,3]==0,4:ncol(termMatrix)])
      colsToRemove<-c(3+which(tColSums==0 | tColSums==nrow(termMatrix[termMatrix[,3]==0,])))
      if (length(colsToRemove)>0) termMatrix<-termMatrix[,-colsToRemove]
      rowsToRemove<-which(rowSums(termMatrix[,-c(1,2,3)])==0)
      if (length(rowsToRemove)>0) termMatrix<-termMatrix[-rowsToRemove,]
      trainingSet <- termMatrix[termMatrix$TRAININGSET == 1, ]
      testSet <- termMatrix[termMatrix$TRAININGSET == 0, ]
      trainingSet <- trainingSet[, -c(1,3)]
      testSet <- testSet[, -c(1,3)]
      size<-table(trainingSet[,1])
      categoryLabels<-names(size)
      #print(head(trainingSet)[,1:10])
      list(train=trainingSet,test=testSet,testSize=nrow(testSet),size=size,categoryLabels=categoryLabels)
    }
    
    getProb<-function(testSet,trainingSet){
      temp<-rbind(testSet, trainingSet)
      prob.wt<-apply(temp[,-1],2,function(x) prod(prop.table(table(x))))
      prob.wt[colSums(testSet[,-1]) == 0]<-0
      prob.wt[colSums(trainingSet[,-1])== 0]<-0
      prob.wt[colSums(testSet[,-1]) == nrow(testSet)]<-0
      prob.wt[colSums(trainingSet[,-1])== nrow(trainingSet)]<-0
      prop.table(prob.wt)
    }
    
    trainDist<-function(wmat) normalizeColumnsToProbability(table(2*wmat[,2]+wmat[,3], by=wmat[,1])) 
    
    testDist<-function(wmat) prop.table(table(2*wmat[,1]+wmat[,2]))
    
    getIndexList<-function(testSet,trainingSet,indexSize,n=2) { 
      prob=getProb(testSet,trainingSet)
      nLast<-ncol(trainingSet)
      array(replicate(indexSize,sample(2:nLast, n, prob=prob)),c(n,indexSize))
    }
    
    getDist<-function(data, stackN=500, nsymptoms=7){	
      #stackN=max(stackN,round(log(ncol(data$train,2)))
      trainingSet<-data$train # Create training set
      testSet<-data$test # save test set
      size<-data$size # Get the size of each category in the training set
      k<-length(size) # Number of categories
      indexList<-getIndexList(testSet,trainingSet,stackN, n=nsymptoms)	# 500 iterations and n words to form WSPs
      d<-rep(0,2^nsymptoms)
      names(d)<- 0:(2^nsymptoms-1)
      test<-matrix(0,(2^nsymptoms)*stackN,1)
      test[,1]<-c(apply(indexList,2,function(x) { z<-testDist(testSet[,x]);d[names(z)]<-z;d}))
      d<-matrix(0,2^nsymptoms,k)
      rownames(d)<-0:(2^nsymptoms-1)
      train<-apply(indexList,2,function(x) {z<-trainDist(trainingSet[,c(1,x)]);d[rownames(z),]<-z;list(d)})
      train<-do.call("rbind",do.call("rbind",train))
      colnames(train)<-data$categoryLabels	
      variance<-rowSums(train*(1-train)/matrix(rep(size,nrow(train)),ncol=k,byrow=T))
      train<-train[variance>0,]
      test<-test[variance>0]
      variance<-variance[variance>0]
      W<-diag(1/variance)
      list(testDist=test,trainDist=train, W=W, size=data$size, testSize=data$testSize,categoryLabels=data$categoryLabels)
    }
    
    gram.schmidt<-function(mat, orthnorm=NULL)
    {
      if (det(mat)==0) stop("mat is not full rank")
      omat<-mat
      p<-dim(omat)[1]
      for (i in 2:p){
        temp <- rep(0,p)
        for (j in 1:(i-1))
          browser()
          temp <- temp+c(t(omat[j,])%*%omat[i,])/c(t(omat[j,])%*%omat[j,])*omat[j,]
        omat[i,] <- omat[i,]-temp
      }
      if (!is.null(orthnorm)) {
        oind<-orthnorm
        for ( i in oind)
          omat[i,] <- omat[i,]/c(t(omat[i,]%*%omat[i,]))^0.5
      }
      return(omat)
    }
    
    calculateRegression<-function(dist) {
      result<-adjustCoef(quad.constrain(dist$testDist,dist$trainDist,dist$W))
      if (is.null(result)) result<-rep(NA,length(dist$categoryLabels))
      result
    }
    
    rdirichlet_va2<-function (alpha) prop.table(rgamma(length(alpha),alpha))
    getTopMinSSE<-function(coef,dist,topN=100,simN=10000,density=25){
      adjustedSSE<-rep(0,simN)
      pList<-cbind(c(1:simN),t(replicate(simN,rdirichlet_va2(density*coef))))
      colnames(pList)<-c("id",dist$categoryLabels)
      train<-dist$trainDist
      test<-dist$testDist
      N<-dist$testSize
      size<-dist$size
      var<-train*(1-train)*repmat((size-1)/size,nrow(train))
      M<-nrow(var)
      k<-ncol(var)
      z<-apply(pList,1,function(p) {
        varTotal<-repmat(p[-1]*(N*p[-1]+size)/(N*size),M)*var
        err<-sum((test-train%*%p[-1])^2)-sum(varTotal)
        adjustedSSE[p[1]]<<-err
        return(NULL)
      }
      )
      pList[order(adjustedSSE),-1][1:topN,]
    }
    
    applyBiasCorrectionFactor<-function(dist,coef){
      train<-dist$trainDist
      size<-dist$size
      k<-length(size)
      G<-diag(1, k)
      G[1,]<-rep(1/k,k)
      G<-as.matrix(gram.schmidt(G, orthnorm=2:k))
      if (k==2)  A<-t(as.matrix(G[2:k,])) else A<-G[2:k,]
      I<-matrix(0,k,k)
      diag(I)<-1
      EE<-matrix(0,nrow=k,ncol=k)
      diag(EE)<-colSums(train*(1-train)/matrix(rep(size,nrow(train)),nrow=nrow(train),byrow=T))
      XX<-t(train)%*%train
      factor<-solve(I-t(A)%*%solve(A%*% XX %*% t(A) + A%*% EE %*% t(A)) %*%A%*%(EE))
      coef<-c(factor%*%coef)
      names(coef)<-dist$categoryLabels
      coef
    }
    
    predictUsingConfusionMatrixMultiIteration <- function(data, classifierFunction,  numberOfCrossValidations=10, numberOfIterations=10) {
      colMeans(t(replicate(numberOfIterations,predictUsingConfusionMatrixSingleIteration(data, classifierFunction, numberOfCrossValidations))),na.rm=TRUE)
    }
    
    predictUsingConfusionMatrixSingleIteration <- function(data, classifierFunction, numberOfCrossValidations=10) { 
      confusionMatrix<-getConfusionMatrix(data,classifierFunction,data$categoryLabels)
      naivePredictions<-getNaivePredictions(data, classifierFunction,data$categoryLabels)
      prediction<-tryCatch(correctNaivePredictionsUsingConfusionMatrix(confusionMatrix, c(naivePredictions)),error=function(e) cat(paste(e,collapse=","),"\n"))
      if (is.null(prediction)) prediction<-rep(NA,length(data$categoryLabels))
      prediction
    }
    
    svmclassifier <- function(data, kern){
      
      # Fit SVM
      svm_out <- e1071::svm(x=data$train[,2:length(data$train[1,])], y=as.factor(data$train[,1]), kernel=kern)
      #print(svm_out)
      # Predict on the test set
      pred_out <- predict(svm_out, newdata=data$test[,2:length(data$test[1,])])
      #print(pred_out)
      prop_out <- table(pred_out)/sum(table(pred_out))
      out_vec <- vector(length=length(data$categoryLabels))
      names(out_vec) <- data$categoryLabels
      out_vec[names(prop_out)] <- prop_out
      
      return(out_vec)
      
    }
    
    mnlclassifier <- function(data){
      
      ## load mnl library
      #library2(maxent, loadin = F)
      
      # Fit MNL classifier
      #max_out <- maxent::maxent(feature_matrix=data$train[,2:length(data$train[1,])], 
                                #code_vector=as.factor(data$train[,1]))
      #max_pred <- predict(max_out, feature_matrix=data$test[,2:length(data$test[1,])])
      
      #Fit MNL classifier 
      max_pred <- predict( glmnet::cv.glmnet(x=as.matrix(data$train[,2:length(data$train[1,])]), 
              y=as.factor(data$train[,1]), nfolds = 5, 
              family = "multinomial"), 
              as.matrix(data$test[,2:length(data$test[1,])]), type = "response", s = "lambda.1se")[,,1]
      
      
      # Predict on the test set
      label_val <- colnames(max_pred)
      proportions <- apply(max_pred, 2, function(x) sum(as.numeric(x)))/sum(as.numeric(max_pred))
      proportions <- proportions/sum(proportions)
      # Proportions
      names(proportions) <- label_val
      out_vec <- vector(length=length(data$categoryLabels)) 
      names(out_vec) <- data$categoryLabels
      out_vec[names(proportions)] <- proportions
      
      return(out_vec)
    }
    
    correctNaivePredictionsUsingConfusionMatrix <- function(confusionMatrix, naivePredictions) {
      quad.constrain(naivePredictions, confusionMatrix, W=1)
    }
    
    getConfusionMatrix<-function(data, classifierFunction, categoryLabels, numberOfCrossValidations=10){
      crossValLabels <- getCrossValidationIndices(data$train$TRUTH, numberOfCrossValidations)
      numberOfCategories<-length(categoryLabels)
      confusionMatrix <- matrix(0, numberOfCategories, numberOfCategories, dimnames=list(categoryLabels,categoryLabels))
      diag(confusionMatrix) <- 1 
      for (cv in 1:numberOfCrossValidations) {
        singleCrossValidationResults<-getSingleCrossValidationResults(data,crossValLabels,cv,classifierFunction)
        if (!is.null(singleCrossValidationResults)) confusionMatrix[rownames(singleCrossValidationResults),colnames(singleCrossValidationResults)] <- confusionMatrix[rownames(singleCrossValidationResults),colnames(singleCrossValidationResults)] + singleCrossValidationResults
      }  	
      normalizeColumnsToProbability(confusionMatrix)
    }
    
    getNaivePredictions<-function(data, classifierFunction,categoryLabels){
      naivePredictionsRaw <- prop.table(table(classifierFunction(data.matrix(data$train[,-1]), data$train$TRUTH, test=as.matrix(data$test[,-1]))))
      naivePredictions<-rep(0,length(categoryLabels))
      names(naivePredictions)<- categoryLabels
      naivePredictions[names(naivePredictionsRaw)]<-naivePredictionsRaw
      naivePredictions
    }
    
    getSingleCrossValidationResults<-function(data, crossValLabels, cv,classifierFunction){
      booleanTrainingLabels<-crossValLabels!=cv
      trainingSetMat = data.matrix(data$train[,-1])
      cvTestSet = trainingSetMat[!booleanTrainingLabels,]
      if (nrow(cvTestSet) == 0)  return(NULL)
      cvTrainingSet = trainingSetMat[booleanTrainingLabels,]
      cvTruthLabels = data$train$TRUTH[booleanTrainingLabels]
      predictions <- classifierFunction(cvTrainingSet, cvTruthLabels, cvTestSet)
      data.matrix(table(pred = predictions, true = data$train$TRUTH[!booleanTrainingLabels]))
    }
    
    getCentroids <- function(training, truthLabels) {
      centroids<-rowsum(training,truthLabels)
      centroids/matrix(rep(table(truthLabels)[rownames(centroids)],ncol(centroids)),nrow=nrow(centroids),byrow=FALSE)
    }
    
    euclideanDistance<-function(x,c){
      x2 = colSums(t(x)^2)
      c2 = colSums(t(c)^2)
      return(x2 %*% matrix(1, 1, nrow(c)) + matrix(1, nrow(x), 1) %*% c2 - (2 * x %*% t(c)))
    }
    
    centroidClassifier<-function(training,trainingLabels, test,distanceMetric="cosine"){
      centroids<-as.matrix(  getCentroids(training, trainingLabels) ) 
      distances<-matrix(0,nrow(test),nrow(centroids))
      if (distanceMetric=="cosine") {
        centroids <- centroids / sqrt(rowSums(centroids*centroids))
        test<-test/sqrt(rowSums(test*test))
        #distances<- -1* test %*% t(centroids)
        distances<- -1* t(  centroids %*% t(test) ) 
        distances[is.nan(distances)]<-100000
        predictions<-apply(distances,1,function(x) sample(rep(which(x==min(x)),2),1))
        rownames(centroids)[predictions]
      } else{ 
        distances[is.nan(distances)]<-100000
        predictions <- apply(test, 1, function(da){ myD <- rowSums( (matrix(rep(f2n(da), times = nrow(centroids)), 
                                                  ncol = ncol(centroids), byrow =T)  - centroids )^2 ); 
                            which_min <- sample(which(myD == min(myD)), 1) } ) 
        rownames(centroids)[predictions]
      } 
      
      
    }
    
    nearestMeanClassifier<-function(training,truthLabels, test) centroidClassifier(training,truthLabels, test,distanceMetric="euclidean")
    
    getCrossValidationIndices <- function(truthLabels, numberOfCrossValidations=10) {
      crossValidationIndicesForAllTrainingRows = matrix(0, length(truthLabels), 1)
      for (categoryLabel in unique(truthLabels)) {
        indicesOfACategory = which(truthLabels == categoryLabel)
        categorySize<-length(indicesOfACategory)
        uniformCrossValidationIndices=c(replicate(ceiling(categorySize/numberOfCrossValidations),sample(1:numberOfCrossValidations, numberOfCrossValidations)))[1:categorySize]
        crossValidationIndicesForAllTrainingRows[sample(indicesOfACategory,categorySize)] = uniformCrossValidationIndices    
      }
      crossValidationIndicesForAllTrainingRows
    }
    
    testResult<-try(ensemble_VA2(termMatrix,seed,nsymptoms=nsymps), T)
    if (is.null(testResult)) {
      y<-termMatrix$TRUTH
      trainingIndices<-which(termMatrix$TRAININGSET==1)
      categoryLabels<-sort(unique(y[trainingIndices]))
      coefList<-data.frame(array(dim=c(1,length(categoryLabels)+1)))
      testIndices<- which(termMatrix$TRAININGSET==0)
      colnames(coefList)<-c("algorithm",categoryLabels)
      coefList[1,2]<--1
      coefList[1,3]<--1
      coefList[1,1]<-"va2-error"
      timeList<-data.frame(algorithm=c("va2-error"),elapsedTime=-1)
      return(list(functionName="va2-error", coefs=coefList,runTimes=timeList, seed=NA))
    } else return(testResult$coefs[6,-1])
    
  }
