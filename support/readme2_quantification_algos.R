if(T == T){ 
  #Optimization Functions
  quad.constrain_QUANT<-function(Y,X){ 
    require(quadprog)
    K<-ncol(X)
    Amat<-t(rbind(rep(1,K),diag(1,K),diag(-1,K)))
    quadprog::solve.QP(t(X)%*%X,matrix(Y,nrow=1)%*%X, Amat, c(1, rep(0,K), rep(-1,K)), meq=1)$solution
  }
  
  hellinger_QUANT<-function(Y,X, start=NULL){
    K<-ncol(X)
    if (is.null(start)) start = rep(1/K,K)
    Aeq=matrix(1,ncol=K,nrow=1)
    Beq=matrix(1,1,1)
    conf<-function(x) return(list(ceq=NULL,c=NULL))	
    objfun<-function(beta){
      beta <- 1/(1+exp(-beta))
      beta <- beta/sum(beta)
      Yh<-(X%*%beta);Yh[Yh<0]<-0;
      1-sum(sqrt(Y*Yh))}
    #library2(NlcOptim);objfun<-function(beta){Yh<-(X%*%beta);Yh[Yh<0]<-0;1-sum(sqrt(Y*Yh))}
    #ret_v <- as.numeric(NlcOptim::NlcOptim(X=start, objfun=objfun,Aeq=Aeq,Beq=Beq, lb=matrix(0,K,1), ub=matrix(1,K,1),confun=conf)$p)
    ret_v <- try(as.numeric(stats::optim(par = rep(0, times = K), fn = objfun)$par), T) 
    ret_v <- try(1/(1+exp(-ret_v)), T); ret_v <- try(ret_v/sum(ret_v), T) 
    return( ret_v )  
  }
  
  L1_QUANT<-function(Y,X,start=NULL){
    K<-ncol(X)
    if (is.null(start)) start = rep(1/K,K)
    Aeq=matrix(1,ncol=K,nrow=1)
    Beq=matrix(1,1,1)
    conf<-function(x) return(list(ceq=NULL,c=NULL))
    #objfun<-function(beta){q<-X%*%beta; q[q<0]<-0; p<-Y; sum(abs(p-q))}
    #requireNlcOptim)
    #as.numeric(NlcOptim::NlcOptim(X=matrix(start,K,1), objfun=objfun,Aeq=Aeq,Beq=Beq, lb=matrix(0,K,1), ub=matrix(1,K,1),confun=conf)$p)
    objfun<-function(beta){
      beta <- 1/(1+exp(-beta))
      beta <- beta/sum(beta)
      q<-(X%*%beta);q[q<0]<-0;
      p<-Y; sum(abs(p-q))}
    ret_v <- try(as.numeric(optim(par = rep(0, times = K), fn = objfun)$par), T) 
    ret_v <- try(1/(1+exp(-ret_v)), T); ret_v <- try(ret_v/sum(ret_v), T) 
    
  }
  
  LAD_QUANT<-function(Y,X){
    M<-nrow(X)
    K<-ncol(X)
    I<-diag(1,M,M)
    Amat<-rbind(cbind(I,X),cbind(I,-X),c(rep(0,M), rep(1,K)))
    bvec<-c(Y,-Y,1)
    cvec<-c(rep(1,M), rep(0,K))
    requirelinprog)
    l<-linprog::solveLP(cvec,bvec,Amat,const.dir=c(rep(">=",2*M),"=="),lpSolve=T)
    as.numeric(tail(l$solution[],K))
  }
  
  #Base classifiers
  svmModelFunction_QUANT<-function(xTrain){
    e1071::svm(xTrain[,-1],xTrain[,1],probability=TRUE, kernel="linear", type="C-classification")
  }
  
  svmPredFunction_QUANT<-function(PLT, testData){
    pred<-predict(PLT,testData,probability=TRUE); 
    probs<-attr(pred,"probabilities")
    probs[,order(as.numeric(colnames(probs)))]
    
  }
  
  liblinearModelFunction<-function(xTrain){
    LiblineaR::LiblineaR(data=xTrain[,-1],target=xTrain[,1], type=7,cost=1, bias=TRUE)
  }
  
  liblinearPredFunction<-function(PLT, testData){
    probs<-predict(PLT,testData, proba=T)$probabilities
    probs[,order(as.numeric(colnames(probs)))]
  }
  
  # methods
  va1<-function(trainMatrix, testMatrix, trials=300, n=15){
    bintodec<-function(x){
      sum(x * 2^ ((length(x)-1):0))
    }
    
    getProb <- function(testSet, trainingSet) {
      temp <- rbind(testSet, trainingSet)
      prob.wt <- apply(temp[, -1], 2, function(x) prod(prop.table(table(x))))
      prob.wt[colSums(testSet[, -1]) == 0] <- 0
      prob.wt[colSums(trainingSet[, -1]) == 0] <- 0
      prob.wt[colSums(testSet[, -1]) == nrow(testSet)] <- 0
      prob.wt[colSums(trainingSet[, -1]) == nrow(trainingSet)] <- 0
      prop.table(prob.wt)
    }
    K<-length(table(trainMatrix[,1]))
    resList<-NULL
    prob=getProb(testMatrix,trainMatrix)
    
    XL<-NULL
    YL<-NULL
    
    for (i in 1:trials){
      cols<-sample(2:ncol(trainMatrix),n,prob=prob)
      XR<-NULL
      profilesTest<-apply(testMatrix[,cols],1,bintodec)
      profilesTrain<-apply(trainMatrix[,cols],1,bintodec)		
      profiles<-unique(union(profilesTest,profilesTrain))
      Y<-prop.table(table(factor(profilesTest, levels=profiles)))
      for (j in 1:K)
        XR<-cbind(XR,prop.table(table(factor(profilesTrain[trainMatrix[,1]==j], levels=profiles))))
      XL<-rbind(XL,XR)
      YL<-c(YL,Y)
    }
    tryCatch(quad.constrain_QUANT(YL,XL),error = function(e) cat(paste(e, collapse = ","), "\n"))
  }
  
  friedman<-function(testPred,trainingFractions,cvPred, cvPredY){
    K<-ncol(testPred)
    
    fF<-matrix(0,nrow(testPred),K)
    for (i in 1:K) fF[,i]<-testPred[,i]>=trainingFractions[i]
    
    flF<-rep(0,K)
    for (i in 1:K) flF[i]<-mean(fF[,i],na.rm=T)			
    
    fA<-matrix(0,nrow(cvPred),K)
    for (i in 1:K) fA[,i]<-cvPred[,i]>=trainingFractions[i]
    
    flA<-matrix(0,K,K)
    for (l in 1:K) 
      for (k in 1:K) 
        flA[l,k]<-mean(fA[cvPredY==k,l],na.rm=T)
    flA<-flA+matrix(rnorm(nrow(flA)*ncol(flA),sd=0.0001),nrow(flA),ncol(flA))
    
    tryCatch(quad.constrain_QUANT(flF,flA),error = function(e) cat("friedman  warning\n"))
    
  }
  
  probAveraging<-function(testPred,cvPred, cvPredY){
    K<-ncol(testPred)
    
    Y<-rep(0,K)
    for (i in 1:K) Y[i]<-mean(testPred[,i])		
    
    X<-matrix(0,K,K)
    for (l in 1:K) 
      for (k in 1:K) 
        X[l,k]<-mean(cvPred[which(cvPredY==k),l],na.rm=T)
    X<-X+matrix(rnorm(K*K,sd=0.0001),K,K)
    
    tryCatch(quad.constrain_QUANT(Y,X),error = function(e)  cat("probAveraging warning\n"))
  }
  
  mixtureL2<-function(testPred,cvPred, cvPredY,numberOfBins=15){
    K<-ncol(testPred)
    
    binProbs=seq(0, 1, by=1/numberOfBins)
    X<-matrix(0,(numberOfBins)*K,K)
    Y<-matrix(0,(numberOfBins)*K,1)
    
    for (k in 1:K){
      binNumsTrain<-findInterval(cvPred[,k],binProbs,rightmost.closed=T)
      binNumsTest<-findInterval(testPred[,k], binProbs,rightmost.closed=T)
      for (ki in 1:numberOfBins){
        Y[(k-1)*numberOfBins+ki]<-mean(binNumsTest<=ki,na.rm=T)
        for (j in 1:K) X[(k-1)*numberOfBins+ki,j]<-mean(binNumsTrain[cvPredY==j]<=ki,na.rm=T)
      }
    }
    X<-X+matrix(rnorm(numberOfBins*K*K,sd=0.0001),numberOfBins*K,K)
    tryCatch(quad.constrain_QUANT(Y,X),error = function(e)  cat("mixtureLS warning\n"))
  }
  
  
  hellinger_QUANTPMF<-function(testPred,cvPred, cvPredY,start=NULL,numberOfBins=15){
    K<-ncol(testPred)
    
    binProbs=seq(0, 1, by=1/numberOfBins)
    X<-matrix(0,(numberOfBins)*K,K)
    Y<-matrix(0,(numberOfBins)*K,1)
    
    for (k in 1:K){
      binNumsTrain<-findInterval(cvPred[,k],binProbs,rightmost.closed=T)
      binNumsTest<-findInterval(testPred[,k], binProbs,rightmost.closed=T)
      for (ki in 1:numberOfBins){
        Y[(k-1)*numberOfBins+ki]<-mean(binNumsTest==ki,na.rm=T)
        for (j in 1:K)
          X[(k-1)*numberOfBins+ki,j]<-mean(binNumsTrain[cvPredY==j]==ki,na.rm=T)
      }
    }
    X<-X+matrix(rnorm(numberOfBins*K*K,sd=0.0001),numberOfBins*K,K)
    
    res<-tryCatch(hellinger_QUANT(Y,X, start),error = function(e) cat("hellinger_QUANTPMF warning\n"))
    if (!is.null(res)) return(res)
    res<-tryCatch(hellinger_QUANT(Y,X),error = function(e) cat("hellinger_QUANTPMF warning\n"))
    if (!is.null(res)) return(res)
    rdirichlet_QUANT <- function(alpha){prop.table(stats::rgamma(length(alpha), alpha))}
    for (i in 1:30){
      res<-tryCatch(hellinger_QUANT(Y,X, start=rdirichlet_QUANT(rep(1,K))),error = function(e) {})
      if (!is.null(res)) {
        cat("Success!\n")
        return(res)
      }
    }
  }
  
  hdx<-function(trainMatrix, testMatrix,start=NULL){
    K<-length(table(trainMatrix[,1]))
    
    X<-NULL
    Y<-NULL
    
    for (i in 1:(ncol(trainMatrix)-1)){
      Y<-c(Y,prop.table(table(factor(testMatrix[,i+1],levels=c(0,1)))))
      XRow<-NULL
      for (j in 1:K)
        XRow<-cbind(XRow,prop.table(table(factor(trainMatrix[trainMatrix[,1]==j,i+1],levels=c(0,1)))))
      X<-rbind(X,XRow)
    }
    
    res<-tryCatch(hellinger_QUANT(Y,X,start),error = function(e) cat("hdx warning\n"))
    if (!is.null(res)) return(res)
    res<-tryCatch(hellinger_QUANT(Y,X),error = function(e) cat("hdx warning\n"))
    if (!is.null(res)){ return(res) } 
    rdirichlet_QUANT <- function(alpha){prop.table(stats::rgamma(length(alpha), alpha))}
    for (i in 1:30){
      res<-tryCatch(hellinger_QUANT(Y,X, start=rdirichlet_QUANT(rep(1,K))),error = function(e) {})
      if (!is.null(res)) {
        cat("Success!\n")
        return(res)
      }
    }
  }
  
  adjCount<-function(testPred,cvPred, cvPredY){
    K<-ncol(testPred)
    
    fF<-matrix(0,nrow(testPred),K)
    for (i in 1:K) fF[,i]<-testPred[,i]>=apply(testPred,1,max)
    
    flF<-rep(0,K)
    for (i in 1:K) flF[i]<-mean(fF[,i],na.rm=T)			
    
    fA<-matrix(0,nrow(cvPred),K)
    for (i in 1:K) fA[,i]<-cvPred[,i]>=apply(cvPred,1,max)
    
    flA<-matrix(0,K,K)
    for (l in 1:K) 
      for (k in 1:K) 
        flA[l,k]<-mean(fA[cvPredY==k,l],na.rm=T)
    flA<-flA+matrix(rnorm(nrow(flA)*ncol(flA),sd=0.0001),nrow(flA),ncol(flA))
    
    tryCatch(quad.constrain_QUANT(flF,flA),error = function(e) cat("adjCount  warning\n"))	
  }
  
  medianSweep<-function(testPred,cvPred, cvPredY){
    K<-ncol(testPred)
    
    binSize<-100
    betas<-array(0,c(K,binSize))
    thresholds<-seq(0,1-1/binSize,1/binSize)
    counter<-1
    
    for (threshold in thresholds){
      fF<-matrix(0,nrow(testPred),K)
      for (i in 1:K) fF[,i]<-testPred[,i]>=threshold
      
      flF<-rep(0,K)
      for (i in 1:K) flF[i]<-mean(fF[,i],na.rm=T)			
      
      fA<-matrix(0,nrow(cvPred),K)
      for (i in 1:K) fA[,i]<-cvPred[,i]>=threshold
      
      flA<-matrix(0,K,K)
      for (l in 1:K) 
        for (k in 1:K) 
          flA[l,k]<-mean(fA[cvPredY==k,l],na.rm=T)
      flA<-flA+matrix(rnorm(nrow(flA)*ncol(flA),sd=0.0001),nrow(flA),ncol(flA))
      res<-tryCatch(quad.constrain_QUANT(flF,flA),error = function(e) cat("medianSweep  warning\n"))
      if (!is.null(res)) betas[,counter]<-res
      else betas[,counter]<-rep(NA,K)
      counter<-counter+1
    }
    betas
  }
  
  mixtureL1_QUANT<-function(testPred,cvPred, cvPredY,numberOfBins=15){
    K<-ncol(testPred)
    binProbs=seq(0, 1, by=1/numberOfBins)
    X<-matrix(0,(numberOfBins)*K,K)
    Y<-matrix(0,(numberOfBins)*K,1)
    
    for (k in 1:K){
      binNumsTrain<-findInterval(cvPred[,k],binProbs,rightmost.closed=T)
      binNumsTest<-findInterval(testPred[,k], binProbs,rightmost.closed=T)
      for (ki in 1:numberOfBins){
        Y[(k-1)*numberOfBins+ki]<-mean(binNumsTest<=ki,na.rm=T)
        for (j in 1:K) X[(k-1)*numberOfBins+ki,j]<-mean(binNumsTrain[cvPredY==j]<=ki,na.rm=T)
      }
    }
    X<-X+matrix(rnorm(numberOfBins*K*K,sd=0.0001),numberOfBins*K,K)
    X[X<0]<-0
    X[X>1]<-1
    Y[Y<0]<-0
    Y[Y>1]<-1
    #tryCatch(LAD_QUANT(Y,X),error = function(e)  cat("mixtureL1_QUANT LAD_QUANT warning\n"))
    tryCatch(L1_QUANT(Y,X),error = function(e)  cat("mixtureL1_QUANT L1_QUANT warning\n"))
  }
  
  naiveMethod_QUANT<-function(trainMatrix,testMatrix,baseClassifierModelFunction=liblinearModelFunction,baseClassifierPredFunction=liblinearPredFunction){
    model<-baseClassifierModelFunction(trainMatrix)
    probs<-baseClassifierPredFunction(model,testMatrix[,-1])
    probs<-probs[,order(as.numeric(colnames(probs)))]
    colMeans(probs)
  }
  
  getNaiveProbs_QUANT<-function(trainMatrix,testMatrix,baseClassifierModelFunction=liblinearModelFunction,baseClassifierPredFunction=liblinearPredFunction){
    model<-baseClassifierModelFunction(trainMatrix)
    probs<-baseClassifierPredFunction(model,testMatrix[,-1])
    probs[,order(as.numeric(colnames(probs)))]
  }
  
  #support functions
  repmat_QUANT<-function(x, n) matrix(rep(x, n), nrow = n, byrow = T)
  
  cvRandomHalfSplit_QUANT<-function(trainMatrix){
    trainMatrix<-trainMatrix[sample(1:nrow(trainMatrix),nrow(trainMatrix)),] 
    trainIndices<-tapply(1:nrow(trainMatrix), list(trainMatrix[,1]), function(s) s)
    cvTrainIndices<-unlist(lapply(trainIndices,function(x) x[1:(length(x)%/%2)]))
    cvTestIndices<-setdiff(unlist(trainIndices),cvTrainIndices)
    list(cvTrain=trainMatrix[cvTrainIndices,],cvTest=trainMatrix[cvTestIndices,])
  }
  
  getMean_QUANT<-function(classMap,betas){
    if (length(betas)<3) return(betas)
    colnames(betas)<-classMap	
    colMeans(betas,na.rm=T)
  }
  
  getMedian_QUANT<-function(classMap,betas){
    if (length(betas)<3) return(betas)
    colnames(betas)<-classMap	
    apply(betas,2,median,na.rm=T)
  }
  
  quantifyAll_QUANT<-function(trainMatrix, testMatrix, baseClassifierModelFunction=liblinearModelFunction,
                              baseClassifierPredFunction=liblinearPredFunction, trials=50){
    testMatrix_orig <- testMatrix
    trainMatrix_orig <- trainMatrix
    classMap <- levels(as.factor(trainMatrix[,1]))
    temp <- as.integer( factor(c(trainMatrix[,1],testMatrix[,1])) ) 
    trainMatrix[,1]<- temp[1:nrow(trainMatrix)]
    testMatrix[,1]<- temp[-c(1:nrow(trainMatrix))]
    testMatrix <- apply(testMatrix, 2, f2n)
    trainMatrix <- apply(trainMatrix, 2, f2n)
    K<-length(classMap)
    friedmanBetas<-probAveragingBetas<-mixtureL2Betas<-mixtureL1_QUANTBetas<-mixtureHPMFBetas<-adjCountBetas<-NULL
    medianSweepBetas<-array(0,c(trials,K,100))
    train_pd_init <- table( trainMatrix[,1] )  
    actual <- train_pd_init; actual[] <- 0 
    temp <- table( testMatrix[,1] )  
    actual[names(temp)] <- temp
    actual <- prop.table(actual)
    for (trial in 1:trials){
      cvSplits<-try(cvRandomHalfSplit_QUANT(trainMatrix), T) 
      model<-try(baseClassifierModelFunction(cvSplits$cvTrain), T) 
      cvPred<-try(baseClassifierPredFunction(model,cvSplits$cvTest[,-1]), T) 
      testPred<-try(baseClassifierPredFunction(model,testMatrix[,-1]), T)  
      trainingFractions<-try(prop.table(table(cvSplits$cvTrain[,1])), T) 
      cvPredY<-try(cvSplits$cvTest[,1], T)  
      
      res<-try(friedman(testPred,trainingFractions,cvPred, cvPredY), T) 
      if(class(res) != "try-error" & !is.null(res)) friedmanBetas<-rbind(friedmanBetas, res)

      res<-try(probAveraging(testPred,cvPred, cvPredY), T) 
      if(class(res) != "try-error" & !is.null(res)) probAveragingBetas<-rbind(probAveragingBetas, res)
      
      res<-try(mixtureL2(testPred,cvPred, cvPredY), T)  
      if(class(res) != "try-error" & !is.null(res)) mixtureL2Betas<-rbind(mixtureL2Betas, res)
      
      res<-try(mixtureL1_QUANT(testPred,cvPred, cvPredY), T) 
      if(class(res) != "try-error" & !is.null(res)) mixtureL1_QUANTBetas<-rbind(mixtureL1_QUANTBetas, res)
      
      res<-try(hellinger_QUANTPMF(testPred,cvPred, cvPredY,actual), T)  
      if(class(res) != "try-error" & !is.null(res)) mixtureHPMFBetas<-rbind(mixtureHPMFBetas, res)
      
      res<-try(medianSweep(testPred,cvPred, cvPredY), T)  
      if(class(res) != "try-error" & !is.null(res)) medianSweepBetas[trial,,]<-res
      
      res<-try(adjCount(testPred,cvPred, cvPredY), T)  
      if(class(res) != "try-error" & !is.null(res)) adjCountBetas<-rbind(adjCountBetas, res)
    }
    betaPerThreshold<-try(apply(medianSweepBetas,3,colMeans,na.rm=T), T)  
    medianSweep<-try(prop.table(matrix(apply(betaPerThreshold,1,median,na.rm=T),ncol=K)), T)  
    
    dt<-data.frame(matrix(0,11,K+1))
    dt[,1]<-c("friedman","prob","mixtureL2","mixtureL1_QUANT","mixtureHPMF","adjCount","medianSweep","hdx","va1","actual","naive")
    
    q1 <- try(getMean_QUANT(classMap,friedmanBetas), T)  
    if(class(q1) == "try-error" | is.null(q1)){q1 <- rep(NA, times = length(dt[1,-1]))}
    dt[1,-1]<-q1
    
    q1 <- try(getMean_QUANT(classMap,probAveragingBetas), T)  
    if(class(q1) == "try-error" | is.null(q1)){q1 <- rep(NA, times = length(dt[1,-1]))}
    dt[2,-1]<-q1
    
    q1 <- try(getMean_QUANT(classMap,mixtureL2Betas), T)  
    if(class(q1) == "try-error" | is.null(q1)){q1 <- rep(NA, times = length(dt[1,-1]))}
    dt[3,-1]<-q1
    
    q1 <- try(getMean_QUANT(classMap,mixtureL1_QUANTBetas), T)  
    if(class(q1) == "try-error" | is.null(q1)){q1 <- rep(NA, times = length(dt[1,-1]))}
    dt[4,-1]<-q1
    
    q1 <- try(getMean_QUANT(classMap,mixtureHPMFBetas), T)  
    if(class(q1) == "try-error" | is.null(q1)){q1 <- rep(NA, times = length(dt[1,-1]))}
    dt[5,-1]<-q1
    
    q1 <- try(getMean_QUANT(classMap,adjCountBetas), T)  
    if(class(q1) == "try-error" | is.null(q1)){q1 <- rep(NA, times = length(dt[1,-1]))}
    dt[6,-1]<-q1
    
    if(class(medianSweep) == "try-error" | is.null(medianSweep)){medianSweep <- rep(NA, times = length(dt[1,-1]))}
    dt[7,-1]<-medianSweep
    
    q1 <- try(hdx(trainMatrix,testMatrix,start=actual), T)  
    if(class(q1) == "try-error" | is.null(q1)){q1 <- rep(NA, times = length(dt[1,-1]))}
    dt[8,-1]<-q1
    
    q1 <- try(va1(trainMatrix,testMatrix,n=4), T)  
    if(class(q1) == "try-error" | is.null(q1)){q1 <- rep(NA, times = length(dt[1,-1]))}
    dt[9,-1]<-q1
    
    dt[10,-1]<-actual
    
    q1 <- try(naiveMethod_QUANT(trainMatrix,testMatrix), T)  
    if(class(q1) == "try-error" | is.null(q1)){q1 <- rep(NA, times = length(dt[1,-1]))}
    dt[11,-1]<-q1
    colnames(dt)<-c("alg",classMap)
    return(  dt )  
  }
  getPrunedSize<-function (coef, size, minN = 0) {
    max.index <- which.max(coef)
    max.value <- max(coef)
    prunedSize <- round(coef * (size[max.index]/max.value))
    removalCounter <- 1
    while (any(((size - prunedSize)/max.value) < -0.01)) {
      prunedSize <- round(coef * ((size[max.index] - removalCounter)/max.value))
      removalCounter <- removalCounter + 1
    }
    prunedSize[prunedSize < min(size, minN)] <- min(size, minN)
    prunedSize
  }
  
  getErrorRate<-function(truth, probs, corrections=NULL, trainingFractions=NULL){
    if (is.null(corrections)){
      return(1-mean(truth==apply(probs,1,which.max)))
    } else {
      return(1-mean(apply(apply(repmat_QUANT(as.numeric(corrections)/as.numeric(trainingFractions),nrow(probs))*probs,1,prop.table),2,which.max)==truth))
    }
  }
} 
