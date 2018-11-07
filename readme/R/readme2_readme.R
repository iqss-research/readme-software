#' readme
#' 
#' Implements the quantification algorithm described in Jerzak, King, and Strezhnev (2018) which is meant to improve on the ideas in Hopkins and King (2010).
#' Employs the Law of Total Expectation in a feature space that is tailoed to minimize the error of the resulting estimate. 
#' Automatic differentiation, stochastic gradient descent, and batch re-normalization are used to carry out the optimization.
#' Takes an inputs (a.) a vector holding the raw documents (1 entry = 1 document), (b.) a vector indicating category membership 
#' (with \code{NA}s for the unlabeled documents), and (c.) a vector indicating whether the labeled or unlabeled status of each document. 
#' Other options exist for users wanting more control over the pre-processing protocol (see \code{undergrad} and the \code{dfm} parameter).
#' 
#' @param dfm 'document-feature matrix'. A data frame where each row represents a document and each column a unique feature. 
#'
#' @param labeledIndicator An indicator vector where each entry corresponds to a row in \code{dfm}. 
#' \code{1} represents document membership in the labeled class. \code{0} represents document membership in the unlabeled class. 
#' 
#' @param categoryVec An factor vector where each entry corresponds to the document category. 
#' The entires of this vector should correspond with the rows of \code{dtm}. If \code{wordVecs_corpus}, \code{wordVecs_corpusPointer}, and \code{dfm} are all \code{NULL}, 
#' \code{readme} will download and use the \code{GloVe} 50-dimensional embeddings trained on Wikipedia. 
#'
#' @param nboot A scalar indicating the number of times the estimation procedure will be re-run (useful for reducing the variance of the final output).
#'
#' @param verbose Should progress updates be given? Input should be a Boolean. 
#' 
#' @param diagnostics Should diagnostics be returned? Input should be a Boolean. 
#'  
#' @param sgd_iters How many stochastic gradient descent iterations should be used? Input should be a positive number.   
#' 
#' @param sgd_momentum Momentum parameter for stochastic gradient descent (default = 0.90)
#'  
#' @param justTransform A Boolean indicating whether the user wants to extract the quanficiation-optimized 
#' features only. 
#' 
#' @param numProjections How many projections should be calculated? Input should be a positive number. Minimum number of projections = number of categories + 2. 
#' 
#' @param mLearn Learning parameter for moments in batch normalization (numeric value from 0-1). Default to 0.01 
#' 
#' @param minBatch What should the minimum per category batch size be in the sgd optimization? Input should be a positive number.   
#' 
#' @param maxBatch What should the maximum per category batch size be in the sgd optimization? Input should be a positive number.   
#' 
#' @param dropout_rate What should the dropout rate be in the sgd optimization? Input should be a positive number.   
#' 
#' @param kMatch What should k be in the k-nearest neighbor matching? Input should be a positive number.   
#' 
#' @param nBoot_matching How many times should matching with resampling be done?  Input should be a positive number.   
#' 
#' @param winsorize Should columns of the raw \code{dfm} be Windorized? 
#' 
#' @return A list consiting of \itemize{
#'   \item estimated category proportions in the unlabeled set (\code{point_readme});
#'   \item the transformed dfm optimized for quantification (\code{transformed_dfm}); 
#'   \item (optional) a list of diagnostics (\code{diagnostics}); 
#' }
#'
#' @section References:
#' \itemize{ 
#' \item Hopkins, Daniel, and King, Gary (2010), 
#' \emph{A Method of Automated Nonparametric Content Analysis for Social Science},
#' \emph{American Journal of Political Science}, Vol. 54, No. 1, January 2010, p. 229-247. 
#' \url{https://gking.harvard.edu/files/words.pdf} 
#' 
#' \item Jerzak, Connor, King, Gary, and Strezhnev, Anton. Working Paper. 
#' \emph{An Improved Method of Automated Nonparametric Content Analysis for Social Science}. 
#' \url{https://gking.harvard.edu/words} 
#' }
#' 
#' @examples 
#' #set seed 
#' set.seed(1)
#' 
#' #Generate synthetic 25-d word vector corpus. 
#' my_wordVecs <- matrix(rnorm(11*25), ncol = 25)
#' row.names(my_wordVecs) <- c("the","true", "thine", "stars", "are" , "fire", ".", "to", "own", "self", "be")
#' 
#' #Generate 100 ``documents'' of between 5-10 words each. 
#' my_documentText <- replicate(100, paste(sample(row.names(my_wordVecs), sample(5:10, 1), replace = T), collapse = " ") ) 
#' 
#' #Assign labeled/unlabeled sets. The first 50 will be labeled; the rest unlabeled. 
#' my_labeledIndicator <- rep(1, times = 100)
#' my_labeledIndicator[51:100] <- 0
#' 
#' #Assign category membership randomly 
#' my_categoryVec <- sample(c("C1", "C2", "C3", "C4"), 100, replace = T)
#' true_unlabeled_pd <- prop.table(table(my_categoryVec[my_labeledIndicator==0]))
#' my_categoryVec[my_labeledIndicator == 0] <- NA
#' 
#' #Get word vector summaries 
#' my_dfm <- undergrad(documentText = my_documentText, wordVecs = my_wordVecs)
#' 
#' #perform estimation
#' readme_results <- readme(dfm = my_dfm,  
#'                          labeledIndicator = my_labeledIndicator, 
#'                          categoryVec = my_categoryVec, 
#'                          nboot = 2)
#'print(readme_results$point_readme)
#'
#' @export 
#' @import tensorflow
readme <- function(dfm, labeledIndicator, categoryVec, 
                   nboot = 10,  sgd_iters = 1000, sgd_momentum = .9, numProjections = 20, minBatch = 3, maxBatch = 20, mLearn= 0.01, dropout_rate = .5, kMatch = 3, minMatch = 2, nBoot_matching = 20,
                   verbose = F, diagnostics = F, justTransform = F, winsorize=T){ 
  
  ## Get summaries of all of the document characteristics and labeled indicator
  nDocuments <- nrow(dfm)
  nFeat <- ncol(dfm)
  nLabeled <- sum(labeledIndicator == 1)
  nUnlabeled <- sum(labeledIndicator == 0)
  labeledCt <- table(categoryVec[labeledIndicator == 1])

  ### Sanity checks
  if (nDocuments != nLabeled + nUnlabeled){
    stop("Error: 'dfm' must have the same number of rows as the length of 'labeledIndicator'")
  }
  if (nDocuments != length(categoryVec)){
    stop("Error: 'dfm' must have the same number of rows as the length of 'categoryVec'")
  }
  
  if (mLearn <= 0 | mLearn > 1){
    stop("Error: 'mLearn' must be greater than 0 and less than 1")
  }
  
  if (verbose == T){
    if (kMatch == 0){
      cat("Note: 'kmatch' set to 0, skipping matching procedure for the labeled set")
    }
  }
  
  ## Print a summary of the input data
  if (verbose == T){
    cat("Data summary:\n")
    cat(paste("Number of labeled documents: ", nLabeled, "\n", sep=""))
    cat(paste("Number of unlabeled documents: ", nUnlabeled, "\n", sep=""))
    cat(paste("Number of features: ", nFeat, "\n", sep=""))   
    cat(paste("Count of documents in each category in the labeled set:\n"))
    print(labeledCt)
  }
  
  ## Generic winsorization function 
  r_clip_by_value = function(x, a, b){x[x<=a] <- a;x[x>=b] <- b;return(x)}
  
  # Winsorize the columns of the document-feature matrix
  if(winsorize == T){
    dfm <- apply(dfm, 2, function(x){ 
      sum_x <- summary(x); qr_ <- 1.5*diff(sum_x[c(2,5)]);
      x[x < sum_x[2]- qr_] <-sum_x[2]- qr_; x[x > sum_x[5]+qr_] <- sum_x[5] + qr_; 
      return( x ) })
  }
  ## Drop invariant columns
  dfm <- dfm[,apply(dfm,2,sd)>0]
  
  #Setup information for SGD
  categoryVec_unlabeled <- as.factor(categoryVec)[labeledIndicator == 0]  
  categoryVec_labeled <- as.factor(categoryVec)[labeledIndicator == 1]
  labeled_pd <- vec2prob( categoryVec_labeled ); unlabeled_pd <- vec2prob( categoryVec_unlabeled )
  dfm_labeled <- dfm[labeledIndicator==1,]; dfm_unlabeled <- dfm[labeledIndicator==0,]
  nCat <- as.integer( length(labeled_pd) ); nDim <- as.integer( ncol(dfm_labeled) )  #nDim = Number of features total

  #Parameters for Batch-SGD
  NObsByCat = rep(min(r_clip_by_value(as.integer( round( sqrt(  nrow(dfm_labeled)*labeled_pd))),minBatch,maxBatch)), nCat) ## Number of observations to sample per category
  nProj <- as.integer(max(numProjections,nCat+2) ); ## Number of projections
  
  #Start SGD
  if (verbose == T){
    cat("Initializing TensorFlow session\n")
  }
  # Initialize tensorflow
  tf$reset_default_graph(); sess <- tf$Session()
  
  ## Construct TensorFlow graph
  if (verbose == T){
    cat("Constructing TensorFlow graph\n")
    cat(paste("Number of feature projections: ", nProj, "\n", sep=""))
  }
  
  ## For calculating discrimination - how many possible cross-category contrasts are there
  contrasts_mat <-  combn(1:nCat, 2) - 1; contrast_indices1 <- as.integer(contrasts_mat[1,]); contrast_indices2 <- as.integer(contrasts_mat[2,])
  
  ## For calculating feature novelty - how many possible cross-feature contrasts are there
  redund_mat <- combn(1:nProj, 2) - 1; redund_indices1 <- as.integer(redund_mat[1,]); redund_indices2 <- as.integer(redund_mat[2,])
  axis_FeatDiscrim = as.integer(nCat!=2)
    
  #Placeholder settings - to be filled when executing TF operations
  sdg_learning_rate = tf$placeholder(tf$float32, shape = c()) ## Placeholder for learning rate
  
  dmax = tf$placeholder(tf$float32, shape = c()); rmax = tf$placeholder(tf$float32, shape = c())
  NObsByCat_cumsum = cumsum(NObsByCat) ## Cumulative sum of the number of observations to use per category when carrying out SGD optimization

  ## Transformation matrix from features to E[S|D] (urat determines how much smoothing we do across categories)
  MultMat = t(do.call(rbind,sapply(1:nCat,function(x){
    urat = 0.005; uncertainty_amt = urat / ( (nCat - 1 ) * urat + 1  );
    MM = matrix(uncertainty_amt, nrow = NObsByCat[x],ncol = nCat); MM[,x] = 1-(nCat-1)*uncertainty_amt
    return( list(MM) )  } )) )
  MultMat = MultMat  / rowSums( MultMat )
  MultMat_tf = tf$constant(MultMat, dtype = tf$float32)
  
  ## Which indices in the labeled set are associated with each category
  list_indices_by_cat <- tapply(1:length(categoryVec_labeled), categoryVec_labeled, c)
    
  #SET UP INPUT layer to TensorFlow
  IL_input <-  tf$placeholder(tf$float32, shape = list(sum(NObsByCat), nDim))
  IL = IL_input 
  { #batch renormalization for the input layer
    IL_m = tf$nn$moments(IL, axes = 0L);
    IL_mu_b = IL_m[[1]];
    IL_sigma2_b = IL_m[[2]];
    IL_sigma_b = tf$sqrt(IL_sigma2_b)
    IL_mu_last = tf$placeholder( tf$float32,shape(dim(IL_mu_b)) )
    IL_sigma_last = tf$placeholder( tf$float32,shape(dim(IL_sigma_b)) )
    IL_n =  tf$nn$batch_normalization(IL, mean = IL_mu_b, variance = IL_sigma2_b, offset = 0, scale = 1, variance_epsilon = 0.001)
  } 
  OUTPUT_IL = tf$placeholder(tf$float32, shape = list(NULL, nDim))
  OUTPUT_IL_n =  tf$nn$batch_normalization(OUTPUT_IL, mean = IL_mu_last,variance = tf$square(IL_sigma_last), offset = 0, scale = 1, variance_epsilon = 0)
  
  #SET UP WEIGHTS to be optimized
  WtsMat = tf$Variable(tf$random_uniform(list(nDim,nProj),-1/sqrt(nDim+nProj), 1/sqrt(nDim+nProj)),dtype = tf$float32, trainable = T)
  BiasVec = tf$Variable(as.vector(rep(0,times = nProj)), trainable = T, dtype = tf$float32)
  
  ### Drop-out transformation (technically, dropconnect is used, with both nodes and connections being removed). 
  dropout_rate1 = dropout_rate  ##RATE FOR DROPPING NODES 
  ulim1 = -0.5 * (1-dropout_rate1) / ( (1-dropout_rate1)-1)
  MASK_VEC1 <- tf$multiply(tf$nn$relu(tf$sign(tf$random_uniform(list(nDim,1L),-0.5,ulim1))), 1 / (ulim1/(ulim1+0.5)))

  dropout_rate2 = 0.05 ##RATE FOR DROPPING CONNECTIONS 
  ulim2 = -0.5 * (1-dropout_rate2) / ( (1-dropout_rate2)-1);
  MASK_VEC2 <- tf$multiply(tf$nn$relu(tf$sign(tf$random_uniform(list(nDim,nProj),-0.5,ulim2))), 1 / (ulim2/(ulim2+0.5)))
  WtsMat_drop = tf$multiply(WtsMat, tf$multiply(MASK_VEC1,MASK_VEC2))

  ### Soft-max transformation
  nonLinearity_fxn = function(x){tf$nn$softsign(x)}
  LFinal = nonLinearity_fxn(tf$matmul(IL_n, WtsMat_drop) + BiasVec)

  #batch renormalization for output layer
  LFinal_m = tf$nn$moments(LFinal, axes = 0L);
  LF_mu_b = LFinal_m[[1]]; 
  LF_sigma2_b = LFinal_m[[2]];
  LF_sigma_b = tf$sqrt(LF_sigma2_b)
  LF_mu_last = tf$placeholder( tf$float32,shape(dim(LF_mu_b)) )
  LF_sigma_last = tf$placeholder( tf$float32,shape(dim(LF_sigma_b)) )
  LFinal_n = tf$nn$batch_normalization(LFinal, mean = LF_mu_b, variance = LF_sigma2_b, offset = 0, scale = 1, variance_epsilon = 0.001)
  
  #Find E[S|D] and calculate objective function  
  ESGivenD_tf = tf$matmul(MultMat_tf,LFinal_n)
  ## Spread component of objective function
  Spread_tf = (tf$maximum(tf$matmul(MultMat_tf,tf$square(LFinal_n)) - tf$square(ESGivenD_tf), 0.001) ) 
  ## Category discrimination (absolute difference in all E[S|D] columns)
  CatDiscrim_tf = tf$abs(tf$gather(ESGivenD_tf, indices = contrast_indices1, axis = 0L) - tf$gather(ESGivenD_tf, indices = contrast_indices2, axis = 0L))
  ## Feature discrimination (row-differences)
  FeatDiscrim_tf = tf$abs(tf$gather(CatDiscrim_tf,  indices = redund_indices1, axis = axis_FeatDiscrim) - tf$gather(CatDiscrim_tf, indices = redund_indices2, axis = axis_FeatDiscrim))
  ## Loss function CatDiscrim + FeatDiscrim + Spread_tf 
  myLoss_tf = -(tf$reduce_mean(CatDiscrim_tf)+tf$reduce_mean(FeatDiscrim_tf) + tf$log(0.01+tf$sqrt(2 * pi * exp(1) * Spread_tf)))
  #https://en.wikipedia.org/wiki/Entropic_uncertainty  
  
  ### Initialize an optimizer using stochastic gradient descent w/ momentum
  myOptimizer_tf = tf$train$MomentumOptimizer(learning_rate = sdg_learning_rate,
                                              momentum      = sgd_momentum, 
                                              use_nesterov  = T)
  
  ### Calculates the gradients from myOptimizer_tf
  myGradients = myOptimizer_tf$compute_gradients(myLoss_tf)
  
  L2_squared_unclipped =  eval(parse( text = paste(sprintf("tf$reduce_sum(tf$square(myGradients[[%s]][[1]]))", 1:length(myGradients)), collapse = "+") ) )
  clip_tf =  tf$placeholder(tf$float32, shape = list()); 
  TEMP__ = eval(parse(text=sprintf("tf$clip_by_global_norm(list(%s),clip_tf)",paste(sprintf('myGradients[[%s]][[1]]', 1:length(myGradients)), collapse = ","))))
  for(jack in 1:length(myGradients)){ myGradients[[jack]][[1]] = TEMP__[[1]][[jack]] } 
  L2_squared =  eval(parse( text = paste(sprintf("tf$reduce_sum(tf$square(myGradients[[%s]][[1]]))", 1:length(myGradients)), collapse = "+") ) )
  ### applies the gradient updates
  myOptimizer_tf_apply = myOptimizer_tf$apply_gradients( myGradients )

  #Updates for the batch normalization moments
  Moments_learn <- mLearn ## 
  IL_mu_ = Moments_learn  * IL_mu_b +  (1-Moments_learn) * IL_mu_last; 
  IL_sigma_ = Moments_learn * IL_sigma_b + (1-Moments_learn) * IL_sigma_last
  
  LF_mu_ = Moments_learn * LF_mu_b + (1-Moments_learn) * LF_mu_last;
  LF_sigma_ = Moments_learn * LF_sigma_b + (1-Moments_learn) * LF_sigma_last
  
  #Setup the outputs 
  OUTPUT_LFinal = nonLinearity_fxn( tf$matmul(OUTPUT_IL_n, WtsMat) + BiasVec )
  OUTPUT_LFinal_n = tf$nn$batch_normalization(OUTPUT_LFinal, mean = LF_mu_last,variance = tf$square(LF_sigma_last), offset = 0, scale = 1, variance_epsilon = 0)
  
  # Initialize global variables in TensorFlow Graph
  init = tf$global_variables_initializer()
 
  # Holding containers for results
  boot_readme <- matrix(nrow=nboot, ncol=nCat);colnames(boot_readme) <- names(labeled_pd)
  hold_coef <- rep(0, nCat); names(hold_coef) <-  names(labeled_pd) ## Holding container for coefficients (for cases where a category is missing from a bootstrap iteration)
  MatchedPrD_div <- OrigESGivenD_div <- MatchedESGivenD_div <- rep(NA, times = nboot) # Holding container for diagnostics
  
  ##  Estimate the parameters
  if (verbose == T){
    cat("Estimating...\n")
  }
  
  for(iter_i in 1:nboot){ 
      sess$run(init) # Initialize TensorFlow graph
      ## Print iteration count
      if (verbose == T & iter_i %% 10 == 0){
        cat(paste("Bootstrap iteration: ", iter_i, "\n"))
      }
      ### Function to generate bootstrap sample
      sgd_grabSamp = function(){ unlist(sapply(1:nCat, function(ze){  sample(list_indices_by_cat[[ze]], NObsByCat[ze], replace = T )  } ))}
      
      ########
      ## Parameter initialization
      ########
      
      ### Means and variances for batch normalization of the input layer - initialize starting parameters
      update_ls <- list() 
      d_ <- replicate(30, sess$run(list(IL_mu_b,IL_sigma_b,L2_squared_unclipped),  feed_dict = dict(IL_input = dfm_labeled[sgd_grabSamp(),],
                                                                               IL_mu_last =  rep(0, times = ncol(dfm_labeled)), IL_sigma_last = rep(1, times = ncol(dfm_labeled)))))
      update_ls[[1]] =  rowMeans( do.call(cbind, d_[1,]) )  
      update_ls[[2]] =  rowMeans( do.call(cbind, d_[2,]) )  
      
      ### Calculate a clip value for the gradients to avoid overflow
      init_L2_squared_vec = unlist( d_[3,] ) 
      rm(d_)
      clip_value = 2 * summary(init_L2_squared_vec)[2]
      
      ## Initialize vector to store learning rates
      inverse_learning_rate_vec <- rep(NA, times = sgd_iters) 
      
      ## Inverse learning rate = clip value
      inverse_learning_rate <- 0.50 * median( init_L2_squared_vec ) 
      
      ### For each iteration of SGD
      for(awer in 1:sgd_iters){
        ## Update the moving averages for batch normalization of the inputs + train parameters (apply the gradients via myOptimizer_tf_apply)
        update_ls = sess$run(list( IL_mu_,IL_sigma_, L2_squared, myOptimizer_tf_apply),
                             feed_dict = dict(IL = dfm_labeled[sgd_grabSamp(),],sdg_learning_rate = 1/inverse_learning_rate,
                                              clip_tf = clip_value,IL_mu_last =  update_ls[[1]], IL_sigma_last = update_ls[[2]]))
        ### Update the learning rate
        inverse_learning_rate_vec[awer] <- inverse_learning_rate <- inverse_learning_rate + update_ls[[3]] / inverse_learning_rate
      }
      
      ### Given the learned parameters, output the feature transformations for the entire matrix
      out_dfm = try(sess$run(OUTPUT_LFinal,feed_dict = dict(OUTPUT_IL = rbind(dfm_labeled, dfm_unlabeled), IL_mu_last =  update_ls[[1]], IL_sigma_last = update_ls[[2]])), T)
      out_dfm_labeled <- out_dfm[1:nrow(dfm_labeled), ]; out_dfm_unlabeled <- out_dfm[-c(1:nrow(dfm_labeled)),]
      
      ### Here ends the SGD for generating optimal document-feature matrix.
      
      ### If we're also going to do estimation
      if(justTransform == F){ 
          ## Minimum number of observations to use in each category per bootstrap iteration
          min_size <- min(r_clip_by_value(as.integer( round( 0.90*(  nrow(dfm_labeled)*labeled_pd) )),10,100))
          nRun = nBoot_matching ; k_match = kMatch ## Initialize parameters - number of runs = nBoot_matching, k_match = number of matches
          ### Sample indices for bootstrap by category
          indices_list = replicate(nRun,list( unlist( lapply(list_indices_by_cat, function(x){sample(x, min_size, replace = T) }) ) ) )
          ### For each bootstrap iteration
          BOOTSTRAP_EST <- sapply(1:nRun, function(boot_iter){ 
            indi_i = indices_list[[boot_iter]]; ## Extract indices associated with that iteration
            Cat_ = categoryVec_labeled[indi_i]; X_ = out_dfm_labeled[indi_i,];Y_ = out_dfm_unlabeled #Category labels, Labeled Features (X), Unlabeled Features Y_ 
            { 
              ### Normalize X and Y
              MM1 = colMeans(Y_); 
              combine_SDs <- function(x,y){r_clip_by_value(max(x,y), 1/3, 3)}
              MM2 = sapply(1:ncol(X_), function(x){ combine_SDs(sd(X_[,x]),Y_[,x]) } )
              X_ = FastScale(X_, MM1, MM2); Y_ = FastScale(Y_, MM1, MM2); 
              
              ## If we're using matching
              if (k_match != 0){
                ### KNN matching - find k_match matches in X_ to Y_
                MatchIndices_i <- knn_adapt(reweightSet = X_, fixedSet = Y_, k = k_match)$return_indices
                #MatchIndices_i <- c(FNN::get.knnx(data = X_, query = Y_, k = k_match)$nn.index) 
                ## Any category with less than minMatch matches includes all of that category
                t_ = table( Cat_[MatchIndices_i] ) ; t_ = t_[t_<minMatch]
                if(length(t_) > 0){ for(t__ in names(t_)){MatchIndices_i = MatchIndices_i[!Cat_[MatchIndices_i] %in%  t__] ; MatchIndices_i = c(MatchIndices_i,which(Cat_ == t__ )) }}
              }else{ ## Otherwise use all the indices
                MatchIndices_i <- 1:nrow(X_)
              }
              categoryVec_labeled_matched = Cat_[MatchIndices_i]; X_ = X_[MatchIndices_i,]
              matched_list_indices_by_cat <- tapply(1:length(categoryVec_labeled_matched), categoryVec_labeled_matched, function(x){c(x) })
         
              ### Carry out estimation on the matched samples
              min_size2 <- min(r_clip_by_value(unlist(lapply(matched_list_indices_by_cat, length))*0.90,10,100))
              est_readme2 = rowMeans(  replicate(20, { 
                matched_list_indices_by_cat_ = lapply(matched_list_indices_by_cat, function(sae){ sample(sae, min_size2, replace = T) })
                X_ = X_[unlist(matched_list_indices_by_cat_),]; categoryVec_labeled_matched_sampled = categoryVec_labeled_matched[unlist(matched_list_indices_by_cat_)]
                MM1_samp = colMeans(Y_);MM2_samp = sapply(1:ncol(X_), function(x){ combine_SDs(X_[,x],Y_[,x]) } )
                X_ = FastScale(X_, MM1_samp, MM2_samp); Y_ = FastScale(Y_, MM1_samp, MM2_samp)
                ESGivenD_sampled = do.call(cbind, tapply(1:length( categoryVec_labeled_matched_sampled ) , categoryVec_labeled_matched_sampled, function(x){colMeans(X_[x,])}) ) 
                try(readme_est_fxn(X = ESGivenD_sampled, Y = colMeans(Y_))[names(labeled_pd)],T) } ) )  
              } 
              list(est_readme2 = est_readme2) 
          } )
        ### Average the bootstrapped estimates
        est_readme2 <- rowMeans(do.call(cbind,BOOTSTRAP_EST), na.rm = T)
        #sum(abs(est_readme2-unlabeled_pd))
        ### Save them as tf_est_results
        tf_est_results <- list(est_readme2 = est_readme2, transformed_unlabeled_dfm = out_dfm_unlabeled,
                               transformed_labeled_dfm = list(unmatched_transformed_labeled_dfm = cbind(as.character(categoryVec_labeled), out_dfm_labeled),matched_transformed_labeled_dfm = cbind(as.character(categoryVec_labeled), out_dfm_labeled)))
      }
      ## If we're just doing the transformation
      else if(justTransform == T){ 
        tf_est_results <- list(transformed_unlabeled_dfm = out_dfm_unlabeled,transformed_labeled_dfm = list(unmatched_transformed_labeled_dfm = cbind(as.character(categoryVec_labeled), out_dfm_labeled)) ) }    

    ## if it's the first iteration
    if(iter_i == 1){ 
      ### Calculate the transformed DFM
      transformed_dfm <- dfm[,1:(ncol(tf_est_results$transformed_unlabeled_dfm))]; transformed_dfm[] <- NA
      transformed_dfm[which(labeledIndicator==1),] <- apply(tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,-1], 2, f2n)
      transformed_dfm[which(labeledIndicator==0),] <- apply(tf_est_results$transformed_unlabeled_dfm, 2, f2n)
      ### Only run one iteration if we're just transforming the data
      if(justTransform == T){sess$close(); return(list(transformed_dfm=transformed_dfm))} 
    }
    ## Save results 
    est_readme <- tf_est_results$est_readme
    temp_est_readme <- hold_coef 
    temp_est_readme[names(est_readme)] <- est_readme
    boot_readme[iter_i,names(temp_est_readme)] <- temp_est_readme
    ## If we're saving diagnostics, do some processing
    if(diagnostics == T){
      ESGivenD_div <- try({ 
        OldMat <- apply(tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,-1], 2, f2n)
        PreESGivenD <-  do.call(cbind,tapply(1:length(tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,1]),
                               tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,1], function(za){
          colMeans(OldMat[za,]) }))
        
        NewMat <- apply(tf_est_results$transformed_labeled_dfm$matched_transformed_labeled_dfm[,-1], 2, f2n)
        PostESGivenD <-  do.call(cbind,tapply(1:length(tf_est_results$transformed_labeled_dfm$matched_transformed_labeled_dfm[,1]),
                                tf_est_results$transformed_labeled_dfm$matched_transformed_labeled_dfm[,1], function(za){colMeans(NewMat[za,])}))
        
        unlabeled_transformed_dfm <- apply(tf_est_results$transformed_unlabeled_dfm, 2, f2n)
        TrueESGivenD <-  do.call(cbind,tapply(1:nrow(unlabeled_transformed_dfm), categoryVec_unlabeled, function(za){
                        colMeans(unlabeled_transformed_dfm[za,]) }))
        sharedCols <- intersect(colnames(TrueESGivenD), colnames(PostESGivenD))
        
        OrigESGivenD_div_ = mean(abs(c(PreESGivenD[,sharedCols]) - c(TrueESGivenD[,sharedCols])))
        MatchedESGivenD_div_ = mean(abs(c(PostESGivenD[,sharedCols]) - c(TrueESGivenD[,sharedCols])))
        t( data.frame(OrigESGivenD_div_ = OrigESGivenD_div_, MatchedESGivenD_div_ = MatchedESGivenD_div_ ) ) 
      }, T)
      MatchedPrD_div[iter_i] <- sum(abs(vec2prob(tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,1])[names(unlabeled_pd)] - unlabeled_pd))
      OrigESGivenD_div[iter_i] <- try(ESGivenD_div["OrigESGivenD_div_",1], T) 
      MatchedESGivenD_div[iter_i] <- try(ESGivenD_div["MatchedESGivenD_div_",1], T)  
    } 
  }
  
  ### Close the TensorFlow session
  sess$close()
  if(verbose==T){
    cat("Finished!")
  }
  ## Parse output
  ## If no diagnostics wanted
  if(diagnostics == F){return( list(point_readme=colMeans(boot_readme, na.rm = T) , transformed_dfm = transformed_dfm) )  }
  ## If diagnostics wanted
  browser() 
  if(diagnostics == T){return( list(point_readme = colMeans(boot_readme, na.rm = T) ,
                                    transformed_dfm = transformed_dfm, 
                                    diagnostics = list(OrigPrD_div = sum(abs(labeled_pd[names(unlabeled_pd)] - unlabeled_pd)),
                                    MatchedPrD_div = mean(MatchedPrD_div, na.rm = T), 
                                    OrigESGivenD_div = mean(OrigESGivenD_div, na.rm = T), 
                                    MatchedESGivenD_div = mean(MatchedESGivenD_div, na.rm = T))) )  }
}

