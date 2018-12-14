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
#' @param batchSizePerCat What should the batch size per category be in the sgd optimization and knn matching? 
#' 
#' @param dropout_rate What should the dropout rate be in the sgd optimization? Input should be a positive number.   
#' 
#' @param batchSizePerCat_match What should the batch size per category be in the bagged knn matching? 
#' 
#' @param nboot_match How many bootstrap samples should we aggregiate when doing the knn matching? 
#' 
#' @param kMatch What should k be in the k-nearest neighbor matching? Input should be a positive number.   
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
#' #Generate 100 ``documents'' of 5-10 words each. 
#' my_documentText <- replicate(100, 
#'                              paste(sample(row.names(my_wordVecs), 
#'                                           sample(5:10, 1), 
#'                                           replace = T), 
#'                                    collapse = " ") ) 
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
#'                          nboot = 2, sgd_iters = 500)
#'print(readme_results$point_readme)
#'
#' @export 
#' @import tensorflow
readme <- function(dfm, labeledIndicator, categoryVec, 
                   nboot          = 4,  
                   sgd_iters      = 1000,
                   sgd_momentum   = .9,
                   numProjections = 20,
                   mLearn         = 0.01, 
                   dropout_rate   = 0.5, 
                   batchSizePerCat = 10, 
                   kMatch         = 3, 
                   batchSizePerCat_match = 20, 
                   minMatch       = 10,
                   nboot_match    = 40,
                   winsorize      = T, 
                   justTransform  = F,
                   verbose        = F,  
                   diagnostics    = F){ 
  
  ## Get summaries of all of the document characteristics and labeled indicator
  nDocuments  = nrow(dfm)
  nFeat       = ncol(dfm)
  nLabeled    = sum(labeledIndicator == 1)
  nUnlabeled  = sum(labeledIndicator == 0)
  labeledCt   = table(categoryVec[labeledIndicator == 1])
  
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

  # Winsorize the columns of the document-feature matrix
  if(winsorize          == T){
  dfm                   = apply(dfm, 2, Winsorize_fxn )
  }
  ## Drop invariant columns
  dfm                   = dfm[,apply(dfm,2,sd)>0]

  #Setup information for SGD
  categoryVec_unlabeled = as.factor( categoryVec )[labeledIndicator == 0]  
  categoryVec_labeled   = as.factor( categoryVec )[labeledIndicator == 1]
  labeled_pd            = vec2prob( categoryVec_labeled ); 
  unlabeled_pd          = vec2prob( categoryVec_unlabeled )
  dfm_labeled           = dfm[labeledIndicator==1,]; 
  dfm_unlabeled         = dfm[labeledIndicator==0,]
  nCat                  = as.integer( length(labeled_pd) ); 
  nDim                  = as.integer( ncol(dfm_labeled) )  #nDim = Number of raw features
  
  #nonlinearity fxn for projection 
  nonLinearity_fxn      = function(x){tf$nn$softsign(x)}
  
  ## Generic winsorization function 
  r_clip_by_value       = function(x, a, b){x[x<=a] <- a;x[x>=b] <- b;return(x)}
  
  ### Function to generate bootstrap sample
  sgd_grabSamp   = function(){ unlist(sapply(1:nCat, function(ze){  sample(l_indices_by_cat[[ze]], NObsPerCat, replace = length(l_indices_by_cat[[ze]]) *0.75 < NObsPerCat )  } ))}
  
  #Parameters for Batch-SGD
  NObsPerCat            = as.integer( batchSizePerCat )#min(r_clip_by_value(as.integer( round( sqrt(  nrow(dfm_labeled)*labeled_pd))),minBatch,maxBatch)) ## Number of observations to sample per category
  nProj                 = as.integer(max(numProjections,nCat+1) ); ## Number of projections
  
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
  contrasts_mat       = combn(1:nCat, 2) - 1
  contrast_indices1   = as.integer(contrasts_mat[1,])
  contrast_indices2   = as.integer(contrasts_mat[2,])
  
  ## For calculating feature novelty - how many possible cross-feature contrasts are there
  redund_mat          = combn(1:nProj, 2) - 1
  redund_indices1     = as.integer(redund_mat[1,])
  redund_indices2     = as.integer(redund_mat[2,])
  axis_FeatDiscrim    = as.integer(nCat!=2)
    
  #Placeholder settings - to be filled when executing TF operations
  sdg_learning_rate   = tf$placeholder(tf$float16, shape = c())
  clip_tf             = tf$placeholder(tf$float16, shape = list()); 
  
  ## Transformation matrix from features to E[S|D] (urat determines how much smoothing we do across categories)
  MultMat             = t(do.call(rbind,sapply(1:nCat,function(x){
                          urat = 0.01; uncertainty_amt = urat / ( (nCat - 1 ) * urat + 1  );MM = matrix(uncertainty_amt, nrow = NObsPerCat,ncol = nCat); MM[,x] = 1-(nCat-1)*uncertainty_amt
                          return( list(MM) )  } )) )
  MultMat             = MultMat  / rowSums( MultMat )
  MultMat_tf          = tf$constant(MultMat, dtype = tf$float16)

  ## Which indices in the labeled set are associated with each category
  l_indices_by_cat    = tapply(1:length(categoryVec_labeled), categoryVec_labeled, c)
    
  #SET UP INPUT layer to TensorFlow and apply batch normalization for the input layer
  
  # In this case, a line with only 3 positions
  if(T == T){
  IL_input_full            = tf$constant(dfm_labeled, dtype = tf$float16)
  for(req in 1:nCat){ 
    eval(parse(text = sprintf("CatIndices_%s = tf$constant( as.integer(l_indices_by_cat[[req]]-1) , dtype = tf$int32)", req)))
    eval(parse(text = sprintf("CatIndices_%s_shuf = tf$random_shuffle(CatIndices_%s)", req,req)))
    eval(parse(text = sprintf("CatIndices_%s_shuf = tf$random_shuffle(CatIndices_%s)", req,req)))
    eval(parse(text = sprintf("CatIndices_%s_shuf = tf$random_shuffle(CatIndices_%s)", req,req)))
    eval(parse(text = sprintf("batch_indices_%s = tf$gather(CatIndices_%s_shuf, indices = as.integer(0:(NObsPerCat-1)), axis = 0L)", req, req ) ) )
  } 
  eval(parse(text = sprintf("Sample_indices_tf   = tf$concat(list(%s), axis = 0L)", 
             paste(paste("batch_indices_", 1:nCat, sep = ""), collapse = ","))))
  IL_input            = tf$gather(IL_input_full, indices = Sample_indices_tf, axis = 0L)
  } 
  #IL_input            = tf$placeholder(tf$float16, shape = list(as.integer(NObsPerCat * nCat), as.integer(nDim)))
  IL_m                = tf$nn$moments(IL_input, axes = 0L);
  IL_mu_b             = IL_m[[1]];
  IL_sigma2_b         = IL_m[[2]];
  IL_sigma_b          = tf$sqrt(IL_sigma2_b)
  IL_mu_last          = tf$placeholder( tf$float16,shape(dim(IL_mu_b)) )
  IL_sigma_last       = tf$placeholder( tf$float16,shape(dim(IL_sigma_b)) )
  IL_n                = tf$nn$batch_normalization(IL_input, mean = IL_mu_b, variance = IL_sigma2_b, offset = 0, scale = 1, variance_epsilon = 0.001)
  OUTPUT_IL           = tf$placeholder(tf$float16, shape = list(NULL, nDim))
  OUTPUT_IL_n         = tf$nn$batch_normalization(OUTPUT_IL, mean = IL_mu_last,variance = tf$square(IL_sigma_last), offset = 0, scale = 1, variance_epsilon = 0)
  
  #SET UP WEIGHTS to be optimized
  WtsMat               = tf$Variable(tf$random_uniform(list(nDim,nProj),-1/sqrt(nDim+nProj), 1/sqrt(nDim+nProj), dtype = tf$float16),dtype = tf$float16, trainable = T)
  BiasVec              = tf$Variable(as.vector(rep(0,times = nProj)), trainable = T, dtype = tf$float16)

  ### Drop-out transformation 
  ulim1                = -0.5 * (1-dropout_rate) / ( (1-dropout_rate)-1)
  MASK_VEC1            = tf$multiply(tf$nn$relu(tf$sign(tf$random_uniform(list(nDim,1L),-0.5,ulim1,dtype = tf$float16))), 1 / (ulim1/(ulim1+0.5)))
  WtsMat_drop          = tf$multiply(WtsMat, MASK_VEC1)

  ### Apply non-linearity + batch normalization 
  LFinal               = nonLinearity_fxn(tf$matmul(IL_n, WtsMat_drop) + BiasVec)
  LFinal_m             = tf$nn$moments(LFinal, axes = 0L);
  LFinal_n             = tf$nn$batch_normalization(LFinal, mean = LFinal_m[[1]], variance = LFinal_m[[2]], offset = 0, scale = 1, variance_epsilon = 0.001)
   
  #Find E[S|D] and calculate objective function  
  ESGivenD_tf          = tf$matmul(MultMat_tf,LFinal_n)
  
  ## Spread component of objective function
  Spread_tf            = tf$sqrt(tf$matmul(MultMat_tf,tf$square(LFinal_n)) - tf$square(ESGivenD_tf)+0.01^2)

  ## Category discrimination (absolute difference in all E[S|D] columns)
  CatDiscrim_tf        = tf$abs(tf$gather(ESGivenD_tf, indices = contrast_indices1, axis = 0L) - tf$gather(ESGivenD_tf, indices = contrast_indices2, axis = 0L))
  
  ## Feature discrimination (row-differences)
  FeatDiscrim_tf       = tf$abs(tf$gather(CatDiscrim_tf,  indices = redund_indices1, axis = axis_FeatDiscrim) - tf$gather(CatDiscrim_tf, indices = redund_indices2, axis = axis_FeatDiscrim))
  
  ## Loss function CatDiscrim + FeatDiscrim + Spread_tf 
  myLoss_tf            = -(tf$reduce_mean(tf$minimum(CatDiscrim_tf,2)  ) + 
                             tf$reduce_mean(tf$minimum(FeatDiscrim_tf,2)  ) + 
                              tf$constant(0.10, dtype = tf$float16)*tf$reduce_mean(tf$log( tf$minimum(Spread_tf,0.40) ) ))
  
  ### Initialize an optimizer using stochastic gradient descent w/ momentum
  myOpt_tf             = tf$train$MomentumOptimizer(learning_rate = sdg_learning_rate,
                                                    momentum      = sgd_momentum, 
                                                    use_nesterov  = T)

  ### Calculates the gradients from myOpt_tf
  myGradients_unclipped = myOpt_tf$compute_gradients(myLoss_tf) 

  myGradients_clipped  = myOpt_tf$compute_gradients(myLoss_tf) 
  TEMP__               = eval(parse(text=sprintf("tf$clip_by_global_norm(list(%s),clip_tf)",paste(sprintf('myGradients_unclipped[[%s]][[1]]', 1:length(myGradients_unclipped)), collapse = ","))))
  for(jack in 1:length(myGradients_clipped)){ myGradients_clipped[[jack]][[1]] = TEMP__[[1]][[jack]] } 
  L2_squared_clipped   = eval(parse( text = paste(sprintf("tf$reduce_sum(tf$square(myGradients_clipped[[%s]][[1]]))", 1:length(myGradients_unclipped)), collapse = "+") ) )
  
  ### applies the gradient updates
  myOpt_tf_apply       = myOpt_tf$apply_gradients( myGradients_clipped )  

  #Updates for the batch normalization moments
  OneMinus_mLearn      = tf$constant(1-mLearn, dtype = tf$float16)
  mLearn               = tf$constant(mLearn, dtype = tf$float16)
  IL_mu_               = mLearn  * IL_mu_b   + OneMinus_mLearn * IL_mu_last; 
  IL_sigma_            = mLearn * IL_sigma_b + OneMinus_mLearn * IL_sigma_last
  
  #Setup the outputs 
  OUTPUT_LFinal        = nonLinearity_fxn( tf$matmul(OUTPUT_IL_n, WtsMat) + BiasVec )
  
  # Initialize global variables in TensorFlow Graph
  init                 = tf$global_variables_initializer()
  
  # Holding containers for results
  boot_readme          = matrix(nrow=nboot, ncol=nCat, dimnames = list(NULL, names(labeled_pd)))
  hold_coef            = labeled_pd## Holding container for coefficients (for cases where a category is missing from a bootstrap iteration)
  hold_coef[]          = 0
  MatchedPrD_div       = OrigESGivenD_div = MatchedESGivenD_div <- rep(NA, times = nboot) # Holding container for diagnostics
  
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
      ### Means and variances for batch normalization of the input layer - initialize starting parameters
      update_ls      = list() 
      browser() 
      sess$run( CatIndices_1_shuf ) 
      d_             = replicate(30, sess$run(list(IL_mu_b, IL_sigma_b, L2_squared_clipped), 
                                              feed_dict = dict(clip_tf = 10000.,
                                                               IL_input      = dfm_labeled[sgd_grabSamp(),],
                                                               IL_mu_last    =  rep(0, times = ncol(dfm_labeled)),
                                                               IL_sigma_last = rep(1, times = ncol(dfm_labeled)))))
      update_ls[[1]] =  rowMeans( do.call(cbind, d_[1,]) )  
      update_ls[[2]] =  rowMeans( do.call(cbind, d_[2,]) )  
      
      ### Calculate a clip value for the gradients to avoid overflow
      init_L2_squared_vec   = unlist( d_[3,] ) 
      inverse_learning_rate = 0.50 * median( init_L2_squared_vec )
      clip_value = 0.50 * median( sqrt( init_L2_squared_vec )  )
      print( clip_value ) 
      rm(d_)
      
      ## Initialize vector to store learning rates
      inverse_learning_rate_vec = rep(NA, times = sgd_iters) 
      
      ### For each iteration of SGDs
      for(awer in 1:sgd_iters){
        ## Update the moving averages for batch normalization of the inputs + train parameters (apply the gradients via myOpt_tf_apply)
        update_ls                       = sess$run(list( IL_mu_,IL_sigma_, L2_squared_clipped, myOpt_tf_apply),
                                                 feed_dict = dict(
                                                                  IL_input          = dfm_labeled[sgd_grabSamp(),],
                                                                  sdg_learning_rate = 1/inverse_learning_rate,
                                                                  clip_tf           = clip_value, 
                                                                  IL_mu_last        = update_ls[[1]], 
                                                                  IL_sigma_last     = update_ls[[2]]))
        ### Update the learning rate
        inverse_learning_rate_vec[awer] = inverse_learning_rate <- inverse_learning_rate + update_ls[[3]] / inverse_learning_rate
      } 
      ### Given the learned parameters, output the feature transformations for the entire matrix
      out_dfm           = try(sess$run(OUTPUT_LFinal,feed_dict = dict(OUTPUT_IL     = rbind(dfm_labeled, dfm_unlabeled), 
                                                                      IL_mu_last    = update_ls[[1]], 
                                                                      IL_sigma_last = update_ls[[2]])), T)
      out_dfm_labeled   = out_dfm[1:nrow(dfm_labeled),]; 
      out_dfm_unlabeled = out_dfm[-c(1:nrow(dfm_labeled)),]
      
      print("e")
      ### Here ends the SGD for generating optimal document-feature matrix.
      
      ### If we're also going to do estimation
      if(justTransform == F){ 
          ## Minimum number of observations to use in each category per bootstrap iteration
          indices_list  = replicate(nboot_match,list( unlist( lapply(l_indices_by_cat, function(x){sample(x, batchSizePerCat_match, replace = length(x) * 0.75 < batchSizePerCat_match  ) }) ) ) )### Sample indices for bootstrap by category. No replacement is important here. 
          MM1           = colMeans(out_dfm_unlabeled); 
          MM2_          = colSds(out_dfm_unlabeled,MM1);
          
          BOOTSTRAP_EST = sapply(1:nboot_match, function(boot_iter){ 
            Cat_    = categoryVec_labeled[indices_list[[boot_iter]]]; 
            X_      = out_dfm_labeled[indices_list[[boot_iter]],];
            Y_      = out_dfm_unlabeled
            
            ### Normalize X and Y
            MM2     = apply(cbind(MM2_, colSds(X_,  colMeans(X_))), 1, function(xa){max(xa)})#robust approx of x*y
            X_      = FastScale(X_, MM1, MM2);
            Y_      = FastScale(Y_, MM1, MM2)
              
            ## If we're using matching
            if (kMatch != 0){
                ### KNN matching - find kMatch matches in X_ to Y_
                MatchIndices_i  = c(FNN::get.knnx(data = X_, query = Y_, k = kMatch)$nn.index) 

                ## Any category with less than minMatch matches includes all of that category
                t_              = table( Cat_[unique(MatchIndices_i)] ); 
                t_              = t_[t_<minMatch]
                if(length(t_) > 0){ for(t__ in names(t_)){
                  MatchIndices_i = MatchIndices_i[!Cat_[MatchIndices_i] %in%  t__] ; 
                  MatchIndices_i = c(MatchIndices_i,
                                     sample(which(Cat_ == t__ ), 
                                            minMatch, replace = length(which(Cat_ == t__ )) < minMatch))
                    }
                  }
            }else{ ## Otherwise use all the indices
                MatchIndices_i  = 1:nrow(X_)
            }
            categoryVec_LabMatch = Cat_[MatchIndices_i]; X_m = X_[MatchIndices_i,]
            MatchIndices_byCat   = tapply(1:length(categoryVec_LabMatch), categoryVec_LabMatch, function(x){c(x) })
          
            ### Carry out estimation on the matched samples
            est_readme2_ = try((  replicate(30, { 
                MatchIndices_byCat_          = lapply(MatchIndices_byCat, function(sae){ sample(sae, batchSizePerCat_match, replace = length(sae) * 0.75 < batchSizePerCat_match ) })
                X__                          = X_m[unlist(MatchIndices_byCat_),]; 
                categoryVec_LabMatch_        = categoryVec_LabMatch[unlist(MatchIndices_byCat_)]

                ESGivenD_sampled            = do.call(cbind, tapply(1:nrow( X__ ) , categoryVec_LabMatch_, function(x){colMeans(X__[x,])}) )
                colnames(ESGivenD_sampled)   = names( MatchIndices_byCat_ ) 
                ED_sampled                   = try(readme_est_fxn(X         = ESGivenD_sampled,
                                                                  Y         = rep(0, times = nrow(ESGivenD_sampled)))[names(labeled_pd)],T)
                return( ED_sampled )  
              } )), T)
              ED_sampled_averaged = try(rowMeans(est_readme2_), T)  
              
              return( list(ED_sampled_averaged) )
          })
          
          ### Average the bootstrapped estimates
          est_readme2 <- rowMeans(do.call(cbind,BOOTSTRAP_EST), na.rm = T)
          #sum(abs(est_readme2-unlabeled_pd)); sum(abs(labeled_pd-unlabeled_pd))
          ### Save them as tf_est_results
          tf_est_results <- list(est_readme2               = est_readme2,
                                 transformed_unlabeled_dfm = out_dfm_unlabeled,
                                 transformed_labeled_dfm   = list(unmatched_transformed_labeled_dfm = cbind(as.character(categoryVec_labeled), out_dfm_labeled),
                                                                matched_transformed_labeled_dfm   = cbind(as.character(categoryVec_labeled), out_dfm_labeled)))
      }
      ## If we're just doing the transformation
      else if(justTransform == T){ 
        tf_est_results <- list(transformed_unlabeled_dfm = out_dfm_unlabeled,
                               transformed_labeled_dfm   = list(unmatched_transformed_labeled_dfm = cbind(as.character(categoryVec_labeled), out_dfm_labeled)) ) }    

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
    est_readme                                 = tf_est_results$est_readme
    temp_est_readme                            = hold_coef 
    temp_est_readme[names(est_readme)]         = est_readme
    boot_readme[iter_i,names(temp_est_readme)] = temp_est_readme
    ## If we're saving diagnostics, do some processing
    if(diagnostics == T){
      ESGivenD_div               = try({ 
        OldMat                   = apply(tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,-1], 2, f2n)
        PreESGivenD              = do.call(cbind,tapply(1:length(tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,1]),
                                            tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,1], function(za){
                                            colMeans(OldMat[za,]) }))
                                          
        NewMat                    = apply(tf_est_results$transformed_labeled_dfm$matched_transformed_labeled_dfm[,-1], 2, f2n)
        PostESGivenD              = do.call(cbind,tapply(1:length(tf_est_results$transformed_labeled_dfm$matched_transformed_labeled_dfm[,1]),
                                    tf_est_results$transformed_labeled_dfm$matched_transformed_labeled_dfm[,1], function(za){colMeans(NewMat[za,])}))
        
        unlabeled_transformed_dfm = apply(tf_est_results$transformed_unlabeled_dfm, 2, f2n)
        TrueESGivenD              = do.call(cbind,tapply(1:nrow(unlabeled_transformed_dfm), categoryVec_unlabeled, function(za){
                                            colMeans(unlabeled_transformed_dfm[za,]) }))
        sharedCols                = intersect(colnames(TrueESGivenD),  colnames(PostESGivenD))
        
        OrigESGivenD_div_         = mean(abs(c(PreESGivenD[,sharedCols]) - c(TrueESGivenD[,sharedCols])))
        MatchedESGivenD_div_      = mean(abs(c(PostESGivenD[,sharedCols]) - c(TrueESGivenD[,sharedCols])))
        return__                  = t( data.frame(OrigESGivenD_div_    = OrigESGivenD_div_, 
                                                  MatchedESGivenD_div_ = MatchedESGivenD_div_ ) ) 
        return__
      }, T)
      MatchedPrD_div[iter_i]      = sum(abs(vec2prob(tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,1])[names(unlabeled_pd)] - unlabeled_pd))
      OrigESGivenD_div[iter_i]    = try(ESGivenD_div["OrigESGivenD_div_",1], T) 
      MatchedESGivenD_div[iter_i] = try(ESGivenD_div["MatchedESGivenD_div_",1], T)  
    } 
  }
  
  ### Close the TensorFlow session
  sess$close()
  if(verbose==T){ cat("Finished!") }
  ## Parse output
  ## If no diagnostics wanted
  if(diagnostics == F){return( list(point_readme    = colMeans(boot_readme, na.rm = T) ,
                                    transformed_dfm = transformed_dfm) )  }
  ## If diagnostics wanted
  if(diagnostics == T){return( list(point_readme    = colMeans(boot_readme, na.rm = T) ,
                                    transformed_dfm = transformed_dfm, 
                                    diagnostics     = list(OrigPrD_div         = sum(abs(labeled_pd[names(unlabeled_pd)] - unlabeled_pd)),
                                                           MatchedPrD_div      = mean(MatchedPrD_div, na.rm = T), 
                                                           OrigESGivenD_div    = mean(OrigESGivenD_div, na.rm = T), 
                                                           MatchedESGivenD_div = mean(MatchedESGivenD_div, na.rm = T))) )  }
}

