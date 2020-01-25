#' readme
#' 
#' Implements the quantification algorithm described in Jerzak, King, and Strezhnev (2018) which is meant to improve on the ideas in Hopkins and King (2010).
#' Employs the Law of Total Expectation in a feature space that is tailoed to minimize the error of the resulting estimate. 
#' Automatic differentiation, stochastic gradient descent, and knn_adaptbatch re-normalization are used to carry out the optimization.
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
#' @param nBoot A scalar indicating the number of times the estimation procedure will be re-run (useful for reducing the variance of the final output).
#'
#' @param verbose Should progress updates be given? Input should be a Boolean. 
#' 
#' @param sgdIters How many stochastic gradient descent iterations should be used? Input should be a positive number.   
#'  
#' @param justTransform A Boolean indicating whether the user wants to extract the quanficiation-optimized 
#' features only. 
#' 
#' @param numProjections How many projections should be calculated? Input should be a positive number. Minimum number of projections = number of categories + 2. 
#' 
#' @param batchSizePerCat What should the batch size per category be in the sgd optimization and knn matching? 
#' 
#' @param batchSizePerCat_match What should the batch size per category be in the bagged knn matching? 
#' 
#' @param nbootMatch How many bootstrap samples should we aggregiate when doing the knn matching? 
#' 
#' @param kMatch What should k be in the k-nearest neighbor matching? Input should be a positive number.   
#' 
#' @param nCores How many CPU cores are available? Default is 1. 
#' 
#' @param nCores_OnJob How many CPU cores should we make available to tensorflow? Default is 1.
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
#'                          nBoot = 2, sgdIters = 500)
#'print(readme_results$point_readme)
#'
#' @import tensorflow 
#' @export 
readme <- function(dfm , 
                   labeledIndicator,
                   categoryVec, 
                   nBoot          = 4,  
                   sgdIters      = 500,
                   numProjections = 20,
                   batchSizePerCat = 10, 
                   kMatch         = 3, 
                   batchSizePerCat_match = 20, 
                   minMatch       = 8,
                   nbootMatch    = 10,
                   justTransform  = F,
                   verbose        = F,  
                   diagnostics    = F, 
                   nCores = 1L, 
                   nCores_OnJob = 1L ,
                   regraph  = F,
                   conda_env = NULL,
                   otherOption = NULL){ 
  eval(parse(text="require(tensorflow,quietly=T)"),envir = globalenv())
  eval(parse(text="suppressWarnings(try(tensorflow::use_compat(version='v1'), T))"),envir = globalenv())
  if(!is.null(conda_env)){
    eval(parse(text=sprintf("suppressWarnings(try(tensorflow::use_condaenv(condaenv='%s',required=F), T))",
                          conda_env)),envir = globalenv())
  }
  
  #set options 
  op <- options(digits.secs = 6)
  
  ## Get summaries of all of the document characteristics and labeled indicator
  nLabeled    = sum(labeledIndicator == 1)
  nUnlabeled  = sum(labeledIndicator == 0)
  labeledCt   = table(categoryVec[labeledIndicator == 1])
  
  if (verbose == T){
    if (kMatch == 0){
      cat("Note: 'kmatch' set to 0, skipping matching procedure for the labeled set")
    }
    cat("Data summary:\n")
    cat(paste("Count of documents in each category in the labeled set:\n"))
    print(labeledCt)
  }

  #Setup information for SGD
  categoryVec_unlabeled = as.factor( categoryVec )[labeledIndicator == 0]
  categoryVec_labeled   = as.factor( categoryVec )[labeledIndicator == 1]
  l_indices_by_cat      = tapply(1:length(categoryVec_labeled), categoryVec_labeled, c)
  labeled_pd            = vec2prob( categoryVec_labeled )
  if(any(labeled_pd == 1/length(categoryVec_labeled)) ){stop("Must have at least 1 labeled document in each category.")}
  unlabeled_pd          = vec2prob( categoryVec_unlabeled )
  nCat                  = as.integer( length(labeled_pd) ); 
  rm(categoryVec);
  
  ## Holding containers for results
  boot_readme          = matrix(0,nrow=nBoot, ncol = nCat, dimnames = list(NULL, names(labeled_pd)))
  boot_readme_NoMatching <- boot_readme
  hold_coef            = labeled_pd## Holding container for coefficients (for cases where a category is missing from a bootstrap iteration)
  hold_coef[]          = 0
  MatchedPrD_div       = OrigESGivenD_div = MatchedESGivenD_div <- rep(NA, times = nBoot) # Holding container for diagnostics
  
  #Parameters for Batch-SGD
  NObsPerCat            = as.integer( batchSizePerCat )#min(r_clip_by_value(as.integer( round( sqrt(  nrow(dfm_labeled)*labeled_pd))),minBatch,maxBatch)) ## Number of observations to sample per category
  nProj                 = as.integer(max( numProjections, nCat+1) ); ## Number of projections
  
  #Start SGD
  if (verbose == T){
    cat("Initializing TensorFlow session\n")
    cat(paste("Number of feature projections: ", nProj, "\n", sep=""))
  }
  # Initialize tensorflow
  
  #Winsorize
  dfm_class = class(dfm)
  if(dfm_class == "list"){ dfm_labeled = as.matrix(data.table::fread(cmd = dfm$labeled_cmd))[,-1]} 
  if(dfm_class != "list"){ dfm_labeled = dfm[which(labeledIndicator==1),]; dfm_unlabeled = dfm[which(labeledIndicator==0),];rm(dfm)} 
  nDim_full             = ncol(dfm_labeled)
  WinsValues = apply(dfm_labeled,2,Winsorize_values)
  WinsMat = function(dfm_, values_){ 
      sapply(1:ncol(dfm_), 
             function(sa){ 
               zap = dfm_[,sa]
               bounds_ = values_[,sa]
               zap[zap < bounds_[1]] <- bounds_[1]
               zap[zap > bounds_[2]] <- bounds_[2]
               return( zap )   })
  }
  dfm_labeled = WinsMat(dfm_labeled, WinsValues)
  
  regraph_ = try((ncol(IL_input) != ncol(dfm_labeled)), T) 
  if(class(regraph_) == "try-error" | regraph_ == T){regraph_ <- T}
  graphfil = graph_file_gen(nDim=nDim_full,nProj=numProjections, regraph = regraph_)
  try(source(graphfil,local=F),T) 
  #try(unlink(graphfil),T);
  
  FinalParams_LIST <- list(); tf_junk <- ls()
  
  ## For calculating discrimination - how many possible cross-category contrasts are there
  contrast_indices1_v       = as.integer( (combn(1:nCat, 2) - 1)[1,])
  contrast_indices2_v       = as.integer( (combn(1:nCat, 2) - 1)[2,])
  if(nCat == 2){dim(contrast_indices1_v) <- dim(contrast_indices2_v) <- 1}
  redund_indices1_v         = as.vector(as.integer((combn(1:nProj, 2) - 1)[1,]))
  redund_indices2_v         = as.vector(as.integer((combn(1:nProj, 2) - 1)[2,]))

  grab_samp <- function(){ 
      unlist(lapply(l_indices_by_cat, function(zed){ sample(zed, size = NObsPerCat, replace = length(zed) <(0.95*NObsPerCat) ) }))
  }
  MultMat_tf_v          = t(do.call(rbind,sapply(1:nCat,function(x){
    urat = 0.001; uncertainty_amt = urat / ( (nCat - 1 ) * urat + 1  ); MM = matrix(uncertainty_amt, nrow = NObsPerCat,ncol = nCat); MM[,x] = 1-(nCat-1)*uncertainty_amt
    return( list(MM) )  } )) ); MultMat_tf_v          = MultMat_tf_v  / rowSums( MultMat_tf_v )
 
     S_ = eval(parse(text="tf$Session(graph = readme_graph,
                  config = tf$ConfigProto(
                    device_count=list('GPU'=0L, 'CPU' = as.integer(nCores)), 
                    inter_op_parallelism_threads = as.integer(nCores_OnJob),
                    intra_op_parallelism_threads = as.integer(nCores_OnJob),
                    allow_soft_placement = T) )"),envir = globalenv()) 
          for(iter_i in 1:nBoot){ 
                      if (verbose == T & iter_i %% 10 == 0){
                        ## Print iteration count
                        cat(paste("Bootstrap iteration: ", iter_i, "\n"))
                      }
                      S_$run(init2)
                      if(iter_i == 1){
                        S_$run(init1) # Initialize 
                        IL_stats       = list(IL_mu_b,IL_sigma2_b)
                        IL_stats       = replicate(100,
                                                   S_$run(IL_stats, feed_dict = dict( IL_input = dfm_labeled[grab_samp(),])))
                      
                        IL_mu_last_v          = colMeans(do.call(rbind,IL_stats[1,]))
                        IL_sigma_last_v       = sqrt(colMeans(do.call(rbind,IL_stats[2,])))
                        rm(IL_stats)
                        
                        #assign entries 
                        L2_initial_v  = sqrt(median(c(unlist(replicate(50, 
                                                                          S_$run(L2_squared_clipped, 
                                                                                 feed_dict =  dict(contrast_indices1=contrast_indices1_v,
                                                                                                   contrast_indices2=contrast_indices2_v,
                                                                                                   redund_indices1=redund_indices1_v,
                                                                                                   redund_indices2=redund_indices2_v,
                                                                                                   MultMat_tf = MultMat_tf_v,IL_input = dfm_labeled[grab_samp(),])))))))
                      S_$run( setclip_action, feed_dict = dict(L2_initial=0.50*L2_initial_v) )
                      }
                      inv_learn_rate_seq = rep(NA,times=sgdIters+1)
                      inv_learn_rate_seq[1] = S_$run( set_inverse_learn_action, feed_dict = dict(L2_initial=max(2*L2_initial_v,4/3)) )

                      ### For each iteration of SGDs
                      t1=Sys.time()
                      learn_seq_spot = 0 ; temp_vec = c()
                      for(j in 1:sgdIters){ 
                        if(j %% 100 == 0 & j < 0.75*sgdIters){learn_seq_spot=0}
                        learn_seq_spot = learn_seq_spot + 1 
                        inv_learn_rate_seq[j+1] = S_$run(learn_group, 
                                                        dict(contrast_indices1=contrast_indices1_v,
                                                         contrast_indices2=contrast_indices2_v,
                                                         redund_indices1=redund_indices1_v,
                                                         redund_indices2=redund_indices2_v,
                                                         sgd_learn_rate = 1/inv_learn_rate_seq[learn_seq_spot],
                                                         MultMat_tf = MultMat_tf_v,IL_input = dfm_labeled[grab_samp(),]))[[1]]
                        temp_vec[j] <- inv_learn_rate_seq[learn_seq_spot]
                      }
                      print(sprintf("Done with this round of training in %s minutes!",round(difftime(Sys.time(),t1,units="mins"),2)))
                      
                      #save final parameters 
                      FinalParams_LIST[[length(FinalParams_LIST)+1]] <- S_$run( FinalParams_list )
               }
                      
          try(S_$close(), T) 
          try(tf$keras$backend$clear_session(), T) 
          try(tf$keras$backend$reset_uids(), T)
  
  tf_junk <- ls()[!ls() %in% c(tf_junk, "IL_mu_last_v","IL_sigma_last_v" )]
  eval(parse(text = sprintf("rm(%s)", paste(tf_junk, collapse = ","))))
  
  for(iter_i in 1:nBoot){ 
    ### Given the learned parameters, output the feature transformations for the entire matrix
    out_dfm_labeled = t( t(FinalParams_LIST[[iter_i]][[1]]) %*% ((t(dfm_labeled) - IL_mu_last_v) / IL_sigma_last_v) + c(FinalParams_LIST[[iter_i]][[2]]))
    out_dfm_labeled = out_dfm_labeled/(1+abs(out_dfm_labeled))
    
    if(dfm_class == "list"){ 
      out_dfm_unlabeled = try(t( t(FinalParams_LIST[[iter_i]][[1]]) %*% ((t(WinsMat(as.matrix(data.table::fread(cmd = dfm$unlabeled_cmd))[,-1], WinsValues)) - IL_mu_last_v) / IL_sigma_last_v) + c(FinalParams_LIST[[iter_i]][[2]])),T)
    } 
    if(dfm_class != "list"){ 
      out_dfm_unlabeled = try(t( t(FinalParams_LIST[[iter_i]][[1]]) %*% ((t(WinsMat(dfm_unlabeled, WinsValues)) - IL_mu_last_v) / IL_sigma_last_v) + c(FinalParams_LIST[[iter_i]][[2]])),T) 
    } 
    out_dfm_unlabeled = out_dfm_unlabeled/(1+abs(out_dfm_unlabeled))
    
    ### Here ends the SGD for generating optimal document-feature matrix.
    ### If we're also going to do estimation
    if(justTransform == F){ 
      ## Minimum number of observations to use in each category per bootstrap iteration
      MM1           = colMeans(out_dfm_unlabeled); 
      MM2_          = colSds(out_dfm_unlabeled,MM1);

      indices_list  = replicate(nbootMatch,list( unlist( lapply(l_indices_by_cat,  function(x){sample(x, batchSizePerCat_match, 
                                                                                                      replace = length(x) * 0.75 < batchSizePerCat_match  ) }) ) ) )### Sample indices for bootstrap by category. No replacement is important here.
      BOOTSTRAP_EST = sapply(1:nbootMatch, function(boot_iter){ 
        Cat_    = categoryVec_labeled[indices_list[[boot_iter]]]; 
        X_      = out_dfm_labeled[indices_list[[boot_iter]],];
        Y_      = out_dfm_unlabeled
        
        ### Normalize X and Y
        MM2     = apply(cbind(MM2_, colSds(X_,  colMeans(X_))), 1, function(xa){max(xa)})#robust approx of x*y
        X_      = FastScale(X_, MM1, MM2);
        Y_      = FastScale(Y_, MM1, MM2)
        
        ### Important functions 
        est_obsMatch = function(weight_indices){ 
          categoryVec_LabMatch = Cat_[weight_indices]; X_m = X_[weight_indices,]
          MatchIndices_byCat   = tapply(1:length(categoryVec_LabMatch),
                                        categoryVec_LabMatch, function(x){c(x) })
          
          ### Carry out estimation on the matched samples
          est_readme2_ = try(  sapply(1:nbootMatch, function(eare){ 
            MatchIndices_byCat_          = lapply(MatchIndices_byCat, function(sae){ sample(sae, 
                                                                                            batchSizePerCat_match, 
                                                                                            replace = length(sae) * 0.75 < batchSizePerCat_match ) })
            X__                          = X_m[unlist(MatchIndices_byCat_),]; 
            categoryVec_LabMatch_        = categoryVec_LabMatch[unlist(MatchIndices_byCat_)]
            
            ESGivenD_sampled             = do.call(cbind, tapply(1:nrow( X__ ) , categoryVec_LabMatch_, function(x){colMeans(X__[x,])}) )
            colnames(ESGivenD_sampled)   <- names(labeled_pd)
            ESGivenD_sampled[rowMeans(ESGivenD_sampled>0) %in% c(0,1),] <- 0 
            Y_ = rep(0, times = nrow(ESGivenD_sampled))
            ED_sampled                   = try(readme_est_fxn(X         = ESGivenD_sampled,
                                                              Y         = Y_)[names(labeled_pd)],T)
            return( list(ED_sampled      = ED_sampled,
                         ESGivenD_Match  = ESGivenD_sampled ) )    
          } ), T)
          
          ED_sampled_averaged = try(colMeans(do.call(rbind,est_readme2_[1,])), T)
          return( list(ED_sampled_averaged         = ED_sampled_averaged ) )
                                                
        } 
        
        ### Weights from KNN matching - find kMatch matches in X_ to Y_
        {            
          knnIndices_i  = try(c(FNN::get.knnx(data    = X_, 
                                                query = Y_, 
                                                k     = kMatch)$nn.index) , T) 
          
          ## Any category with less than minMatch matches includes all of that category
          t_              = table( Cat_[unique(knnIndices_i)] ); 
          t_              = t_[t_<minMatch]
          if(length(t_) > 0){ for(t__ in names(t_)){
            knnIndices_i = knnIndices_i[!Cat_[knnIndices_i] %in%  t__] ; 
            knnIndices_i = c(knnIndices_i,
                               sample(which(Cat_ == t__ ), 
                                      minMatch, 
                                      replace = T))
          }
          }
        }
        
        ### All indices
        { 
          AllIndices_i  = 1:nrow(X_)
        }
        
        est_readme2 <- est_obsMatch(knnIndices_i)
        est_readme2_NoMatching <- est_obsMatch(AllIndices_i)
        return( list(est_readme2=est_readme2$ED_sampled_averaged,
                     est_readme2_NoMatching=est_readme2_NoMatching$ED_sampled_averaged,
                     est_readme2_ESGivenD =est_readme2$ESGivenD_sampled_averaged,
                     est_readme2_ESGivenD_NoMatching=est_readme2_NoMatching$ESGivenD_sampled_averaged) ) 
      })
      
      ### Get the bootstrapped estimates
      est_readme2 <- rowMeans(do.call(cbind,BOOTSTRAP_EST[1,]),na.rm=T)
      est_readme2_NoMatching <- rowMeans(do.call(cbind,BOOTSTRAP_EST[2,]),na.rm=T)
      
      try(rm(BOOTSTRAP_EST,indices_list), T)  
    } 
    
    ## Get the transformed data 
    if(justTransform==T){ 
      ### Calculate the transformed DFM
      f2n = function(.){as.numeric(as.character(.))}
      transformed_dfm <- matrix(NA, nrow =  length(labeledIndicator), ncol = nProj)
      transformed_dfm[which(labeledIndicator==1),] <- apply(out_dfm_labeled, 2, f2n)
      transformed_dfm[which(labeledIndicator==0),] <- apply(out_dfm_unlabeled, 2, f2n)
      return(list(transformed_dfm=transformed_dfm))
    } 
    
    { 
      ### Save first iteration as tf_est_results
      tf_est_results <- list(transformed_labeled_dfm   = out_dfm_labeled,
                             transformed_unlabeled_dfm = out_dfm_unlabeled)
      
      ### Calculate the transformed DFM
      transformed_dfm <- matrix(NA, nrow =  length(labeledIndicator), ncol = nProj)
      transformed_dfm[which(labeledIndicator==1),] <- try(apply(tf_est_results$transformed_labeled_dfm, 2, f2n),T)
      transformed_dfm[which(labeledIndicator==0),] <- apply(tf_est_results$transformed_unlabeled_dfm, 2, f2n)
    }
    
    ## Save results 
    boot_readme[iter_i,names(est_readme2)] = est_readme2
    boot_readme_NoMatching[iter_i,names(est_readme2)] = est_readme2_NoMatching
  } 
  
  ### Close the TensorFlow session
  if(verbose==T){ cat("Finished!") }
  ## Parse output
  ## If no diagnostics wanted
  #sort( sapply(ls(),function(x){object.size(get(x))})) 
  return( list(point_readme = colMeans(boot_readme, na.rm = T),
               point_readme_NoMatching = colMeans(boot_readme_NoMatching, na.rm = T),
               transformed_dfm = transformed_dfm)) 

}

graph_file_gen <- function(nDim,nProj=20,regraph = F,use_env=globalenv()){
  { 
  eval_text = sprintf('
  tf$reset_default_graph()
  readme_graph = tf$Graph()
  with(readme_graph$as_default(), {
    #Assumptions 
    nDim = as.integer(  %s  )  
    nProj = as.integer(  %s  )  
    NObsPerCat = as.integer(  10 )  
    dropout_rate <- 0.50 
    
    #INPUTS 
    contrast_indices1            = tf$placeholder(tf$int32,list(NULL))
    contrast_indices2            = tf$placeholder(tf$int32,list(NULL))
    redund_indices1            = tf$placeholder(tf$int32,list(NULL))
    redund_indices2            = tf$placeholder(tf$int32,list(NULL))
    MultMat_tf               = tf$placeholder(tf$float32,list(NULL, NULL))
    L2_initial               = tf$placeholder(tf$float32)

    IL_input             = tf$placeholder(tf$float32,list(NULL, nDim))
    
    # Placeholder settings - to be filled when executing TF operations
    clip_tf               = tf$Variable(10000., dtype = tf$float32, trainable = F )
    inverse_learn_rate = tf$Variable(1., dtype = tf$float32, trainable = F)
    sgd_learn_rate     = tf$placeholder(tf$float32)
    
    IL_m                = tf$nn$moments(IL_input, axes = 0L)
    IL_mu_b             = IL_m[[1]]; IL_sigma2_b         = IL_m[[2]]
    IL_n                = tf$nn$batch_normalization(IL_input, mean = IL_m[[1]], variance = IL_m[[2]], offset = 0, scale = 1, variance_epsilon = 0.001)
    
    #SET UP WEIGHTS to be optimized
    initializer_reweighting =  1/sd(replicate(100, {
      beta__                =   runif(nDim,  -1/sqrt(nDim), 1/sqrt(nDim)  )
      dropout__             =   rbinom(nDim, size = 1, prob = dropout_rate)
      beta__[dropout__==1]  <- 0
      beta__[dropout__==0]  <- beta__[dropout__==0] / (1 - dropout_rate)
      sum(beta__) }))
    WtsMat               = tf$Variable(initializer_reweighting*tf$random_uniform(list(nDim,nProj),-1/sqrt(nDim), 1/sqrt(nDim), dtype = tf$float32),dtype = tf$float32, trainable = T)
    BiasVec              = tf$Variable(as.vector(rep(0,times = nProj)), trainable = T, dtype = tf$float32)
    
    ### Drop-out transformation
    ulim1                = -0.5 * (1-dropout_rate) / ( (1-dropout_rate)-1)
    MASK_VEC           = tf$multiply(tf$nn$relu(tf$sign(tf$random_uniform(list(nDim,1L),-0.5,ulim1,dtype = tf$float32))), 1 / (ulim1/(ulim1+0.5)))
    WtsMat_drop          = tf$multiply(WtsMat, MASK_VEC)

    ### Apply non-linearity + batch normalization 
    LFinal               = tf$nn$softsign( tf$matmul(IL_n, WtsMat_drop) + BiasVec)
    LFinal_m             = tf$nn$moments(LFinal, axes = 0L)
    LFinal_n             = tf$nn$batch_normalization(LFinal, mean = LFinal_m[[1]], variance = LFinal_m[[2]], offset = 0, scale = 1, variance_epsilon = 0.001)
    
    #Find E[S|D] and calculate objective function  
    ESGivenD_tf          = tf$matmul(MultMat_tf,LFinal_n)
    
    ## Spread component of objective function 
    gathering_mat        = tf$range(start = 0L, limit = tf$shape(LFinal_n)[[0]], delta = 1L, dtype = tf$int32)
    gathering_mat        = tf$transpose(tf$reshape(gathering_mat, shape = list(-1L, NObsPerCat) ))
    Spread_tf            = tf$minimum(tf$reduce_mean(tf$abs(tf$gather(params = LFinal_n, indices = gathering_mat, axis = 0L) - ESGivenD_tf), 0L),1)

    ## Category discrimination (absolute difference in all E[S|D] columns)
    CatDiscrim_tf        = tf$minimum(tf$abs(tf$gather(ESGivenD_tf, indices = contrast_indices1, axis = 0L) -
                                               tf$gather(ESGivenD_tf, indices = contrast_indices2, axis = 0L)), 1.50)
    
    ## Feature discrimination (row-differences)
    FeatDiscrim_tf       = tf$minimum(tf$abs(tf$gather(CatDiscrim_tf,  indices = redund_indices1, axis = 1L) -
                                               tf$gather(CatDiscrim_tf, indices = redund_indices2, axis = 1L)), 1.50)
    
    ## Loss function CatDiscrim + FeatDiscrim + Spread_tf 
    Loss_tf            = -( tf$reduce_mean(CatDiscrim_tf) + 
                                tf$reduce_mean(FeatDiscrim_tf) +
                                0.01 * tf$reduce_mean( tf$log(tf$reduce_min(Spread_tf, 0L)+0.001) ))

    ### Initialize an optimizer using stochastic gradient descent w/ momentum
    Optimizer_tf             = tf$train$MomentumOptimizer(learning_rate = sgd_learn_rate,
                                                          momentum      = 0.90, use_nesterov  = T)
    
    ### Calculates the gradients from myOpt_tf
    Gradients_unclipped  = Optimizer_tf$compute_gradients( Loss_tf ) 
    Gradients_clipped    = Gradients_unclipped
    TEMP__               = tf$clip_by_global_norm(list(Gradients_unclipped[[1]][[1]],
                                                        Gradients_unclipped[[2]][[1]]),clip_tf)
    for(jack in 1:length(Gradients_clipped)){ Gradients_clipped[[jack]][[1]] = TEMP__[[1]][[jack]] } 
    L2_squared_clipped   = tf$reduce_sum(tf$square(Gradients_clipped[[1]][[1]])) + 
                                      tf$reduce_sum(tf$square(Gradients_clipped[[2]][[1]]))
    inverse_learn_rate_update = tf$assign_add(ref = inverse_learn_rate, value = L2_squared_clipped / inverse_learn_rate)
    
    #learning consists of gradient updates plus learning rate updates. 
    learn_group       = list(  inverse_learn_rate, inverse_learn_rate_update, 
                                   Optimizer_tf$apply_gradients( Gradients_clipped ))
    
    # Initialize variables in TensorFlow Graph
    init0 = tf$variables_initializer(list(WtsMat, BiasVec,clip_tf,inverse_learn_rate,
                                         Optimizer_tf$get_slot(tf$trainable_variables()[[1]],Optimizer_tf$get_slot_names()),
                                         Optimizer_tf$get_slot(tf$trainable_variables()[[2]],Optimizer_tf$get_slot_names())))
    init1 = tf$variables_initializer(list(clip_tf,inverse_learn_rate,
                                         Optimizer_tf$get_slot(tf$trainable_variables()[[1]],Optimizer_tf$get_slot_names()),
                      Optimizer_tf$get_slot(tf$trainable_variables()[[2]],Optimizer_tf$get_slot_names())))
     init2 = tf$variables_initializer(list(WtsMat, BiasVec))

    #other actions 
    FinalParams_list        = list(WtsMat,BiasVec)
    setclip_action          = clip_tf$assign(  L2_initial   )
    set_inverse_learn_action          = inverse_learn_rate$assign(  L2_initial )
  })
  readme_graph$finalize()

  ', nDim, nProj)
  } 
  if(  (!"readme_graph" %in% ls(env = globalenv())) | regraph == T){
    if(regraph == T){
      print("Performance warning: Rebuilding tensorflow graph...")
      suppressWarnings(rm(readme_graph, envir = use_env)); #tf$reset_default_graph()
    }
    print("Building master readme graph...")
    #use_env = globalenv()
    #eval(parse(text=eval_text), envir = globalenv())
    #eval.parent(parse(text=eval_text))
    graphfil <- tempfile(fileext=".R")
    zz <- file(graphfil, "w")  # open an output file connection
    cat(eval_text, file = zz)
    close(zz)
    return( graphfil )
  } 
}
