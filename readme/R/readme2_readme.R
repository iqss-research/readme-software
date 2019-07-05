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
#' @param diagnostics Should diagnostics be returned? Input should be a Boolean. 
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
                   bagFrac        = 1, 
                   kMatch         = 3, 
                   batchSizePerCat_match = 20, 
                   minMatch       = 8,
                   nbootMatch    = 10,
                   justTransform  = F,
                   verbose        = F,  
                   diagnostics    = F, 
                   nCores = 1L, 
                   nCores_OnJob = 1L ,
                   otherOption = NULL){ 
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
  l_indices_by_cat    = tapply(1:length(categoryVec_labeled), categoryVec_labeled, c)
  labeled_pd            = vec2prob( categoryVec_labeled )
  unlabeled_pd          = vec2prob( categoryVec_unlabeled )
  nCat                  = as.integer( length(labeled_pd) ); 
  rm(categoryVec);
  
  ## Holding containers for results
  boot_readme          = matrix(0,nrow=nBoot, ncol = nCat, dimnames = list(NULL, names(labeled_pd)))
  for(aje in 1:9){ 
    eval(parse(text=sprintf("boot_readme_%s = boot_readme",aje)))  
  }
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
  
  require(tensorflow, quietly = T)
  regraph_ = try((ncol(IL_input) != ncol(dfm_labeled)), T) 
  if(class(regraph_) == "try-error" | regraph_ == T){regraph_ <- T}
  start_reading(nDim=nDim_full,bagFrac = bagFrac, nProj=numProjections, regraph = regraph_)
  
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
 
  eval_dict = "dict( contrast_indices1 = contrast_indices1_v, 
contrast_indices2 = contrast_indices2_v, 
redund_indices1 = redund_indices1_v, 
redund_indices2 = redund_indices2_v, 
MultMat_tf = MultMat_tf_v, 
IL_input = dfm_labeled[grab_samp(),bag_cols]
)"
          nDim_bag      = round(nDim_full * bagFrac)
          bag_cols_mat = matrix(NA,nrow=nBoot,ncol=nDim_bag)
          S_ = tf$Session(graph = readme_graph,
                  config = tf$ConfigProto(
                    allow_soft_placement = T, 
                    device_count=list("GPU"=0L, "CPU" = as.integer(nCores)), 
                    inter_op_parallelism_threads = as.integer(nCores_OnJob),intra_op_parallelism_threads = as.integer(nCores_OnJob) ) )
          for(iter_i in 1:nBoot){ 
                      if (verbose == T & iter_i %% 10 == 0){
                        ## Print iteration count
                        cat(paste("Bootstrap iteration: ", iter_i, "\n"))
                      }

                      bag_cols               = sample(1:nDim_full,nDim_bag)
                      bag_cols_mat[iter_i,] <- bag_cols
                      S_$run(init) # Initialize TensorFlow graph
                      if(iter_i == 1){
                        IL_sigma_last_v       = list(IL_mu_b,IL_sigma2_b)
                        IL_sigma_last_v       = replicate(300, S_$run(IL_sigma_last_v, feed_dict = eval(parse(text = eval_dict))))
                      
                        IL_mu_last_v          = colMeans(do.call(rbind,IL_sigma_last_v[1,]))
                        IL_sigma_last_v       = sqrt(colMeans(do.call(rbind,IL_sigma_last_v[2,])))
                        L2_squared_initial_v  = median(c(unlist(replicate(50, S_$run(L2_squared_clipped, feed_dict = eval(parse(text = eval_dict)))))))
                        S_$run( setclip_action, feed_dict = dict(L2_squared_initial=L2_squared_initial_v) ) 
                      }
                      S_$run( restart_action, feed_dict = dict(L2_squared_initial=L2_squared_initial_v) ) 
                      
                      ### For each iteration of SGDs
                      t1=Sys.time()
                      for(j in 1:sgdIters){ S_$run(learning_group,eval(parse(text = eval_dict))) } 
                      
                      print(sprintf("Done with this round of training in %s seconds!",round(Sys.time()-t1, 2)))
                      FinalParams_LIST[[length(FinalParams_LIST)+1]] <- S_$run( FinalParams_list )
              }
          try(S_$close(), T) 
          try(tf$keras$backend$clear_session(), T) 
          try(tf$keras$backend$reset_uids(), T)
  
  tf_junk <- ls()[!ls() %in% c(tf_junk, "IL_mu_last_v","IL_sigma_last_v","bag_cols_mat" )]
  eval(parse(text = sprintf("rm(%s)", paste(tf_junk, collapse = ","))))
  
  for(iter_i in 1:nBoot){ 
    ### Given the learned parameters, output the feature transformations for the entire matrix
    out_dfm_labeled = t( t(FinalParams_LIST[[iter_i]][[1]]) %*% ((t(dfm_labeled[,bag_cols_mat[iter_i,]]) - IL_mu_last_v) / IL_sigma_last_v) + c(FinalParams_LIST[[iter_i]][[2]]))
    out_dfm_labeled = out_dfm_labeled/(1+abs(out_dfm_labeled))
    
    if(dfm_class == "list"){ 
      out_dfm_unlabeled = try(t( t(FinalParams_LIST[[iter_i]][[1]]) %*% ((t(WinsMat(as.matrix(data.table::fread(cmd = dfm$unlabeled_cmd))[,-1], WinsValues)[,bag_cols_mat[iter_i,]]) - IL_mu_last_v) / IL_sigma_last_v) + c(FinalParams_LIST[[iter_i]][[2]])),T)
    } 
    if(dfm_class != "list"){ 
      out_dfm_unlabeled = try(t( t(FinalParams_LIST[[iter_i]][[1]]) %*% ((t(WinsMat(dfm_labeled[,bag_cols_mat[iter_i,]], WinsValues)) - IL_mu_last_v) / IL_sigma_last_v) + c(FinalParams_LIST[[iter_i]][[2]])),T) 
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
      
      est_PropbDistMatch = function(out_dfm_labeled_, out_dfm_unlabeled_,l_indices_by_cat_){ 
          if(!class(l_indices_by_cat_) %in% c("list", "array")){l_indices_by_cat_    = tapply(1:length(l_indices_by_cat_), l_indices_by_cat_, c)} 
          MM1 = colMeans(out_dfm_unlabeled_)
          MM2     = apply(cbind(colSds(out_dfm_labeled_,  colMeans(out_dfm_labeled_)),
                                colSds(out_dfm_unlabeled_,  colMeans(out_dfm_unlabeled_))), 1, function(xa){max(xa)})#robust approx of x*y
          out_dfm_labeled_n      = FastScale(out_dfm_labeled_, MM1, MM2);
          out_dfm_unlabeled_n      = FastScale(out_dfm_unlabeled_, MM1, MM2)
          RegData = sapply(1:nProj,function(proj_i){ 
            X_l      = out_dfm_labeled_n[,proj_i]
            X_u      = out_dfm_unlabeled_n[,proj_i]
            
            distParams = lapply(l_indices_by_cat_,function(sa){ 
              c(mean(X_l[sa]),sd(X_l[sa]))
            })
            dist_u = lapply(distParams,function(dist_k){ 
              dnorm(X_u,mean=dist_k[1], sd = dist_k[2])
            })
            denominator_u = Reduce("+",dist_u)
            p_u = lapply(dist_u,function(dist_i){ 
              prop.table(hist( dist_i / denominator_u,plot = F,breaks=seq(0,1,0.1))$counts)}) 
            p_u = do.call(cbind,p_u)
            
            dist_l = lapply(distParams,function(dist_k){ 
              dnorm(X_l,mean=dist_k[1], sd = dist_k[2])
            })
            denominator_l = Reduce("+",dist_l)
            
            p_l = lapply(dist_l,function(dist_i){ 
              dist_i / denominator_l}) 
            p_l_cond = lapply(p_l,function(p_l_k){ 
              do.call(cbind,lapply(l_indices_by_cat_,function(cat_k_indices){ 
                prop.table(hist(p_l_k[cat_k_indices],plot=F, breaks=seq(0,1,0.1))$counts)
              } ) )
            })
            p_l_cond = do.call(rbind,p_l_cond)
            
            Y = c(p_u)
            X =  p_l_cond
            list(Y=Y,X=X)
          } ) 
          Y = do.call(c, RegData[1,] )
          X = do.call(rbind,RegData[2,])
          est_readme2 = readme_est_fxn(Y=Y,X=X)
          names(est_readme2) = colnames(X)
          return( est_readme2 ) 
      }
      est_DistMatch = function(out_dfm_labeled_, out_dfm_unlabeled_,l_indices_by_cat_){ 
        if(!class(l_indices_by_cat_) %in% c("list", "array")){l_indices_by_cat_    = tapply(1:length(l_indices_by_cat_), l_indices_by_cat_, c)} 
        MM1 = colMeans(out_dfm_unlabeled_)
        MM2     = apply(cbind(colSds(out_dfm_labeled_,  colMeans(out_dfm_labeled_)),
                              colSds(out_dfm_unlabeled_,  colMeans(out_dfm_unlabeled_))), 1, function(xa){max(xa)})#robust approx of x*y
        out_dfm_labeled_n      = FastScale(out_dfm_labeled_, MM1, MM2);
        out_dfm_unlabeled_n      = FastScale(out_dfm_unlabeled_, MM1, MM2)
        RegData = sapply(1:nProj,function(proj_i){
          X_l      = out_dfm_labeled_n[,proj_i]
          X_u      = out_dfm_unlabeled_n[,proj_i]
          
          myBreaks = c(-Inf,seq(-1.5,1.5,0.5),Inf)
          p_u = prop.table(hist(X_u,plot = F,breaks=myBreaks)$counts)
          
          p_l_cond = do.call(cbind,lapply(l_indices_by_cat_,function(cat_k_indices){ 
              prop.table(hist(X_l[cat_k_indices],plot=F, breaks=myBreaks)$counts)
            } ) )
      
          Y = c(p_u)
          X =  p_l_cond
          list(Y=Y,X=X)
        } ) 
        Y = do.call(c, RegData[1,] )
        X = do.call(rbind,RegData[2,])
        est_readme2 = readme_est_fxn(Y=Y,X=X)
        names(est_readme2) = colnames(X)
        return( est_readme2 ) 
      }
      require(Rsolnp, quietly = T)
      BOOTSTRAP_EST = sapply(1:nbootMatch, function(boot_iter){ 
        Cat_    = categoryVec_labeled[indices_list[[boot_iter]]]; 
        X_      = out_dfm_labeled[indices_list[[boot_iter]],];
        Y_      = out_dfm_unlabeled
        
        ### Normalize X and Y
        MM2     = apply(cbind(MM2_, colSds(X_,  colMeans(X_))), 1, function(xa){max(xa)})#robust approx of x*y
        X_      = FastScale(X_, MM1, MM2);
        Y_      = FastScale(Y_, MM1, MM2)
        
        ### Important functions 
        est_obsMatch = function(weight_indices,return_error = FALSE){ 
          categoryVec_LabMatch = Cat_[weight_indices]; X_m = X_[weight_indices,]
          MatchIndices_byCat   = tapply(1:length(categoryVec_LabMatch),
                                        categoryVec_LabMatch, function(x){c(x) })
          
          ### Carry out estimation on the matched samples
          est_readme2_ = try((  sapply(1:nbootMatch, function(eare){ 
            MatchIndices_byCat_          = lapply(MatchIndices_byCat, function(sae){ sample(sae, 
                                                                                            batchSizePerCat_match, 
                                                                                            replace = length(sae) * 0.75 < batchSizePerCat_match ) })
            X__                          = X_m[unlist(MatchIndices_byCat_),]; 
            categoryVec_LabMatch_        = categoryVec_LabMatch[unlist(MatchIndices_byCat_)]
            
            ESGivenD_sampled             = do.call(cbind, tapply(1:nrow( X__ ) , categoryVec_LabMatch_, function(x){colMeans(X__[x,])}) )
            colnames(ESGivenD_sampled)   = names(labeled_pd)
            ESGivenD_sampled[rowMeans(ESGivenD_sampled>0) %in% c(0,1),] <- 0 
            Y_ = rep(0, times = nrow(ESGivenD_sampled))
            ED_sampled                   = try(readme_est_fxn(X         = ESGivenD_sampled,
                                                              Y         = Y_)[names(labeled_pd)],T)
            return( list(ED_sampled=ED_sampled,
                         error = sum(abs(ESGivenD_sampled %*%ED_sampled-Y_) )))    
          } )), T)
          ED_sampled_averaged = try(colMeans(do.call(rbind,est_readme2_[1,])), T)
          if(return_error == FALSE){return( ED_sampled_averaged )  }
          if(return_error == TRUE){return( list(ED_sampled_averaged=ED_sampled_averaged,
                                                error=mean(unlist(est_readme2_[2,]) ),
                                                errorSE=sd(unlist(est_readme2_[2,]) ) / sqrt(length(unlist(est_readme2_[2,])))  ))  }
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
        
        ### Weights using the synthetic controls objective 
        { 
            chunk_k <-  ncol(X_)
            Y_mean = rep(0,times=chunk_k)
            chunk_n = nrow(X_)
            
            lambda_seq = c(2)
            count_ = 0 
            cv_results = list() 
            for(lambda_ in lambda_seq){ 
            count_ <- count_ + 1 
            ObjectiveFxn_toMininimize = function(WTS){ return( sum( abs(Y_mean - colSums( X_ * WTS)   / chunk_n  ) ) /chunk_k   + lambda_ * sum( WTS^2 )  )  }
            if(count_ == 1){  WtsVec = prop.table(runif(nrow(X_), 0.49, 0.51)) }  
            WtsVec = solnp(              pars  = WtsVec, #initial parameter guess 
                                         fun   = ObjectiveFxn_toMininimize,
                                         eqfun = function(WTS){sum(WTS)},#weights must sum...
                                         eqB   = 1,  #...to 1
                                         LB    = rep(0,times = nrow(X_)), #weights must be non-negative 
                                         UB    = rep(1,times = nrow(X_)), #weights must be less than 1 
                                         control = list(trace = 0))$pars
            WtsVec_ = round(WtsVec * 2000  )
            reweightIndices_i = unlist(  sapply(1:length(WtsVec_),
                                    function(indi){
                                      rep(indi,times=WtsVec_[indi])}) )  
            ## Any category with less than minMatch matches includes all of that category
            t_              = table( Cat_[unique(reweightIndices_i)] ); 
            t_              = t_[t_<minMatch]
            if(length(t_) > 0){ for(t__ in names(t_)){
              reweightIndices_i = reweightIndices_i[!Cat_[reweightIndices_i] %in%  t__] ; 
              reweightIndices_i = c(reweightIndices_i,
                               sample(which(Cat_ == t__ ), 
                                      minMatch, 
                                      replace = T))
            }
            } 
            results_lambda_  = est_obsMatch(reweightIndices_i,return_error = T)
            cv_results[[count_]] = list(
                            estimate = results_lambda_$ED_sampled_averaged,
                                error = results_lambda_$error,
                                errorSE = results_lambda_$errorSE,
                                 lambda = lambda_)
            } 
        }
        
        error_cv = unlist(lapply(cv_results,function(x){x$error}))
        errorSE_cv = unlist(lapply(cv_results,function(x){x$errorSE}))
        estimate_cv = do.call(rbind,lapply(cv_results,function(x){x$estimate}))
        error_cv1SE = unlist(lapply(cv_results,function(x){x$errorSE}))
        trueError_cv = apply(estimate_cv,1,function(x){sum(abs(x-unlabeled_pd))})
        which_lambda_1se = max(which(error_cv <= min(error_cv)+error_cv1SE[which.min(error_cv)]))
        est_readme2_9 = cv_results[[which_lambda_1se]]$estimate
        est_readme2_4 = cv_results[[which(lambda_seq == 2)]]$estimate

        ### All indices
        { 
          AllIndices_i  = 1:nrow(X_)
        }
        
        est_readme2 = est_obsMatch(knnIndices_i)
        est_readme2_1 =  est_PropbDistMatch(out_dfm_labeled_   = X_[knnIndices_i,],
                                     out_dfm_unlabeled_ = Y_,
                                     l_indices_by_cat_  = Cat_[knnIndices_i])
        est_readme2_2 = est_obsMatch(AllIndices_i)
        est_readme2_3 =  est_PropbDistMatch(out_dfm_labeled_   = X_[AllIndices_i,],
                                     out_dfm_unlabeled_ = Y_,
                                     l_indices_by_cat_  = Cat_[AllIndices_i])
        #est_readme2_4 = est_obsMatch(reweightIndices_i)
        est_readme2_5 =   est_PropbDistMatch(out_dfm_labeled_   = X_[reweightIndices_i,],
                                      out_dfm_unlabeled_ = Y_,
                                      l_indices_by_cat_  = Cat_[reweightIndices_i])

        return( list(est_readme2=est_readme2,
                     est_readme2_1=est_readme2_1,
                     est_readme2_2=est_readme2_2,
                     est_readme2_3=est_readme2_3,
                     est_readme2_4=est_readme2_4,
                     est_readme2_5=est_readme2_5,
                     est_readme2_9=est_readme2_9) ) 
      })
      
      ### Average the bootstrapped estimates
      est_readme2 <- rowMeans(do.call(cbind,BOOTSTRAP_EST[1,]), na.rm = T)
      est_readme2_1 <- rowMeans(do.call(cbind,BOOTSTRAP_EST[2,]), na.rm = T)
      est_readme2_2 <- rowMeans(do.call(cbind,BOOTSTRAP_EST[3,]), na.rm = T)
      est_readme2_3 <- rowMeans(do.call(cbind,BOOTSTRAP_EST[4,]), na.rm = T)
      est_readme2_4 <- rowMeans(do.call(cbind,BOOTSTRAP_EST[5,]), na.rm = T)
      est_readme2_5 <- rowMeans(do.call(cbind,BOOTSTRAP_EST[6,]), na.rm = T)
      est_readme2_9 <- rowMeans(do.call(cbind,BOOTSTRAP_EST[7,]), na.rm = T)
  
    #use all data and distributions 
    {
      est_readme2_6   = est_PropbDistMatch(out_dfm_labeled_   = out_dfm_labeled,
                                                out_dfm_unlabeled_ = out_dfm_unlabeled,
                                                l_indices_by_cat_  = l_indices_by_cat)
      est_readme2_8   = est_DistMatch(out_dfm_labeled_   = out_dfm_labeled,
                                         out_dfm_unlabeled_ = out_dfm_unlabeled,
                                         l_indices_by_cat_  = l_indices_by_cat)
    }
    
    #use all data and means 
      {
        MM1 = colMeans(out_dfm_unlabeled)
        MM2     = apply(cbind(colSds(out_dfm_unlabeled,  colMeans(out_dfm_unlabeled)),
                              colSds(out_dfm_labeled,  colMeans(out_dfm_labeled))), 1, function(xa){max(xa)})#robust approx of x*y
        out_dfm_labeled_n      = FastScale(out_dfm_labeled, MM1, MM2);
        out_dfm_unlabeled_n      = FastScale(out_dfm_unlabeled, MM1, MM2)
        ESGivenD                      =  do.call(cbind,lapply(l_indices_by_cat,function(xa){colMeans(out_dfm_labeled_n[xa,])}))
        ES                            = colMeans(out_dfm_unlabeled_n)
        est_readme2_7                   = try(readme_est_fxn(X         = ESGivenD,
                                                           Y           = ES),T) 
      
      }
      
      #sum(abs(est_readme2-unlabeled_pd)); sum(abs(labeled_pd-unlabeled_pd))
      try(rm(BOOTSTRAP_EST,indices_list), T)  
      if(diagnostics == F){rm(out_dfm_labeled,out_dfm_unlabeled) }
    } 
    
    ## If we're just doing the transformation
    if(justTransform == T){ 
      ### Calculate the transformed DFM
      f2n = function(.){as.numeric(as.character(.))}
      transformed_dfm <- matrix(NA, nrow =  length(labeledIndicator), ncol = nProj)
      transformed_dfm[which(labeledIndicator==1),] <- apply(out_dfm_labeled, 2, f2n)
      transformed_dfm[which(labeledIndicator==0),] <- apply(out_dfm_unlabeled, 2, f2n)
      
      return(list(transformed_dfm=transformed_dfm))
    } 
    
    ## if it's the first iteration
    if(diagnostics == T){ 
      f2n = function(.){as.numeric(as.character(.))}
      ### Save them as tf_est_results
      tf_est_results <- list(transformed_unlabeled_dfm = out_dfm_unlabeled,
                             transformed_labeled_dfm   = list(unmatched_transformed_labeled_dfm = cbind(as.character(categoryVec_labeled), out_dfm_labeled),
                                                              matched_transformed_labeled_dfm   = cbind(as.character(categoryVec_labeled), out_dfm_labeled)))
      
      ### Calculate the transformed DFM
      transformed_dfm <- matrix(NA, nrow =  length(labeledIndicator), ncol = nProj)
      transformed_dfm[which(labeledIndicator==1),] <- apply(tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,-1], 2, f2n)
      transformed_dfm[which(labeledIndicator==0),] <- apply(tf_est_results$transformed_unlabeled_dfm, 2, f2n)
      
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
    
    ## Save results 
    boot_readme[iter_i,names(est_readme2)] = est_readme2
    for(aje in 1:9){ 
        eval(parse(text=sprintf("boot_readme_%s[iter_i,names(est_readme2)] = est_readme2_%s",aje,aje)))  
    }
  } 
  
  ### Close the TensorFlow session
  browser()
  if(verbose==T){ cat("Finished!") }
  ## Parse output
  ## If no diagnostics wanted
  #sort( sapply(ls(),function(x){object.size(get(x))})) 
  if(diagnostics == F){return( list(point_readme = colMeans(boot_readme, na.rm = T),
                                    point_readme_1    = colMeans(boot_readme_1, na.rm = T) ,
                                    point_readme_2    = colMeans(boot_readme_2, na.rm = T) ,
                                    point_readme_3    = colMeans(boot_readme_3, na.rm = T) ,
                                    point_readme_4    = colMeans(boot_readme_4, na.rm = T) ,
                                    point_readme_5    = colMeans(boot_readme_5, na.rm = T) ,
                                    point_readme_6    = colMeans(boot_readme_6, na.rm = T) ,
                                    point_readme_7    = colMeans(boot_readme_7, na.rm = T),
                                    point_readme_8    = colMeans(boot_readme_8, na.rm = T),
                                    point_readme_9    = colMeans(boot_readme_9, na.rm = T)) )  }
  ## If diagnostics wanted
  if(diagnostics == T){return( list(point_readme    = colMeans(boot_readme, na.rm = T) ,
                                    point_readme_1    = colMeans(boot_readme_1, na.rm = T) ,
                                    point_readme_2    = colMeans(boot_readme_2, na.rm = T) ,
                                    point_readme_3    = colMeans(boot_readme_3, na.rm = T) ,
                                    point_readme_4    = colMeans(boot_readme_4, na.rm = T) ,
                                    point_readme_5    = colMeans(boot_readme_5, na.rm = T) ,
                                    point_readme_6    = colMeans(boot_readme_6, na.rm = T) ,
                                    point_readme_7    = colMeans(boot_readme_7, na.rm = T) ,
                                    point_readme_8    = colMeans(boot_readme_8, na.rm = T) ,
                                    point_readme_9    = colMeans(boot_readme_9, na.rm = T) ,
                                    diagnostics     = list(OrigPrD_div         = sum(abs(labeled_pd[names(unlabeled_pd)] - unlabeled_pd)),
                                                           MatchedPrD_div      = mean(MatchedPrD_div, na.rm = T), 
                                                           OrigESGivenD_div    = mean(OrigESGivenD_div, na.rm = T), 
                                                           MatchedESGivenD_div = mean(MatchedESGivenD_div, na.rm = T))) )  }
}

start_reading <- function(nDim,bagFrac = 1, nProj=20,regraph = F){
  eval_text = sprintf('
  tf$reset_default_graph()
  readme_graph = tf$Graph()
  with(readme_graph$as_default(), {
    #Assumptions 
    nDim_bag <- as.integer(round( %s * %s ) )
    nProj = as.integer(  %s  )  
    NObsPerCat = as.integer(  10 )  
    dropout_rate <- 0.50 
    
    #INPUTS 
    contrast_indices1            = tf$placeholder(tf$int32,list(NULL))
    contrast_indices2            = tf$placeholder(tf$int32,list(NULL))
    redund_indices1            = tf$placeholder(tf$int32,list(NULL))
    redund_indices2            = tf$placeholder(tf$int32,list(NULL))
    IL_input             = tf$placeholder(tf$float32,list(NULL, nDim_bag))
    MultMat_tf           = tf$placeholder(tf$float32,list(NULL, NULL))
    L2_squared_initial       = tf$placeholder(tf$float32)
    
    #Placeholder settings - to be filled when executing TF operations
    clip_tf               = tf$Variable(10000., dtype = tf$float32, trainable = F )
    inverse_learning_rate = tf$Variable(1, dtype = tf$float32, trainable = F)
    sgd_learning_rate      = 1. / inverse_learning_rate
    
    IL_m                = tf$nn$moments(IL_input, axes = 0L)
    IL_mu_b             = IL_m[[1]]
    IL_sigma2_b         = IL_m[[2]]
    IL_n                = tf$nn$batch_normalization(IL_input, mean = IL_m[[1]], variance = IL_m[[2]], offset = 0, scale = 1, variance_epsilon = 0.001)
    
    #SET UP WEIGHTS to be optimized
    initializer_reweighting =  1/sd(replicate(500, {
      beta__                =   runif(nDim_bag,  -1/sqrt(nDim_bag), 1/sqrt(nDim_bag)  )
      dropout__             =   rbinom(nDim_bag, size = 1, prob = dropout_rate)
      beta__[dropout__==1]  <- 0
      beta__[dropout__==0]  <- beta__[dropout__==0] / (1 - dropout_rate)
      sum(beta__) }))
    WtsMat               = tf$Variable(initializer_reweighting*tf$random_uniform(list(nDim_bag,nProj),-1/sqrt(nDim_bag), 1/sqrt(nDim_bag), dtype = tf$float32),dtype = tf$float32, trainable = T)
    BiasVec              = tf$Variable(as.vector(rep(0,times = nProj)), trainable = T, dtype = tf$float32)
    
    ### Drop-out transformation
    ulim1                = -0.5 * (1-dropout_rate) / ( (1-dropout_rate)-1)
    MASK_VEC1            = tf$multiply(tf$nn$relu(tf$sign(tf$random_uniform(list(nDim_bag,1L),-0.5,ulim1,dtype = tf$float32))), 1 / (ulim1/(ulim1+0.5)))
    WtsMat_drop          = tf$multiply(WtsMat, MASK_VEC1)
    ### Apply non-linearity + batch normalization 
    LFinal               = tf$nn$softsign( tf$matmul(IL_n, WtsMat_drop) + BiasVec)
    LFinal_m             = tf$nn$moments(LFinal, axes = 0L)
    LFinal_n             = tf$nn$batch_normalization(LFinal, mean = LFinal_m[[1]], variance = LFinal_m[[2]], offset = 0, scale = 1, variance_epsilon = 0.001)
    
    #Find E[S|D] and calculate objective function  
    ESGivenD_tf          = tf$matmul(MultMat_tf,LFinal_n)
    
    ## Spread component of objective function 
    gathering_mat        = tf$range(start = 0L, limit = tf$shape(LFinal_n)[[0]], delta = 1L, dtype = tf$int32)
    gathering_mat        = tf$transpose(tf$reshape(gathering_mat, shape = list(-1L, NObsPerCat) ))
    Spread_tf            = tf$minimum(tf$reduce_mean(tf$abs(tf$gather(params = LFinal_n, indices = gathering_mat, axis = 0L) - ESGivenD_tf), 0L),0.50)

    ## Category discrimination (absolute difference in all E[S|D] columns)
    CatDiscrim_tf        = tf$minimum(tf$abs(tf$gather(ESGivenD_tf, indices = contrast_indices1, axis = 0L) -
                                               tf$gather(ESGivenD_tf, indices = contrast_indices2, axis = 0L)), 1.50)
    
    ## Feature discrimination (row-differences)
    FeatDiscrim_tf       = tf$minimum(tf$abs(tf$gather(CatDiscrim_tf,  indices = redund_indices1, axis = 1L) -
                                               tf$gather(CatDiscrim_tf, indices = redund_indices2, axis = 1L)), 1.50)
    
    ## Loss function CatDiscrim + FeatDiscrim + Spread_tf 
    Loss_tf            = -( tf$reduce_mean(CatDiscrim_tf) + 
                                tf$reduce_mean(FeatDiscrim_tf) + 
                                  0.01 * tf$reduce_mean( tf$log(tf$reduce_min(Spread_tf, 0L)+0.01) ))
                                #0.10 * tf$reduce_mean( Spread_tf ) #max Spread_tf is 0.30
    ### Initialize an optimizer using stochastic gradient descent w/ momentum
    Optimizer_tf             = tf$train$MomentumOptimizer(learning_rate = sgd_learning_rate,
                                                          momentum      = 0.90, use_nesterov  = T)
    
    ### Calculates the gradients from myOpt_tf
    Gradients_unclipped  = Optimizer_tf$compute_gradients( Loss_tf ) 
    Gradients_clipped    = Gradients_unclipped
    TEMP__               = tf$clip_by_global_norm(list(Gradients_unclipped[[1]][[1]],
                                                        Gradients_unclipped[[2]][[1]]),clip_tf)
    for(jack in 1:length(Gradients_clipped)){ Gradients_clipped[[jack]][[1]] = TEMP__[[1]][[jack]] } 
    L2_squared_clipped   = tf$reduce_sum(tf$square(Gradients_clipped[[1]][[1]])) + 
                                      tf$reduce_sum(tf$square(Gradients_clipped[[2]][[1]]))
    inverse_learning_rate_update = tf$assign_add(ref = inverse_learning_rate, value = L2_squared_clipped / inverse_learning_rate)
    
    ### applies the gradient updates
    myOpt_tf_apply       = Optimizer_tf$apply_gradients( Gradients_clipped )
    
    #learning consists of gradient updates plus learning rate updates. 
    learning_group       = list(  inverse_learning_rate_update, myOpt_tf_apply)
    
    # Initialize variables in TensorFlow Graph
    init = tf$variables_initializer(list(WtsMat, BiasVec,clip_tf,inverse_learning_rate,
                                         Optimizer_tf$get_slot(tf$trainable_variables()[[1]],Optimizer_tf$get_slot_names()),
                                         Optimizer_tf$get_slot(tf$trainable_variables()[[2]],Optimizer_tf$get_slot_names())))
    
    #other actions 
    FinalParams_list        = list(WtsMat, BiasVec)
    setclip_action          = clip_tf$assign(  0.50 * sqrt( L2_squared_initial )  )
    restart_action          = inverse_learning_rate$assign(  0.50 *  L2_squared_initial )
  })
  readme_graph$finalize()

  ', nDim,bagFrac, nProj)
  if(  (!"readme_graph" %in% ls(env = globalenv())) | regraph == T){
    if(regraph == T){
      print("Performance warning: Rebuilding tensorflow graph...")
      suppressWarnings(rm(readme_graph, envir = globalenv())); tf$reset_default_graph()
    }
    print("Building master readme graph...")
    eval(parse(text=eval_text), envir = globalenv())
    print("Readme is now initialized!")
  } 
}
