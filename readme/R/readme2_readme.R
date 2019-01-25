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
#' @param justTransform A Boolean indicating whether the user wants to extract the quanficiation-optimized 
#' features only. 
#' 
#' @param numProjections How many projections should be calculated? Input should be a positive number. Minimum number of projections = number of categories + 2. 
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
readme <- function(dfm = NULL,
                   dfm_cmd = NULL, 
                   labeledIndicator,
                   categoryVec, 
                   nboot          = 4,  
                   sgd_iters      = 1000,
                   numProjections = 20,
                   dropout_rate   = 0.50, 
                   batchSizePerCat = 10, 
                   kMatch         = 3, 
                   batchSizePerCat_match = 20, 
                   minMatch       = 5,
                   nboot_match    = 50,
                   justTransform  = F,
                   verbose        = F,  
                   diagnostics    = F){ 
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
  
  # Winsorize the columns of the document-feature matrix
  #dfm                   = apply(dfm, 2, Winsorize_fxn )
  
  #Setup information for SGD
  categoryVec_unlabeled = as.factor( categoryVec )[labeledIndicator == 0]
  categoryVec_labeled   = as.factor( categoryVec )[labeledIndicator == 1]
  l_indices_by_cat    = tapply(1:length(categoryVec_labeled), categoryVec_labeled, c)
  labeled_pd            = vec2prob( categoryVec_labeled )
  unlabeled_pd          = vec2prob( categoryVec_unlabeled )
  nCat                  = as.integer( length(labeled_pd) ); 
  rm(categoryVec);
  
  ## Holding containers for results
  boot_readme          = matrix(nrow=nboot, ncol = nCat, dimnames = list(NULL, names(labeled_pd)))
  hold_coef            = labeled_pd## Holding container for coefficients (for cases where a category is missing from a bootstrap iteration)
  hold_coef[]          = 0
  MatchedPrD_div       = OrigESGivenD_div = MatchedESGivenD_div <- rep(NA, times = nboot) # Holding container for diagnostics
  
  #Parameters for Batch-SGD
  NObsPerCat            = as.integer( batchSizePerCat )#min(r_clip_by_value(as.integer( round( sqrt(  nrow(dfm_labeled)*labeled_pd))),minBatch,maxBatch)) ## Number of observations to sample per category
  nProj                 = as.integer(max( numProjections, nCat+1) ); ## Number of projections
  
  #Start SGD
  if (verbose == T){
    cat("Initializing TensorFlow session\n")
    cat(paste("Number of feature projections: ", nProj, "\n", sep=""))
  }
  # Initialize tensorflow
  
  FinalParams_LIST <- list(); tf_junk <- ls()
  #try(detach("package:tensorflow", unload=TRUE), T)  
  #require("tensorflow", quietly = T)
  
  ## For calculating discrimination - how many possible cross-category contrasts are there
  contrast_indices1_v       = as.integer( (combn(1:nCat, 2) - 1)[1,])
  contrast_indices2_v       = as.integer( (combn(1:nCat, 2) - 1)[2,])
  redund_indices1_v     = as.integer((combn(1:nProj, 2) - 1)[1,])
  redund_indices2_v     = as.integer((combn(1:nProj, 2) - 1)[2,])
  axis_FeatDiscrim_v    = as.integer(nCat!=2)
  
  dfm_labeled = as.matrix(data.table::fread(cmd = dfm_cmd$labeled_cmd))[,-1]
  grab_samp <- function(){ 
      unlist(lapply(l_indices_by_cat, function(zed){ sample(zed, size = NObsPerCat, replace = 0.90*(NObsPerCat > length(zed))) }))
  }
  MultMat_tf_v          = t(do.call(rbind,sapply(1:nCat,function(x){
    urat = 0.001; uncertainty_amt = urat / ( (nCat - 1 ) * urat + 1  ); MM = matrix(uncertainty_amt, nrow = NObsPerCat,ncol = nCat); MM[,x] = 1-(nCat-1)*uncertainty_amt
    return( list(MM) )  } )) ); MultMat_tf_v          = MultMat_tf_v  / rowSums( MultMat_tf_v )

  eval_dict = "dict( contrast_indices1 = contrast_indices1_v, 
contrast_indices2 = contrast_indices2_v, 
redund_indices1 = redund_indices1_v, 
redund_indices2 = redund_indices2_v, 
axis_FeatDiscrim = axis_FeatDiscrim_v, 
MultMat_tf = MultMat_tf_v, 
IL_input = dfm_labeled[grab_samp(),]
)"
  
with(environment(), { 
                    for(iter_i in 1:nboot){ 
                      if (verbose == T & iter_i %% 10 == 0){
                        ## Print iteration count
                        cat(paste("Bootstrap iteration: ", iter_i, "\n"))
                      }
                      
                      sess$run(init) # Initialize TensorFlow graph
                      if(iter_i == 1){
                        IL_sigma_last_v       = list(IL_mu_b,IL_sigma2_b)
                        IL_sigma_last_v       = replicate(300, sess$run(IL_sigma_last_v, feed_dict = eval(parse(text = eval_dict))))
                        IL_mu_last_v          = colMeans(do.call(rbind,IL_sigma_last_v[1,]))
                        IL_sigma_last_v       = sqrt(colMeans(do.call(rbind,IL_sigma_last_v[2,])))
                        L2_squared_initial_v  = median(c(unlist(replicate(50, sess$run(L2_squared_clipped, feed_dict = eval(parse(text = eval_dict)))))))
                        sess$run( setclip_action, feed_dict = dict(L2_squared_initial=L2_squared_initial_v) ) 
                      }
                      sess$run( restart_action, feed_dict = dict(L2_squared_initial=L2_squared_initial_v) ) 
                      
                      ### For each iteration of SGDs
                      print("Training...")
                      t1=Sys.time()
                      for(j in 1:sgd_iters){ sess$run(learning_group,eval(parse(text = eval_dict))) } 
                      print(Sys.time()-t1)
                      
                      print("Done with this round of training...!")
                      FinalParams_LIST[[length(FinalParams_LIST)+1]] <- sess$run( FinalParams_list )
                    }
                    #try(sess$close(), T) 
                    }) 
  #tf$keras$backend$clear_session()
  #tf$keras$backend$reset_uids()
  #tf$reset_default_graph()
  tf_junk <- ls()[!ls() %in% c(tf_junk, "IL_mu_last_v","IL_sigma_last_v" )]
  eval(parse(text = sprintf("rm(%s)", paste(tf_junk, collapse = ","))))
  
  for(iter_i in 1:nboot){ 
    ### Given the learned parameters, output the feature transformations for the entire matrix
    out_dfm_labeled = t( t(FinalParams_LIST[[iter_i]][[1]]) %*% ((t(as.matrix(data.table::fread(cmd = dfm_cmd$labeled_cmd))[,-1]) - IL_mu_last_v) / IL_sigma_last_v) + c(FinalParams_LIST[[iter_i]][[2]]))
    out_dfm_labeled = out_dfm_labeled/(1+abs(out_dfm_labeled))
    
    out_dfm_unlabeled = t( t(FinalParams_LIST[[iter_i]][[1]]) %*% ((t(as.matrix(data.table::fread(cmd = dfm_cmd$unlabeled_cmd))[,-1]) - IL_mu_last_v) / IL_sigma_last_v) + c(FinalParams_LIST[[iter_i]][[2]]))
    out_dfm_unlabeled = out_dfm_unlabeled/(1+abs(out_dfm_unlabeled))
    
    ### Here ends the SGD for generating optimal document-feature matrix.
    ### If we're also going to do estimation
    if(justTransform == F){ 
      ## Minimum number of observations to use in each category per bootstrap iteration
      MM1           = colMeans(out_dfm_unlabeled); 
      MM2_          = colSds(out_dfm_unlabeled,MM1);
      indices_list  = replicate(nboot_match,list( unlist( lapply(l_indices_by_cat,  function(x){sample(x, batchSizePerCat_match, 
                                                                                                       replace = length(x) * 0.75 < batchSizePerCat_match  ) }) ) ) )### Sample indices for bootstrap by category. No replacement is important here.
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
          MatchIndices_i  = try(c(FNN::get.knnx(data  = X_, 
                                                query = Y_, 
                                                k     = kMatch)$nn.index) , T) 
          
          ## Any category with less than minMatch matches includes all of that category
          t_              = table( Cat_[unique(MatchIndices_i)] ); 
          t_              = t_[t_<minMatch]
          if(length(t_) > 0){ for(t__ in names(t_)){
            MatchIndices_i = MatchIndices_i[!Cat_[MatchIndices_i] %in%  t__] ; 
            MatchIndices_i = c(MatchIndices_i,
                               sample(which(Cat_ == t__ ), 
                                      minMatch, 
                                      replace = T))
          }
          }
        }else{ ## Otherwise use all the indices
          MatchIndices_i  = 1:nrow(X_)
        }
        categoryVec_LabMatch = Cat_[MatchIndices_i]; X_m = X_[MatchIndices_i,]
        MatchIndices_byCat   = tapply(1:length(categoryVec_LabMatch),
                                      categoryVec_LabMatch, function(x){c(x) })
        
        ### Carry out estimation on the matched samples
        est_readme2_ = try((  sapply(1:nboot_match, function(eare){ 
          MatchIndices_byCat_          = lapply(MatchIndices_byCat, function(sae){ sample(sae, 
                                                                                          batchSizePerCat_match, 
                                                                                          replace = length(sae) * 0.75 < batchSizePerCat_match ) })
          X__                          = X_m[unlist(MatchIndices_byCat_),]; 
          categoryVec_LabMatch_        = categoryVec_LabMatch[unlist(MatchIndices_byCat_)]
          
          ESGivenD_sampled             = do.call(cbind, tapply(1:nrow( X__ ) , categoryVec_LabMatch_, function(x){colMeans(X__[x,])}) )
          colnames(ESGivenD_sampled)   = names(labeled_pd)
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
      rm(BOOTSTRAP_EST,indices_list) ; 
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
    temp_est_readme                            = hold_coef 
    temp_est_readme[names(est_readme2)]         = est_readme2
    boot_readme[iter_i,names(temp_est_readme)] = temp_est_readme
  }
  
  ### Close the TensorFlow session
  if(verbose==T){ cat("Finished!") }
  ## Parse output
  ## If no diagnostics wanted
  #sort( sapply(ls(),function(x){object.size(get(x))})) 
  if(diagnostics == F){return( list(point_readme = colMeans(boot_readme, na.rm = T) ) )  }
  ## If diagnostics wanted
  if(diagnostics == T){return( list(point_readme    = colMeans(boot_readme, na.rm = T) ,
                                    diagnostics     = list(OrigPrD_div         = sum(abs(labeled_pd[names(unlabeled_pd)] - unlabeled_pd)),
                                                           MatchedPrD_div      = mean(MatchedPrD_div, na.rm = T), 
                                                           OrigESGivenD_div    = mean(OrigESGivenD_div, na.rm = T), 
                                                           MatchedESGivenD_div = mean(MatchedESGivenD_div, na.rm = T))) )  }
}


#' start_reading
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
#' @param justTransform A Boolean indicating whether the user wants to extract the quanficiation-optimized 
#' features only. 
#' 
#' @param numProjections How many projections should be calculated? Input should be a positive number. Minimum number of projections = number of categories + 2. 
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
start_reading <- function(){
  eval_text = 'require(tensorflow, quietly = T)
  tf$reset_default_graph()
  G_ = tf$Graph()
  with(G_$as_default(), {
    #Assumptions 
    nDim <- as.integer( 600 ) 
    nProj = as.integer(  20  )  
    NObsPerCat = as.integer(  10 )  
    dropout_rate <- 0.50 
    
    #INPUTS 
    axis_FeatDiscrim = tf$placeholder(tf$int32)
    contrast_indices1            = tf$placeholder(tf$int32,list(NULL))
    contrast_indices2            = tf$placeholder(tf$int32,list(NULL))
    redund_indices1            = tf$placeholder(tf$int32,list(NULL))
    redund_indices2            = tf$placeholder(tf$int32,list(NULL))
    IL_input            = tf$placeholder(tf$float32,list(NULL, nDim))
    MultMat_tf          = tf$placeholder(tf$float32,list(NULL, NULL))
    L2_squared_initial      = tf$placeholder(tf$float32)
    
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
      beta__                =   runif(nDim,  -1/sqrt(nDim), 1/sqrt(nDim)  )
      dropout__             =   rbinom(nDim, size = 1, prob = dropout_rate)
      beta__[dropout__==1]  <- 0
      beta__[dropout__==0]  <- beta__[dropout__==0] / (1 - dropout_rate)
      sum(beta__) }))
    WtsMat               = tf$Variable(initializer_reweighting*tf$random_uniform(list(nDim,nProj),-1/sqrt(nDim), 1/sqrt(nDim), dtype = tf$float32),dtype = tf$float32, trainable = T)
    BiasVec              = tf$Variable(as.vector(rep(0,times = nProj)), trainable = T, dtype = tf$float32)
    
    ### Drop-out transformation
    ulim1                = -0.5 * (1-dropout_rate) / ( (1-dropout_rate)-1)
    MASK_VEC1            = tf$multiply(tf$nn$relu(tf$sign(tf$random_uniform(list(nDim,1L),-0.5,ulim1,dtype = tf$float32))), 1 / (ulim1/(ulim1+0.5)))
    WtsMat_drop          = tf$multiply(WtsMat, MASK_VEC1)
    ### Apply non-linearity + batch normalization 
    LFinal               = tf$nn$softsign( tf$matmul(IL_n, WtsMat_drop) + BiasVec)
    LFinal_m             = tf$nn$moments(LFinal, axes = 0L)
    LFinal_n             = tf$nn$batch_normalization(LFinal, mean = LFinal_m[[1]], variance = LFinal_m[[2]], offset = 0, scale = 1, variance_epsilon = 0.001)
    
    #Find E[S|D] and calculate objective function  
    ESGivenD_tf          = tf$matmul(MultMat_tf,LFinal_n)
    
    ## Spread component of objective function
    #Gather slices from params axis axis according to indices.
    #gathering_mat =   (sapply(1:nCat, function(er){ 
    #if(er == 1){indices_ =  1:NObsPerCat-1 }
    #if(er > 1){indices_ =  ((er-1)*NObsPerCat):(er*NObsPerCat-1) }
    #return(as.integer(indices_))}))
    Spread_tf            = tf$matmul(MultMat_tf,tf$square(LFinal_n)) - tf$square(ESGivenD_tf)
    
    ## Category discrimination (absolute difference in all E[S|D] columns)
    CatDiscrim_tf        = tf$minimum(tf$abs(tf$gather(ESGivenD_tf, indices = contrast_indices1, axis = 0L) -
                                               tf$gather(ESGivenD_tf, indices = contrast_indices2, axis = 0L)), 1.50)
    
    ## Feature discrimination (row-differences)
    FeatDiscrim_tf       = tf$minimum(tf$abs(tf$gather(CatDiscrim_tf,  indices = redund_indices1, axis = axis_FeatDiscrim) -
                                               tf$gather(CatDiscrim_tf, indices = redund_indices2, axis = axis_FeatDiscrim)), 1.50)
    
    ## Loss function CatDiscrim + FeatDiscrim + Spread_tf 
    myLoss_tf            = -( tf$reduce_mean(CatDiscrim_tf) + 
                                tf$reduce_mean(FeatDiscrim_tf) + 
                                0.10 * tf$reduce_mean(Spread_tf) )
    
    ### Initialize an optimizer using stochastic gradient descent w/ momentum
    my_optimizer             = tf$train$MomentumOptimizer(learning_rate = sgd_learning_rate,
                                                          momentum      = 0.90, use_nesterov  = T)
    
    ### Calculates the gradients from myOpt_tf
    Gradients_unclipped  = my_optimizer$compute_gradients( myLoss_tf ) 
    Gradients_clipped    = Gradients_unclipped
    TEMP__               = tf$clip_by_global_norm(list(Gradients_unclipped[[1]][[1]],
                                                        Gradients_unclipped[[2]][[1]]),clip_tf)
    for(jack in 1:length(Gradients_clipped)){ Gradients_clipped[[jack]][[1]] = TEMP__[[1]][[jack]] } 
    L2_squared_clipped   = tf$reduce_sum(tf$square(Gradients_clipped[[1]][[1]])) + 
                                      tf$reduce_sum(tf$square(Gradients_clipped[[2]][[1]]))
    inverse_learning_rate_update = tf$assign_add(ref = inverse_learning_rate, value = L2_squared_clipped / inverse_learning_rate)
    
    ### applies the gradient updates
    myOpt_tf_apply       = my_optimizer$apply_gradients( Gradients_clipped )
    
    #learning consists of gradient updates plus learning rate updates. 
    learning_group       = list(  inverse_learning_rate_update, myOpt_tf_apply)
    
    # Initialize variables in TensorFlow Graph
    init = tf$variables_initializer(list(WtsMat, BiasVec,clip_tf,inverse_learning_rate,
                                         my_optimizer$get_slot(tf$trainable_variables()[[1]],my_optimizer$get_slot_names()),
                                         my_optimizer$get_slot(tf$trainable_variables()[[2]],my_optimizer$get_slot_names())))
    
    #other actions 
    FinalParams_list        = list(WtsMat, BiasVec)
    setclip_action          = clip_tf$assign(  0.50 * sqrt( L2_squared_initial )  )
    restart_action          = inverse_learning_rate$assign(  0.50 *  L2_squared_initial )
  })
  G_$finalize()
  
  sess <- tf$Session(graph = G_,
           config = tf$ConfigProto(
  allow_soft_placement = TRUE 
  #device_count=list("GPU"=0L, "CPU" = nCores), 
  #inter_op_parallelism_threads = nCores,intra_op_parallelism_threads = nCores
  ))'
  eval(parse(text=eval_text), envir = globalenv())
}
