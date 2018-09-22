#' readme
#' 
#' Implements the quantification algorithm described in Jerzak, King, and Strezhnev (2018) which is meant to improve on the ideas in Hopkins and King (2010).
#' Employs the Law of Total Expectation in a feature space that is crafted to minimize the error of the resulting estimate. Automatic differentiation, stochastic gradient descent, and batch re-normalization are used to carry out the optimization.
#' Takes an inputs (a.) a vector holding the raw documents (1 entry = 1 document), (b.) a vector indicating category membership 
#' (with \code{NA}s for the unlabeled documents), and (c.) a vector indicating whether the labeled or unlabeled status of each document. 
#' Other options exist for users wanting more control over the pre-processing protocol (see \code{undergrad} and the \code{dfm} parameter}).
#' 
#' @param documentText A vector in which each entry corresponds to a document. The function will automatically ``clean'' the text. For 
#' more control over the cleaning process, users should pre-process the text themselves, use the \code{undergrad} function, and leave the ``documentText'' parameter \code{NULL}. 
#' 
#' @param labeledIndicator An indicator vector where each entry corresponds to a row in \code{dfm}. 
#' \code{1} represents document membership in the labeled class. \code{0} represents document membership in the unlabeled class. 
#' 
#' @param categoryVec An factor vector where each entry corresponds to the document category. 
#' The entires of this vector should correspond with the rows of \code{dtm}. If \code{wordVecs_corpus}, \code{wordVecs_corpusPointer}, and \code{dfm} are all \code{NULL}, 
#' \code{readme} will download and use the \code{GloVe} 50-dimensional embeddings trained on Wikipedia. 
#' 
#' @param wordVecs_corpus A data.table object in which the first column holds the text of each word, 
#' and in which the remaining columns contain the numerical representation. Either \code{wordVecs_corpus} or 
#' \code{wordVecs_corpusPointer} should be null. If \code{wordVecs_corpus}, \code{wordVecs_corpusPointer}, and \code{dfm} are all \code{NULL}, 
#' \code{readme} will download and use the \code{GloVe} 50-dimensional embeddings trained on Wikipedia. 
#' 
#' 
#'@param dfm 'document-feature matrix'. A data frame where each row represents a document and each column a unique feature. Note that 
#'this parameter should be \code{NULL} if the user is supplying the raw document text into \code{readme} (i.e. \code{documentText} is not null).
#'#'
#' @param nboot A scalar indicating the number of times the estimation will be re-run (useful for reducing the variance of the final output).
#'
#' @param verbose Should diagnostic plots be displayed? 
#'  
#' @param sgd_iters How many stochastic gradient descent iterations should be used?   
#'  
#' @param justTransform A Boolean indicating whether the user wants to extract the quanficiation-optimized 
#' features only.  
#' 
#' @return A list consiting of \itemize{
#'   \item estimated category proportions in the unlabeled set (\code{point_readme});
#'   \item the transformed dfm optimized for quantification (\code{transformed_dfm}); 
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
#' my_wordVecs_corpus <- data.frame(matrix(rnorm(11*25), ncol = 25))
#' my_wordVecs_corpus <- cbind(c("the","true", "thine", "stars", "are" , "fire", ".", "to", "own", "self", "be"), my_wordVecs_corpus)
#' my_wordVecs_corpus <- data.table::as.data.table(my_wordVecs_corpus)
#' 
#' #Generate 100 ``documents'' of between 5-10 words each. 
#' my_documentText <- replicate(100, paste(sample(my_wordVecs_corpus[[1]], sample(5:10, 1)), collapse = " ") ) 
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
#' #perform estimation
#' readme_results <- readme(documentText = my_documentText,
#'        labeledIndicator= my_labeledIndicator, 
#'        categoryVec = my_categoryVec, 
#'        wordVecs_corpus = my_wordVecs_corpus,
#'        nboot = 1)
#'print(readme_results$point_readme)
#'
#' @export 
#' 
#' 

readme <- function(documentText = NULL, 
                    labeledIndicator, categoryVec, 
                    wordVecs_corpus = NULL, dfm = NULL, nboot = 10,  sgd_iters = 1000, 
                    verbose=F, diagnostics = F, justTransform = F){ 
  if(!is.null(documentText)){ dfm <- undergrad(documentText, wordVecs_corpus = wordVecs_corpus)  } 
    
  #load in tensorflow only, use packageName::packageFunction notation otherwise. 
  library2("tensorflow", loadin = T);library2("limSolve", loadin = F)
  r_clip_by_value = function(x, a, b){x[x<=a] <- a;x[x>=b] <- b;return(x)}
  
  #Winsorization
  dfm <- apply(dfm, 2, function(x){ 
    sum_x <- summary(x); qr_ <- 1.5*diff(sum_x[c(2,5)]);
    x[x < sum_x[2]- qr_] <-sum_x[2]- qr_; x[x > sum_x[5]+qr_] <- sum_x[5] + qr_; 
    return( x ) })
  dfm <- dfm[,apply(dfm,2,sd)>0]
  
  #setup things for iterations 
  categoryVec_unlabeled <- as.factor(categoryVec)[labeledIndicator == 0]
  categoryVec_labeled <- as.factor(categoryVec)[labeledIndicator == 1]
  labeled_pd <- vec2prob( categoryVec_labeled ); unlabeled_pd <- vec2prob( categoryVec_unlabeled )
  dfm_labeled <- dfm[labeledIndicator==1,]; dfm_unlabeled <- dfm[labeledIndicator==0,]
  nCat <- as.integer( length(labeled_pd) ); nDim <- as.integer( ncol(dfm_labeled) )  

  #controlling parameters for sgd
  NObsByCat = rep(min(r_clip_by_value(as.integer( round( sqrt(  nrow(dfm_labeled)*labeled_pd))),3,10)), nCat)
  nProj <- as.integer(max(20,nCat+2) );
  
  #set up sdg 
  { 
    tf$reset_default_graph(); sess <- tf$Session()
    contrasts_mat <-  combn(1:nCat, 2) - 1; contrast_indices1 <- as.integer(contrasts_mat[1,]); contrast_indices2 <- as.integer(contrasts_mat[2,])
    redund_mat <- combn(1:nProj, 2) - 1; redund_indices1 <- as.integer(redund_mat[1,]); redund_indices2 <- as.integer(redund_mat[2,])
    axis_FeatDiscrim = as.integer(nCat!=2)
    
    #set placeholders 
    sdg_learning_rate = tf$placeholder(tf$float32, shape = c())
    dmax = tf$placeholder(tf$float32, shape = c()); rmax = tf$placeholder(tf$float32, shape = c())
    NObsByCat_cumsum = cumsum(NObsByCat)
  
    MultMat = t(do.call(rbind,sapply(1:nCat,function(x){
      urat = 0.01; uncertainty_amt = urat / ( (nCat - 1 ) * urat + 1  );
      MM = matrix(uncertainty_amt, nrow = NObsByCat[x],ncol = nCat); MM[,x] = 1-(nCat-1)*uncertainty_amt
      return( list(MM) )  } )) )
    MultMat = MultMat  / rowSums( MultMat )
    MultMat_tf = tf$constant(MultMat, dtype = tf$float32)
    list_indices_by_cat <- tapply(1:length(categoryVec_labeled), categoryVec_labeled, c)
      
    #SET UP INPUTS 
    IL_input <-  tf$placeholder(tf$float32, shape = list(sum(NObsByCat), nDim))
    IL = IL_input 
    { #batch renormalization for inputs 
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
    
    #SET UP WEIGHTS 
    WtsMat = tf$Variable(tf$random_uniform(list(nDim,nProj),-1/sqrt(nDim+nProj), 1/sqrt(nDim+nProj)),dtype = tf$float32, trainable = T)
    BiasVec = tf$Variable(as.vector(rep(0,times = nProj)), trainable = T, dtype = tf$float32)
    dropout_rate1 = 0.50
    ulim1 = -0.5 * (1-dropout_rate1) / ( (1-dropout_rate1)-1)
    MASK_VEC1 <- tf$multiply(tf$nn$relu(tf$sign(tf$random_uniform(list(nDim,1L),-0.5,ulim1))), 1 / (ulim1/(ulim1+0.5)))
    
    dropout_rate2 = 1 - (1 - 0.51) /  (1 -  dropout_rate1 );
    ulim2 = -0.5 * (1-dropout_rate2) / ( (1-dropout_rate2)-1);
    MASK_VEC2 <- tf$multiply(tf$nn$relu(tf$sign(tf$random_uniform(list(nDim,nProj),-0.5,ulim2))), 1 / (ulim2/(ulim2+0.5)))
    WtsMat_drop = tf$multiply(WtsMat, tf$multiply(MASK_VEC1,MASK_VEC2))
    nonLinearity_fxn = function(x){tf$nn$softsign(x)}
    LFinal = nonLinearity_fxn(tf$matmul(IL_n, WtsMat_drop) + BiasVec)
    { #batch renormalization for output  
      LFinal_m = tf$nn$moments(LFinal, axes = 0L);
      LF_mu_b = LFinal_m[[1]]; 
      LF_sigma2_b = LFinal_m[[2]];
      LF_sigma_b = tf$sqrt(LF_sigma2_b)
      LF_mu_last = tf$placeholder( tf$float32,shape(dim(LF_mu_b)) )
      LF_sigma_last = tf$placeholder( tf$float32,shape(dim(LF_sigma_b)) )
      LFinal_n = tf$nn$batch_normalization(LFinal, mean = LF_mu_b, variance = LF_sigma2_b, offset = 0, scale = 1, variance_epsilon = 0.001)
    }
    
    #Find E[S|D] and calculate objective function  
    ESGivenD_tf = tf$matmul(MultMat_tf,LFinal_n)
    Spread_tf = tf$sqrt(tf$maximum(tf$matmul(MultMat_tf,tf$square(LFinal_n)) - tf$square(ESGivenD_tf), 0.001))
    CatDiscrim_tf = tf$abs(tf$gather(ESGivenD_tf, indices = contrast_indices1, axis = 0L) - tf$gather(ESGivenD_tf, indices = contrast_indices2, axis = 0L))
    FeatDiscrim_tf = tf$abs(tf$gather(CatDiscrim_tf,  indices = redund_indices1, axis = axis_FeatDiscrim) - tf$gather(CatDiscrim_tf, indices = redund_indices2, axis = axis_FeatDiscrim))
    myLoss_tf = -(tf$reduce_mean(CatDiscrim_tf)+tf$reduce_mean(FeatDiscrim_tf)  + 1 * tf$reduce_mean(tf$log(0.01+Spread_tf) ) )
    myOptimizer_tf = tf$train$MomentumOptimizer(learning_rate=sdg_learning_rate,momentum = 0.90,use_nesterov = T)
    myGradients = myOptimizer_tf$compute_gradients(myLoss_tf)
    L2_squared_unclipped =  eval(parse( text = paste(sprintf("tf$reduce_sum(tf$square(myGradients[[%s]][[1]]))", 1:length(myGradients)), collapse = "+") ) )
    clip_tf =  tf$placeholder(tf$float32, shape = list()); 
    TEMP__ = eval(parse(text=sprintf("tf$clip_by_global_norm(list(%s),clip_tf)",paste(sprintf('myGradients[[%s]][[1]]', 1:length(myGradients)), collapse = ","))))
    for(jack in 1:length(myGradients)){ myGradients[[jack]][[1]] = TEMP__[[1]][[jack]] } 
    L2_squared =  eval(parse( text = paste(sprintf("tf$reduce_sum(tf$square(myGradients[[%s]][[1]]))", 1:length(myGradients)), collapse = "+") ) )
    myOptimizer_tf_apply = myOptimizer_tf$apply_gradients( myGradients )

    #updates for next iteration  
    Moments_learn <- 0.01
    IL_mu_ = Moments_learn  * IL_mu_b +  (1-Moments_learn) * IL_mu_last; 
    IL_sigma_ = Moments_learn * IL_sigma_b + (1-Moments_learn) * IL_sigma_last
    
    LF_mu_ = Moments_learn * LF_mu_b + (1-Moments_learn) * LF_mu_last;
    LF_sigma_ = Moments_learn * LF_sigma_b + (1-Moments_learn) * LF_sigma_last
    
    #Setup the outputs 
    OUTPUT_LFinal = nonLinearity_fxn( tf$matmul(OUTPUT_IL_n, WtsMat) + BiasVec )
    OUTPUT_LFinal_n = tf$nn$batch_normalization(OUTPUT_LFinal, mean = LF_mu_last,variance = tf$square(LF_sigma_last), offset = 0, scale = 1, variance_epsilon = 0)
    
    #set up initializer 
    init = tf$global_variables_initializer()
  }

  #set up holding containers 
  boot_readme <- matrix(nrow=nboot, ncol=nCat);colnames(boot_readme) <- names(labeled_pd)
  hold_coef <- rep(0, nCat); names(hold_coef) <-  names(labeled_pd)
  MatchedPrD_div <- OrigESGivenD_div <- MatchedESGivenD_div <- rep(NA, times = nboot)
  for(iter_i in 1:nboot){ 
  {
      sess$run(init)#initialize parameters and gather up useful statistics
      sgd_grabSamp = function(){ unlist(sapply(1:nCat, function(ze){  sample(list_indices_by_cat[[ze]], NObsByCat[ze], replace = T )  } ))}
      update_ls <- list() 
      update_ls[[1]] =  c(rowMeans(  replicate(50, sess$run(IL_mu_b,  feed_dict = dict(IL_input = dfm_labeled[sgd_grabSamp(),],#rmax = 1, dmax = 0,
                                            IL_mu_last =  rep(0, times = ncol(dfm_labeled)), IL_sigma_last = rep(1, times = ncol(dfm_labeled)) ) )) )  )
      update_ls[[2]] =  c(rowMeans( replicate(50, sess$run(IL_sigma_b,  feed_dict = dict(IL_input = dfm_labeled[sgd_grabSamp(),],#rmax = 1, dmax = 0,
                                                                         IL_mu_last =  rep(0, times = ncol(dfm_labeled)), IL_sigma_last = rep(1, times = ncol(dfm_labeled)) ) )) )  )
      init_L2_squared_vec = unlist(replicate(50,sess$run(list(L2_squared_unclipped),  
                           feed_dict = dict(IL_input = dfm_labeled[sgd_grabSamp(),],rmax = 1, dmax = 0,IL_mu_last =  rep(0, times = ncol(dfm_labeled)), IL_sigma_last = rep(1, times = ncol(dfm_labeled))) )))
      clip_value =  0.5*median(sqrt(init_L2_squared_vec))
      m1 <-inverse_learning_rate_vec <- rep(NA, times = sgd_iters) 
      inverse_learning_rate <- 0.5 * median( init_L2_squared_vec ) 
      for(awer in 1:sgd_iters){
        update_ls = sess$run(list( IL_mu_,IL_sigma_, L2_squared, myOptimizer_tf_apply),
                             feed_dict = dict(IL = dfm_labeled[sgd_grabSamp(),],sdg_learning_rate = 1/inverse_learning_rate,
                                              clip_tf = clip_value,IL_mu_last =  update_ls[[1]], IL_sigma_last = update_ls[[2]]))
        inverse_learning_rate_vec[awer] <- inverse_learning_rate <- inverse_learning_rate + update_ls[[3]] / inverse_learning_rate
      }
      out_dfm = try(sess$run(OUTPUT_LFinal,feed_dict = dict(OUTPUT_IL = rbind(dfm_labeled, dfm_unlabeled), IL_mu_last =  update_ls[[1]], IL_sigma_last = update_ls[[2]])), T)
      out_dfm_labeled <- out_dfm[1:nrow(dfm_labeled), ]; out_dfm_unlabeled <- out_dfm[-c(1:nrow(dfm_labeled)),]

      if(justTransform == F){ 
        if(verbose == T){ 
          par(mfrow = c(2,1)); plot_indices <- sample(1:nProj, 2);
          xlim_ <- c(min(out_dfm[,plot_indices[1]]),max(out_dfm[,plot_indices[1]])); ylim_ <- c(min(out_dfm[,plot_indices[2]]),max(out_dfm[,plot_indices[2]]))
          plot(out_dfm_labeled[,plot_indices],col = as.factor(categoryVec_labeled),
               xlab = "Projection 1", ylab = "Projection 2", 
               main = "Labeled Set Results",
               xlim =xlim_,ylim=ylim_, cex = 1.5, pch = as.numeric(as.factor(categoryVec_labeled)))
          legend("bottomleft", 
                 col = unique(as.factor(categoryVec_labeled)),
                 pch = unique(as.numeric(as.factor(categoryVec_labeled))), 
                 legend = unique(as.character(categoryVec_labeled)),
                 cex = 2/length(unique(categoryVec_labeled)))
          pch_unlabeled = as.numeric(as.factor(categoryVec_unlabeled)); pch_unlabeled[is.na(pch_unlabeled)] <- 1
          col_unlabeled = as.numeric(as.factor(categoryVec_unlabeled)); col_unlabeled[is.na(col_unlabeled)] <- 1
          plot(out_dfm_unlabeled[,plot_indices],col = as.factor(col_unlabeled),xlim =xlim_,ylim=ylim_,
               xlab = "Projection 1", ylab = "Projection 2", 
               main = "Unlabeled Set Results", cex = 1.5,  pch = pch_unlabeled)
          par(mfrow = c(1,1));
        }
        if(T == T){ 
          min_size <- min(r_clip_by_value(as.integer( round( 0.90*(  nrow(dfm_labeled)*labeled_pd) )),10,100))
          nRun = 50; k_match = 3
          indices_list = replicate(nRun,list( unlist( lapply(list_indices_by_cat, function(x){sample(x, min_size, replace = T) }) ) ) )
          BOOTSTRAP_EST <- sapply(1:nRun, function(boot_iter){ 
            indi_i = indices_list[[boot_iter]]; 
            Cat_ = categoryVec_labeled[indi_i]; X_ = out_dfm_labeled[indi_i,];Y_ = out_dfm_unlabeled
            { 
              MM1 = colMeans(Y_); 
              get_SD1 <- get_SD2 <- function(x,y){min( max(x,y,1/6),6)}
              MM2 = sapply(1:ncol(X_), function(x){x1 = sd(X_[,x]); x2 = sd(Y_[,x]); get_SD1(x1,x2) } )
              X_ = FastScale(X_, MM1, MM2); Y_ = FastScale(Y_, MM1, MM2); 
              
              MatchIndices_i <- knn_adapt(reweightSet = X_, fixedSet = Y_, k = k_match)$return_indices
              t_ = table( Cat_[MatchIndices_i] ) ; t_ = t_[t_<5]
              if(length(t_) > 0){ for(t__ in names(t_)){MatchIndices_i = MatchIndices_i[!Cat_[MatchIndices_i] %in%  t__] ; MatchIndices_i = c(MatchIndices_i,which(Cat_ == t__ )) }}
              categoryVec_labeled_matched = Cat_[MatchIndices_i]; X_ = X_[MatchIndices_i,]
              matched_list_indices_by_cat <- tapply(1:length(categoryVec_labeled_matched), categoryVec_labeled_matched, function(x){c(x) })

              min_size2 <- min(r_clip_by_value(unlist(lapply(matched_list_indices_by_cat, length))*0.90,10,100))
              est_readme2 = rowMeans(  replicate(30, { 
                matched_list_indices_by_cat_ = lapply(matched_list_indices_by_cat, function(sae){ sample(sae, min_size2, replace = T) })
                X_ = X_[unlist(matched_list_indices_by_cat_),]; categoryVec_labeled_matched_sampled = categoryVec_labeled_matched[unlist(matched_list_indices_by_cat_)]
                MM1_samp = colMeans(Y_);MM2_samp = sapply(1:ncol(X_), function(x){x1 = sd(X_[,x]); x2 = sd(Y_[,x]); get_SD2(x1,x2) } )
                X_ = FastScale(X_, MM1_samp, MM2_samp); 
                Y_ = FastScale(Y_, MM1_samp, MM2_samp)
                ESGivenD_sampled = do.call(cbind, tapply(1:length( categoryVec_labeled_matched_sampled ) , categoryVec_labeled_matched_sampled, function(x){colMeans(X_[x,])}) ) 
                try(readme_est_fxn(X = ESGivenD_sampled, Y = colMeans(Y_))[names(labeled_pd)],T) } ) )  
              } 
              list(est_readme2 = est_readme2) 
          } )
        } 
        est_readme2 <- rowMeans(do.call(cbind,BOOTSTRAP_EST), na.rm = T)
        #sum(abs(est_readme2-unlabeled_pd))
        tf_est_results <- list(est_readme2 = est_readme2, transformed_unlabeled_dfm = out_dfm_unlabeled,
                               transformed_labeled_dfm = list(unmatched_transformed_labeled_dfm = cbind(as.character(categoryVec_labeled), out_dfm_labeled),matched_transformed_labeled_dfm = cbind(as.character(categoryVec_labeled), out_dfm_labeled)))
      }
      if(justTransform == T){ tf_est_results <- list(transformed_unlabeled_dfm = out_dfm_unlabeled,transformed_labeled_dfm = list(unmatched_transformed_labeled_dfm = cbind(as.character(categoryVec_labeled), out_dfm_labeled)) ) }    
    }
    if(iter_i == 1){ 
      transformed_dfm <- dfm[,1:(ncol(tf_est_results$transformed_unlabeled_dfm))]; transformed_dfm[] <- NA
      transformed_dfm[which(labeledIndicator==1),] <- apply(tf_est_results$transformed_labeled_dfm$unmatched_transformed_labeled_dfm[,-1], 2, f2n)
      transformed_dfm[which(labeledIndicator==0),] <- apply(tf_est_results$transformed_unlabeled_dfm, 2, f2n)
      if(justTransform == T){sess$close(); return(list(transformed_dfm=transformed_dfm))} 
    }
    est_readme <- tf_est_results$est_readme
    temp_est_readme <- hold_coef 
    temp_est_readme[names(est_readme)] <- est_readme
    boot_readme[iter_i,names(temp_est_readme)] <- temp_est_readme
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
  sess$close()
  if(diagnostics == F){return( list(point_readme=colMeans(boot_readme, na.rm = T) , transformed_dfm = transformed_dfm) )  }
  if(diagnostics == T){return( list(point_readme = colMeans(boot_readme, na.rm = T) ,
                                    transformed_dfm = transformed_dfm, 
                                    diagnostics = list(OrigPrD_div = sum(abs(labeled_pd[names(unlabeled_pd)] - unlabeled_pd)),
                                    MatchedPrD_div = mean(MatchedPrD_div, na.rm = T), 
                                    OrigESGivenD_div = mean(OrigESGivenD_div, na.rm = T), 
                                    MatchedESGivenD_div = mean(MatchedESGivenD_div, na.rm = T))) )  }
}

