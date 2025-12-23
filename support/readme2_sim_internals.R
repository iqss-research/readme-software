devtools::install_github("iqss-research/readme-software/readme")
library(readme)
initialize_tensorflow()
numericize_text     = TRUE
compute_readme2     = TRUE
compute_splits      = TRUE
compute_classifiers = TRUE
use_RCE = FALSE
labeled_sz_ <- 300; unlabeled_sz_ <- 300;  nboot <- 2; my_seed = 1 

support_files <- list.files("./support/")
support_files <- support_files[! support_files %in% c("readme2_sim_internals.R")]
for(support_file in support_files){ source(sprintf("./support/%s", support_file)) }

wordVecs_corpus <- data.table::fread(wordVecs_pointer)
wordVecs_rowNames <- wordVecs_corpus[[1]]
wordVecs_corpus <- as.matrix (  wordVecs_corpus[,-1] )  
row.names(wordVecs_corpus) <- wordVecs_rowNames
rm(wordVecs_rowNames)

eval_discrete <- 'try( DiscreteTest(
dtm=as.matrix(data.table::fread(cmd = dtm_pastein )[,-1])[,colMeans(data.table::fread(cmd = dtm_pastein )[,-1])>0.01], 
labeledIndicator=iter_labeledIndicator, 
categoryVec=iter_categoryVec,
compareQuantifiers = TRUE), TRUE)'

eval_continuous <- 'try( ContinuousTest(
dfm=as.matrix(data.table::fread(cmd = dfm_pastein )[,-1]), 
labeledIndicator=iter_labeledIndicator, 
categoryVec=iter_categoryVec, 
nboot = nboot), TRUE)'

### List all of the Crimson Hexagon CSVs
csv_names <- list.files("./validationdata/HandcodedCSVs")
support_files_numericize <- c("readme2_numericize_helper_fxns.R")
support_files_samp <- c("readme2_sampling.R","readme2_sampling_helper_fxns.R")
support_files_class <- c("readme2_testing.R","readme2_testing_helper_fxns.R")

for(support_file in support_files_numericize){source(sprintf("./support/%s", support_file)) }
for(support_file in support_files_samp){source(sprintf("./support/%s", support_file)) }
for(support_file in support_files_class){source(sprintf("./support/%s", support_file)) }
filename_saved_data = "results"
filename_saved_results = "results"
filename_support = "support"


csv_keys <- grep(list.files("./data/"), pattern = "\\.csv", value = TRUE)
csv_keys <- sample(csv_keys)
counter_ <- 0 
global_iter_seq <- 1:length(csv_keys)
for(ijack in global_iter_seq){
    counter_ <- counter_ +1 
    print( sprintf("Dataset %s - %s", c(counter_),gsub(csv_keys[ijack], pattern = "\\.csv", replacement = "")  ) )
    
    ### Initialize placeholder for storing the errors
    errors <- data.frame()
    indices_list <- list()

    ### Accounting 
    out_file <- gsub(csv_keys[ijack], pattern = "\\.csv", replacement = "_results.csv")
    #csv_name <- gsub(csv_keys[ijack], pattern = "\\.csv", replacement = "")
    csv_name = csv_keys[ijack]
    in_file <- cbind(read.csv(sprintf("./data/%s", csv_keys[ijack]), header = TRUE), 1)
    colnames(in_file)[3] <- "LABELEDSET"
    in_file[,2] <- enc2utf8(as.character(in_file[,2]))
    
      print(counter_)
      ### Initialize placeholder for storing the errors
      errors <- data.frame()
      
      dtm_loc  = paste(sprintf("%s/DTM_",filename_saved_results), csv_name, sep = "")
      dfm_loc  = paste(sprintf("%s/DFM_",filename_saved_results), csv_name, sep = "")
      cats_loc = paste(sprintf("%s/CATS_",filename_saved_results), csv_name, sep = "")
      
      if(numericize_text == FALSE){ 
        corpus_categoryVec <- try(c(as.character(read.csv(file = cats_loc)[,-1])), TRUE)
      }
      
      if(numericize_text == TRUE){ 
        {
          #creat DTM and DFM 
          my_text = cleanme(in_file[,2])
          corpus_DTM <- toDTM(my_text)
          corpus_categoryVec = in_file[,1]
          
          docSummaries <- FastScale( undergrad(my_text,  wordVecs = wordVecs_corpus))
          rm(my_text);
          if(use_RCE == TRUE){rm(wordVecs_corpus)}
          
          ## Make "CATEGORY" coding a factor variable
          corpus_categoryVec <- gsub(as.character(corpus_categoryVec), pattern = "[[:punct:]]", replacement = "_")
          corpus_categoryVec <- gsub(corpus_categoryVec, pattern = "[[:space:]]", replacement = "_")
          corpus_categoryVec <- as.factor(corpus_categoryVec)
          
          #save for precomputations 
          write.csv(corpus_DTM, file = paste("./", filename_saved_data, "/DTM_", csv_name, sep = ""))
          write.csv(docSummaries, file = paste("./", filename_saved_data, "/DFM_", csv_name, sep = ""))
          write.csv(as.matrix(corpus_categoryVec), file = paste("./", filename_saved_data, "/CATS_", csv_name, sep = ""))
          
    
          rm(corpus_DTM,docSummaries); 
        }
      } 
      
      compute_classifiers_ = compute_classifiers
      compute_splits_ = compute_splits 
      if(compute_splits == TRUE & compute_classifiers == FALSE){ 
        #If we allowed this, then errors would no longer correspond to the same data split. 
        stop("INCONSISTENT SIMULATION SPECIFICATIONS")  
      }
      if(compute_splits == FALSE){ 
        try_splits = try(load(sprintf(paste("./", filename_saved_data, "/SPLITS_%s.RData",sep = ""),
                                      gsub(csv_name, pattern = ".csv", replacement = ""))), TRUE)
        if(class(try_splits) == "try-error"){
          compute_splits_ <- TRUE
        }
        if(class(try_splits) != "try-error"){
          if(length(indices_list) == iterations){
            compute_splits_ <- FALSE
          }
          if(length(indices_list) != iterations){
            compute_classifiers_ <- compute_splits_ <- TRUE 
          }
        }
      }
      
      if(compute_splits == TRUE){
        samp_fxns_added <- ls(); for(support_file in support_files_samp){source(sprintf("./%s/%s",filename_support, support_file)) }
        samp_fxns_added <- ls()[!ls() %in% samp_fxns_added]
        csv_error <- rep(NA, length=iterations)
        sampling_scheme_used <- rep(NA, times = iterations)
        if(sampling_scheme != "BalancedCombination"){ProjectionsMat = NULL }
        if(sampling_scheme == "BalancedCombination"){
          iter_labeledIndicator = c(rep(1,times = 300),rep(0,times=length(corpus_categoryVec)-300))
          selected_indices = 1:length(corpus_categoryVec)
          selected_indices = selected_indices[corpus_categoryVec[selected_indices] %in% 
                                                unique(corpus_categoryVec[selected_indices[iter_labeledIndicator==1]])]
          iter_labeledIndicator = iter_labeledIndicator[selected_indices]
          iter_corpus_categoryVec = as.character( corpus_categoryVec[selected_indices] )  
          dfm_pastein_labeled  = paste0("sed -n '" , paste0(  c(1,selected_indices[iter_labeledIndicator==1] + 1) , 
                                                              collapse = "p\n " ) ,"p' ",dfm_loc, collapse = "" )
          dfm_pastein_unlabeled  = paste0("sed -n '" , paste0(  c(1,selected_indices[iter_labeledIndicator==0] + 1) , 
                                                                collapse = "p\n " ) ,"p' ",dfm_loc, collapse = "" )
          ProjectionsMat <- readme(dfm =  list(labeled_cmd   = dfm_pastein_labeled,
                                               unlabeled_cmd = dfm_pastein_unlabeled),
                                   labeledIndicator = iter_labeledIndicator, 
                                   categoryVec = iter_corpus_categoryVec, 
                                   nCores = nCores, 
                                   nBoot = 1, justTransform = TRUE)$transformed_dfm
        }
        indices_list <- list()
        for(it in 1:iterations){ 
          cat(paste("Generating Dataset :", it, "\n"))
          sampling_scheme_old <- sampling_scheme
          if(sampling_scheme == "BalancedCombination"){
            if(it == 1){ 
              sampling_scheme_vec <- c( "Max_XDiv", "Min_XDiv","Uniform_XDiv" , 
                                        "Historical","Sequential", "HistoricalMaxU", "HistoricalMaxL","HistoricalVarySampSize",
                                        "Ahistorical_NoReuse","firat2018",
                                        "breakdown_sampling","breakdown_sampling2", 
                                        "JustPermutations", "MinDiv", "MaxDiv",
                                        "Ahistorical_NoReuse2","Ahistorical_aykut","Ahistorical_aykut_quantification")
              sampling_scheme_vec <- sample(sampling_scheme_vec)
              sampling_scheme_vec <- rep(sampling_scheme_vec, times = ceiling( iterations/length(sampling_scheme_vec)) ) 
            }
            sampling_scheme <- sampling_scheme_vec[it]
          }
          labeled_sz_checked = labeled_sz_; unlabeled_sz_checked = unlabeled_sz_
          target_sz = labeled_sz_+unlabeled_sz_
          if(target_sz >= length(corpus_categoryVec)){ 
            frac_seq = seq(0.01, 0.99, length.out = 100)
            cand_sz = sapply(frac_seq, 
                             function(xa){ xa  * target_sz})
            labeled_sz_checked = round(labeled_sz_ * frac_seq[which.min(abs(cand_sz - length(corpus_categoryVec) + 3))][1])
            unlabeled_sz_checked = round(unlabeled_sz_ * frac_seq[which.min(abs(cand_sz - length(corpus_categoryVec) + 3))][1])
          }
          while_ok <- FALSE; while_ok_counter = 0
          while(while_ok == FALSE){ 
            if(while_ok_counter > 0){print(sprintf("BAD: %s", while_ok_counter))}
            while_ok_counter = while_ok_counter + 1
            INDICES_LIST = try(GetLabeledUnlabeledIndices( csv_category = as.factor(corpus_categoryVec),
                                                           sampling_scheme = sampling_scheme, 
                                                           labeled_sz = labeled_sz_checked, unlabeled_sz = unlabeled_sz_checked,
                                                           ProjectionsMat_input = ProjectionsMat ), TRUE)  
            if(length(unique(corpus_categoryVec[INDICES_LIST[[1]]])) > 1){while_ok = TRUE}
            if(while_ok_counter > 10){while_ok = FALSE; INDICES_LIST <- try("a"+ 2, TRUE)}
            
          } 
          if(class(INDICES_LIST) == "try-error"){ 
            if(use_RCE == FALSE){print("SAMPLING ERROR, DEFAULTING TO HISTORICAL");browser()}
            INDICES_LIST = try(GetLabeledUnlabeledIndices( csv_category = corpus_categoryVec,
                                                           sampling_scheme = "Historical", 
                                                           labeled_sz = labeled_sz_checked, unlabeled_sz = unlabeled_sz_checked, 
                                                           ProjectionsMat_input = ProjectionsMat), TRUE)  
          }
          labeled_indices <- INDICES_LIST$labeled_indices
          unlabeled_indices <- INDICES_LIST$unlabeled_indices
          LC <- corpus_categoryVec[INDICES_LIST$labeled_indices]
          UC <- corpus_categoryVec[INDICES_LIST$unlabeled_indices]
          LC_CATS_KEEP <- table( LC); LC_CATS_KEEP <- names(LC_CATS_KEEP)[LC_CATS_KEEP>10]
          unlabeled_indices <- unlabeled_indices[UC %in% LC_CATS_KEEP]
          labeled_indices <- labeled_indices[LC %in% LC_CATS_KEEP]
          indices_list[[it]] <- list(labeled_indices = labeled_indices, 
                                     unlabeled_indices = unlabeled_indices)
          sampling_scheme_used[it] <- sampling_scheme
          sampling_scheme <- sampling_scheme_old
        }
        indices_list <- indices_list[unlist(lapply(indices_list, function(as){ 
          x_ = try(min(length(as[[1]]), length(as[[2]])), TRUE)
          if(class(x_) == "try-error"){x_ <- 0}
          return( x_ ) })) > 0]
        names(indices_list) <- sampling_scheme_used
        save(indices_list, file = sprintf("./%s/SPLITS_%s.RData",filename_saved_data ,
                                          gsub(csv_name, pattern = ".csv", replacement = "")))
        rm(ProjectionsMat,indices_list)
      } 
      
      #first do classifier + quantifier analysis 
      try(load(sprintf("./%s/SPLITS_%s.RData",filename_saved_data ,
                       gsub(csv_name, pattern = ".csv", replacement = ""))), TRUE)
      if(compute_classifiers == FALSE){
        test_ = try(sum(is.na(as.data.frame(data.table::fread(sprintf("./%s/%s", filename_saved_results, out_file) ))[,-1]["continuousSVM_error"] )) > 0, TRUE)
        if(class(test_) == "try-error"){ compute_classifiers_ <- TRUE } 
        if(class(test_) != "try-error"){ 
          try( if(test_ == 0){  compute_classifiers_ <- FALSE } , TRUE)  
          if(test_ != 0){  compute_classifiers_ <- TRUE }
        }
      } 
      
      if(compute_classifiers_ == TRUE){ 
        class_fxns_added <- ls(); for(support_file in support_files_class){source(sprintf("./%s/%s", filename_support,support_file)) }
        class_fxns_added <- ls()[!ls() %in% class_fxns_added]
        
        for (it in 1:iterations){
          print( it )  
          labeled_indices <- indices_list[[it]]$labeled_indices
          unlabeled_indices <- indices_list[[it]]$unlabeled_indices
          
          selected_indices = sort(c(labeled_indices, unlabeled_indices))
          iter_labeledIndicator <- rep(0,times = length(corpus_categoryVec))
          iter_labeledIndicator[labeled_indices] <- 1
          iter_labeledIndicator <- iter_labeledIndicator[selected_indices]
          iter_categoryVec = as.character( corpus_categoryVec[selected_indices]  )
          
          #################### Run the relevant code ####################### 
          dtm_pastein = paste0("sed -n '" , paste0( c(1,selected_indices+1),collapse = "p\n " ) ,
                               "p' ",dtm_loc, collapse = "" )
          dfm_pastein  = paste0("sed -n '" , paste0(  c(1,selected_indices + 1) , 
                                                      collapse = "p\n " ) ,
                                "p' ",dfm_loc, collapse = "" )
          print("Starting Classifier + Quantifier Analysis")
          
          discreteResults <- try( eval(parse(text = eval_discrete ) ), TRUE)
          continuousResults <- try( eval(parse(text = eval_continuous ) ), TRUE)
          outer_start_time=proc.time()[3]
          if(class(discreteResults) == "try-error"){print(discreteResults)}
          if(class(continuousResults) == "try-error"){print(continuousResults)}
          
          #################### Save estimates ##############################
          new_error <-  try(data.frame(dataset=paste(csv_name, " - Analysis",sep=""), 
                                       logbits_inmemory = log(sum(sort( sapply(ls(),function(x){object.size(get(x))})))),
                                       logbits_inmemory2 = NA,
                                       elapsed_time = NA,
                                       initial_seed = c(my_seed), 
                                       readme2_error= NA,  
                                       readme_error = discreteResults$error_readme , 
                                       
                                       continuousRegression_error = continuousResults$ErrorResultsEnsemble_continuous$Regression_est,
                                       discreteRegression_error = discreteResults$ErrorResultsEnsemble_discrete$Regression_est,
                                       
                                       #continuous comparisons 
                                       continuousEnsemble_error = continuousResults$ErrorResultsEnsemble_continuous$ensemble,
                                       continuousBayes_error = continuousResults$ErrorResultsEnsemble_continuous$NaiveBayes_est,
                                       continuousSVM_error = continuousResults$ErrorResultsEnsemble_continuous$SVM_est,
                                       continuousForest_error = continuousResults$ErrorResultsEnsemble_continuous$Forest_est,
                                       
                                       #continuous pure count comparisons 
                                       continuousRegression_error_count = continuousResults$ErrorResultsEnsemble_continuous_count$Regression_est_count,
                                       continuousEnsemble_error_count = continuousResults$ErrorResultsEnsemble_continuous_count$ensemble_count,
                                       continuousBayes_error_count = continuousResults$ErrorResultsEnsemble_continuous_count$NaiveBayes_est_count,
                                       continuousSVM_error_count = continuousResults$ErrorResultsEnsemble_continuous_count$SVM_est_count,
                                       continuousForest_error_count = continuousResults$ErrorResultsEnsemble_continuous_count$Forest_est_count,
                                       
                                       #discrete comparisons 
                                       discreteEnsemble_error = discreteResults$ErrorResultsEnsemble_discrete$ensemble,
                                       discreteBayes_error = discreteResults$ErrorResultsEnsemble_discrete$NaiveBayes_est,
                                       discreteSVM_error = discreteResults$ErrorResultsEnsemble_discrete$SVM_est,
                                       discreteForest_error = discreteResults$ErrorResultsEnsemble_discrete$Forest_est,
                                       
                                       #discrete pure count comparisons 
                                       discreteRegression_error_count = discreteResults$ErrorResultsEnsemble_discrete_count$Regression_est_count,
                                       discreteEnsemble_error_count = discreteResults$ErrorResultsEnsemble_discrete_count$ensemble_count,
                                       discreteBayes_error_count = discreteResults$ErrorResultsEnsemble_discrete_count$NaiveBayes_est_count,
                                       discreteSVM_error_count = discreteResults$ErrorResultsEnsemble_discrete_count$SVM_est_count,
                                       discreteForest_error_count = discreteResults$ErrorResultsEnsemble_discrete_count$Forest_est_count,
                                       
                                       #quantification error metrics 
                                       va2_error  = discreteResults$va2_error,
                                       quantify_friedman_error = discreteResults$error_quantify[[1]]["friedman"], 
                                       quantify_prob_error = discreteResults$error_quantify[[1]]["prob"], 
                                       quantify_mixtureL2_error = discreteResults$error_quantify[[1]]["mixtureL2"], 
                                       quantify_mixtureL1_error = discreteResults$error_quantify[[1]]["mixtureL1_QUANT"], 
                                       quantify_mixtureHPMF_error = discreteResults$error_quantify[[1]]["mixtureHPMF"], 
                                       quantify_adjCount_error = discreteResults$error_quantify[[1]]["adjCount"], 
                                       quantify_medianSweep_error = discreteResults$error_quantify[[1]]["medianSweep"], 
                                       quantify_hdx_error = discreteResults$error_quantify[[1]]["hdx"], 
                                       quantify_va1_error = discreteResults$error_quantify[[1]]["va1"], 
                                       quantify_naive_error = discreteResults$error_quantify[[1]]["naive"], 
                                       
                                       PrDDiv_withMatching= NA, PrDDiv = NA, 
                                       ESGivenDDiv_withMatching = NA, ESGivenDDiv = NA, 
                                       nCat = length( unique(corpus_categoryVec[ labeled_indices ] ) ), 
                                       iteration = it, corpus_numb = counter_, nboot = nboot, 
                                       n_labeled = sum(iter_labeledIndicator==1),  n_unlabeled = sum(iter_labeledIndicator==0), 
                                       sampling_scheme = names(indices_list)[it]), TRUE)
          row.names(new_error) <- NULL ; errors <- rbind(errors,new_error);rm(new_error)
          write.csv(errors, sprintf("./%s/%s", filename_saved_results, out_file) ) 
          
        }
        rm(errors); 
      } 
      
      if(compute_readme2 == TRUE){ 
        for (it in 1:iterations){
          print(it)
          labeled_indices <- indices_list[[it]]$labeled_indices
          unlabeled_indices <- indices_list[[it]]$unlabeled_indices
          selected_indices = sort(c(labeled_indices, unlabeled_indices))
          iter_labeledIndicator <- rep(0,times = length(corpus_categoryVec))
          iter_labeledIndicator[labeled_indices] <- 1
          iter_labeledIndicator <- iter_labeledIndicator[selected_indices]
          iter_categoryVec = as.character( corpus_categoryVec[selected_indices]  )
          table( iter_categoryVec )
          
          dfm_pastein_labeled  = paste0("sed -n '" , paste0(  c(1,selected_indices[iter_labeledIndicator==1] + 1) , 
                                                              collapse = "p\n " ) ,"p' ",dfm_loc, collapse = "" )
          dfm_pastein_unlabeled  = paste0("sed -n '" , paste0(  c(1,selected_indices[iter_labeledIndicator==0] + 1) , 
                                                                collapse = "p\n " ) ,"p' ",dfm_loc, collapse = "" )
          outer_start_time <- proc.time()[3]
          #profvis::profvis(readme(dfm =  list(labeled_cmd   = dfm_pastein_labeled,unlabeled_cmd = dfm_pastein_unlabeled),labeledIndicator = iter_labeledIndicator,categoryVec = iter_categoryVec, nboot = 1))
          unlabeled_pd  = c(prop.table(table(as.factor(iter_categoryVec)[iter_labeledIndicator==0])))
          labeled_pd  = c(prop.table(table(as.factor(iter_categoryVec)[iter_labeledIndicator==1])))
          
          readme2Results <- try(readme(dfm = list(labeled_cmd   = dfm_pastein_labeled,
                                                  unlabeled_cmd = dfm_pastein_unlabeled),
                                       labeledIndicator = iter_labeledIndicator,
                                       categoryVec = iter_categoryVec, nBoot = nboot), TRUE)
          if(class(readme2Results) == "try-error"){print( readme2Results )}
          tf$keras$backend$clear_session()
          mem_at_iter = pryr::mem_used()
          print(mem_at_iter)
          readme2Results = list(error_readme2 = sum(abs(readme2Results$point_readme[names(unlabeled_pd)] - unlabeled_pd)))
          elapsed_time = proc.time()[3]-outer_start_time
          
          if(is.null(readme2Results$PrDDiv)){
            readme2Results$PrDDiv <- readme2Results$PrDDiv_withMatching <- NA 
            readme2Results$ESGivenDDiv_withMatching <- readme2Results$ESGivenDDiv <- NA
          }
          
          results = as.data.frame(data.table::fread(sprintf("./%s/%s", filename_saved_results, out_file) ))[,-1] 
          results[it,"PrDDiv"] <- sum(abs(labeled_pd[names(unlabeled_pd)] - unlabeled_pd))
          results[it,"logbits_inmemory2"] <- log(mem_at_iter)
          results[it,"elapsed_time"] <- elapsed_time
          results[it,"readme2_error"] <- readme2Results$error_readme2
          results[it,"PrDDiv_withMatching"] <- readme2Results$PrDDiv_withMatching
          results[it,'ESGivenDDiv_withMatching'] <- readme2Results$ESGivenDDiv_withMatching
          results[it,"ESGivenDDiv"] <- readme2Results$ESGivenDDiv
          write.csv(results,sprintf("./%s/%s", filename_saved_results, out_file) )
          rm(results,readme2Results,unlabeled_pd)
        }
        
      } 

}      
#Spitout results 
if(TRUE == TRUE){
  global_results <- c() 
  my_csvs <- list.files("./results/")
  for(my_csv in my_csvs){ df_i <- try(read.csv(sprintf("./results/%s", my_csv  ))[,-1], TRUE) ; global_results <- rbind(global_results, df_i)}
}
 