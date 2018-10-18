devtools::install_github("iqss-research/readme-software/readme")
library(readme)

folder_name <- sprintf("Results_%s_%s", sampling_scheme , Sys.Date() ) 
labeled_sz <- 300; unlabeled_sz <- 300; 
feat <- 15
nboot <- 10 
minFreq <- 0.01; maxFreq <- 0.99 

support_files <- list.files("./support/")
support_files <- support_files[! support_files %in% c("readme2_master_public.R")]
for(support_file in support_files){ source(sprintf("./support/%s", support_file)) }

library("data.table")
wordVecs_corpus <- fread(wordVecs_pointer)
wordVecs_rowNames <- wordVecs_corpus[[1]]
wordVecs_corpus <- as.matrix (  wordVecs_corpus[,-1] )  
row.names(wordVecs_corpus) <- wordVecs_rowNames
rm(wordVecs_rowNames)

eval_text <- 'try( readme2_testing(dtm=iter_dtm, labeledIndicator=iter_labeledIndicator, 
categoryVec=iter_categoryVec, dfm = iter_docSummaries, 
nboot = nboot, compareClassifiers = T, compareQuantifiers = T, verbose = !RCE_indicator), T)'

csv_keys <- grep(list.files("./data/"), pattern = "\\.csv", value = T)
csv_keys <- sample(csv_keys)
ijack_counter <- 0 
global_iter_seq <- 1:length(csv_keys)
for(ijack in global_iter_seq){
    ijack_counter <- ijack_counter +1 
    print( sprintf("Dataset %s - %s", c(ijack_counter),gsub(csv_keys[ijack], pattern = "\\.csv", replace = "")  ) )
    
    ### Initialize placeholder for storing the errors
    errors <- data.frame()
    indices_list <- list()

    ### Accounting 
    out_file <- gsub(csv_keys[ijack], pattern = "\\.csv", replace = "_results.csv")
    csv_name <- gsub(csv_keys[ijack], pattern = "\\.csv", replace = "")
    in_file <- cbind(read.csv(sprintf("./data/%s", csv_keys[ijack]), header = T), 1)
    colnames(in_file)[3] <- "LABELEDSET"
    in_file[,2] <- enc2utf8(as.character(in_file[,2]))
    
    
    corpus_DTM <- toDTM(my_text)
    corpus_categoryVec = csv_input[,2]
    
    
    csv_undergrad <- undergrad(undergradVersion = "1", undergradVersion1_control = list(
                                    control=in_file,
                               sep=",", contentKey ="RAWTEXT",inputType = "raw", 
                               categoryKey="CATEGORY", 
                               labeledSet_key ="LABELEDSET",
                               minFreq=minFreq,  maxFreq=maxFreq) ) 
    my_text <- as.character( in_file[,2] )  
      
    DocSummaries_input <- undergrad(my_text,  wordVecs_corpus = wordVecs_corpus)
    rm(my_text_orig); rm(my_text);
        
    ## Make "CATEGORY" coding a factor variable
    csv_undergrad[,2] <- as.character(csv_undergrad[,2])
    csv_undergrad[,2] <- paste(csv_undergrad[,2], as.numeric(as.factor(csv_undergrad[,2])) ) 
    csv_undergrad[,2] <- gsub(as.character(csv_undergrad[,2]), 
                                    pattern = "[[:punct:]]", 
                                      replace = " ")
    csv_undergrad[,2] <- gsub(gsub(csv_undergrad[,2], 
                                           pattern = "  ", 
                                           replace = " "), pattern = "  ", replace =" ")
    csv_undergrad[,2] <- gsub(csv_undergrad[,2], 
                                      pattern = "[[:space:]]", 
                                      replace = "_")
    csv_undergrad[,2] <- as.factor(csv_undergrad[,2])
    csv_undergrad <- csv_undergrad[,!duplicated(colnames(csv_undergrad))]
    colnames(csv_undergrad)[-c(1:3)] <- gsub(colnames(csv_undergrad)[-c(1:3)] , 
                                                     pattern = "[[:punct:]]", 
                                                     replace = "")
    
    csv_error <- rep(NA, length=iterations)
    sampling_scheme_used <- rep(NA, times = iterations)
    for(it in 1:iterations){ 
        cat(paste("Generating Dataset :", it, "\n"))
        sampling_scheme_old <- sampling_scheme
        if(sampling_scheme == "RandomCombination"){
          sampling_scheme <- sample(c( "Max_XDiv", "Min_XDiv","Uniform_XDiv" , 
                                      "Historical","Historical2", "Historical3", "HistoricalVarySampSize", 
                                      "Ahistorical_NoReuse",
                                      "breakdown_sampling","breakdown_sampling2", 
                                      "JustPermutations", "MinDiv", "MaxDiv",
                                      "Ahistorical_NoReuse2","Ahistorical_aykut","Ahistorical_aykut_quantification"
                                      ), 1)
          if(csv_name %in% "immigration"){  sampling_scheme <- sample(c("Historical"), 1)  }
        }

        INDICES_LIST = try(GetLabeledUnlabeledIndices( csv_undergrad_input = csv_undergrad,
                                                   sampling_scheme = sampling_scheme, 
                                                   labeled_sz = labeled_sz, unlabeled_sz = unlabeled_sz, vecs_input_ordered = DocSummaries_input ), T)  
        if(length(INDICES_LIST$labeled_indices)==0){browser()}
        if(class(INDICES_LIST) == "try-error"){ 
          print("SAMPLING ERROR, DEFAULTING TO HISTORICAL")
          sampling_scheme <- "Historical"
          INDICES_LIST = try(GetLabeledUnlabeledIndices( csv_undergrad_input = csv_undergrad,
                                                         sampling_scheme = sampling_scheme, 
                                                         labeled_sz = labeled_sz, unlabeled_sz = unlabeled_sz, vecs_input_ordered = DocSummaries_input ), T)  
        }
        labeled_indices <- INDICES_LIST$labeled_indices
        unlabeled_indices <- INDICES_LIST$unlabeled_indices
        LC <- csv_undergrad$CATEGORY[INDICES_LIST$labeled_indices]
        UC <- csv_undergrad$CATEGORY[INDICES_LIST$unlabeled_indices]
        LC_CATS_KEEP <- table( LC); LC_CATS_KEEP <- names(LC_CATS_KEEP)[LC_CATS_KEEP>10]
        unlabeled_indices <- unlabeled_indices[UC %in% LC_CATS_KEEP]
        labeled_indices <- labeled_indices[LC %in% LC_CATS_KEEP]
        indices_list[[it]] <- list(labeled_indices = labeled_indices, 
                                   unlabeled_indices = unlabeled_indices)
        sampling_scheme_used[it] <- sampling_scheme
        sampling_scheme <- sampling_scheme_old
      }
      
      for (it in 1:iterations){
        cat(paste("Estimation Iteration", it, "\n"))
        
        labeled_indices <- indices_list[[it]]$labeled_indices
        unlabeled_indices <- indices_list[[it]]$unlabeled_indices
        
        csv_data_it <- as.data.frame( csv_undergrad )  
        csv_data_it$LABELEDSET <- 0
        csv_data_it$LABELEDSET[labeled_indices] <- 1 
        csv_data_it <- csv_data_it[c(labeled_indices, unlabeled_indices),]
        csv_data_it <- cbind(csv_data_it[,1:3], csv_data_it[,-c(1:3)][,colMeans(csv_data_it[,-c(1:3)])>minFreq])
        csv_data_it$CATEGORY <- as.character(csv_data_it$CATEGORY)
        
        #################### Run the testing code ####################### 
        outer_start_time <-  proc.time(); predicted_prD <- try( eval(parse(text = eval_text ) ), T); print("outer_time:"); print( proc.time()- outer_start_time)
        
        #################### Save estimates ##############################
        if( class(predicted_prD) == "try-error"  ) {print(predicted_prD); browser()}
        if( class(predicted_prD) != "try-error"  ) 
        { 
          new_error <-  try(data.frame(dataset=csv_name,
                                   readme_error = predicted_prD$error_readme , 
                                   readme2_error = predicted_prD$error_readme2, 
                                   
                                   #continuous comparisons 
                                   continuousRegression_error = predicted_prD$ErrorResultsEnsemble_continuous$Regression_est,
                                   continuousEnsemble_error = predicted_prD$ErrorResultsEnsemble_continuous$ensemble,
                                   continuousBayes_error = predicted_prD$ErrorResultsEnsemble_continuous$NaiveBayes_est,
                                   continuousSVM_error = predicted_prD$ErrorResultsEnsemble_continuous$SVM_est,
                                   continuousForest_error = predicted_prD$ErrorResultsEnsemble_continuous$Forest_est,
                                   
                                   #continuous pure count comparisons 
                                   continuousRegression_error_count = predicted_prD$ErrorResultsEnsemble_continuous_count$Regression_est_count,
                                   continuousEnsemble_error_count = predicted_prD$ErrorResultsEnsemble_continuous_count$ensemble_count,
                                   continuousBayes_error_count = predicted_prD$ErrorResultsEnsemble_continuous_count$NaiveBayes_est_count,
                                   continuousSVM_error_count = predicted_prD$ErrorResultsEnsemble_continuous_count$SVM_est_count,
                                   continuousForest_error_count = predicted_prD$ErrorResultsEnsemble_continuous_count$Forest_est_count,
                                   
                                   #discrete comparisons 
                                   discreteRegression_error = predicted_prD$ErrorResultsEnsemble_discrete$Regression_est,
                                   discreteEnsemble_error = predicted_prD$ErrorResultsEnsemble_discrete$ensemble,
                                   discreteBayes_error = predicted_prD$ErrorResultsEnsemble_discrete$NaiveBayes_est,
                                   discreteSVM_error = predicted_prD$ErrorResultsEnsemble_discrete$SVM_est,
                                   discreteForest_error = predicted_prD$ErrorResultsEnsemble_discrete$Forest_est,
                                   
                                   #discrete pure count comparisons 
                                   discreteRegression_error_count = predicted_prD$ErrorResultsEnsemble_discrete_count$Regression_est_count,
                                   discreteEnsemble_error_count = predicted_prD$ErrorResultsEnsemble_discrete_count$ensemble_count,
                                   discreteBayes_error_count = predicted_prD$ErrorResultsEnsemble_discrete_count$NaiveBayes_est_count,
                                   discreteSVM_error_count = predicted_prD$ErrorResultsEnsemble_discrete_count$SVM_est_count,
                                   discreteForest_error_count = predicted_prD$ErrorResultsEnsemble_discrete_count$Forest_est_count,
                                   
                                   #quantification error metrics 
                                   va2_error  = predicted_prD$va2_error,
                                   quantify_friedman_error = predicted_prD$error_quantify[[1]]["friedman"], 
                                   quantify_prob_error = predicted_prD$error_quantify[[1]]["prob"], 
                                   quantify_mixtureL2_error = predicted_prD$error_quantify[[1]]["mixtureL2"], 
                                   quantify_mixtureL1_error = predicted_prD$error_quantify[[1]]["mixtureL1_QUANT"], 
                                   quantify_mixtureHPMF_error = predicted_prD$error_quantify[[1]]["mixtureHPMF"], 
                                   quantify_adjCount_error = predicted_prD$error_quantify[[1]]["adjCount"], 
                                   quantify_medianSweep_error = predicted_prD$error_quantify[[1]]["medianSweep"], 
                                   quantify_hdx_error = predicted_prD$error_quantify[[1]]["hdx"], 
                                   quantify_va1_error = predicted_prD$error_quantify[[1]]["va1"], 
                                   quantify_naive_error = predicted_prD$error_quantify[[1]]["naive"], 
                                   
                                   #diagnostics 
                                   PrDDiv_withMatching = c(predicted_prD$PrDDiv_withMatching), 
                                   PrDDiv = c(predicted_prD$PrDDiv), 
                                   
                                   ESGivenDDiv_withMatching = c(predicted_prD$ESGivenDDiv_withMatching), 
                                   ESGivenDDiv = c(predicted_prD$ESGivenDDiv), 
                                   
                                   nCat = length( unique(csv_data_it$CATEGORY) ), 
                                   iteration = it, feat = feat, nboot = nboot, 
                                   n_labeled = sum(csv_data_it$LABELEDSET==1),  n_unlabeled = sum(csv_data_it$LABELEDSET==0), 
                                   sampling_scheme = sampling_scheme_used[it], 
                                   nWords = ncol(csv_data_it)-3), T)  
          if(class(new_error) != "try-error"){ 
            row.names(new_error) <- NULL ; errors <- rbind(errors,new_error)
          }
          
          ### Save results
          write.csv(errors, sprintf("./results/%s", out_file) ) 
          print( errors )  
        } 
      }
}

#Spitout results 
if(T == T){
  global_results <- c() 
  my_csvs <- list.files("./results/")
  for(my_csv in my_csvs){ df_i <- try(read.csv(sprintf("./results/%s", my_csv  ))[,-1], T) ; global_results <- rbind(global_results, df_i)}
}
 