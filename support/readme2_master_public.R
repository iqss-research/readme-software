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

wordVecs_corpus <- data.table::fread(wordVecs_pointer)
wordVecs_rowNames <- wordVecs_corpus[[1]]
wordVecs_corpus <- as.matrix (  wordVecs_corpus[,-1] )  
row.names(wordVecs_corpus) <- wordVecs_rowNames
rm(wordVecs_rowNames)

eval_text <- 'try( readme2_testing(dtm=iter_dtm, labeledIndicator=iter_labeledIndicator, 
categoryVec=iter_categoryVec, dfm = iter_docSummaries, 
nboot = nboot, compareClassifiers = T, compareQuantifiers = T, verbose = F), T)'

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
    
    corpus_DTM <- toDTM(in_file[,2])
    corpus_categoryVec = in_file[,1]
    docSummaries <- undergrad(cleanText(in_file[,2]),  wordVecs = wordVecs_corpus)
    rm(in_file);
        
    ## Make "CATEGORY" coding a factor variable
    corpus_categoryVec <- as.character(corpus_categoryVec)
    corpus_categoryVec <- paste(corpus_categoryVec, as.numeric(as.factor(corpus_categoryVec)) ) 
    corpus_categoryVec <- gsub(as.character(corpus_categoryVec), 
                                    pattern = "[[:punct:]]", 
                                      replace = " ")
    corpus_categoryVec <- gsub(gsub(corpus_categoryVec, 
                                           pattern = "  ", 
                                           replace = " "), pattern = "  ", replace =" ")
    corpus_categoryVec <- gsub(corpus_categoryVec, 
                                      pattern = "[[:space:]]", 
                                      replace = "_")
    corpus_categoryVec <- as.factor(corpus_categoryVec)
    
    csv_error <- rep(NA, length=iterations)
    sampling_scheme_used <- rep(NA, times = iterations)
    for(it in 1:iterations){ 
      cat(paste("Generating Dataset :", it, "\n"))
      sampling_scheme_old <- sampling_scheme
      if(sampling_scheme == "RandomCombination"){
        if(it == 1){ 
          sampling_scheme_vec <- c( "Max_XDiv", "Min_XDiv","Uniform_XDiv" , 
                                    "Historical","Historical2", "Historical3", "HistoricalVarySampSize", 
                                    "Ahistorical_NoReuse","firat2018",
                                    "breakdown_sampling","breakdown_sampling2", 
                                    "JustPermutations", "MinDiv", "MaxDiv",
                                    "Ahistorical_NoReuse2","Ahistorical_aykut","Ahistorical_aykut_quantification")
          sampling_scheme_vec <- sample(sampling_scheme_vec)
          sampling_scheme_vec <- rep(sampling_scheme_vec, times = ceiling( iterations/length(sampling_scheme_vec)) ) 
        }
        sampling_scheme <- sampling_scheme_vec[it]
      }
      if(sampling_scheme == "RandomCombination2"){
        if(it == 1){ 
          sampling_scheme_vec <- sample(c("Historical","firat2018"))
          sampling_scheme_vec <- rep(sampling_scheme_vec, times = ceiling( iterations/length(sampling_scheme_vec)) ) 
        }
        sampling_scheme <- sampling_scheme_vec[it]
      }
      
      docSummaries_fs = FastScale(docSummaries)
      docSummaries_fs = docSummaries_fs[,!is.na(colSums(docSummaries_fs))]
      INDICES_LIST = try(GetLabeledUnlabeledIndices( csv_category = corpus_categoryVec,
                                                     sampling_scheme = sampling_scheme, 
                                                     labeled_sz = labeled_sz, unlabeled_sz = unlabeled_sz, docSummaries_input = docSummaries_fs ), T)  
      if(class(INDICES_LIST) == "try-error"){ 
        print("SAMPLING ERROR, DEFAULTING TO HISTORICAL")
        INDICES_LIST = try(GetLabeledUnlabeledIndices( csv_category = corpus_categoryVec,
                                                       sampling_scheme = "Historical", 
                                                       labeled_sz = labeled_sz, unlabeled_sz = unlabeled_sz, 
                                                       docSummaries_input = docSummaries_fs  ), T)  
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
    
    for (it in 1:iterations){
      cat(paste("Estimation Iteration", it, "\n"))
      
      labeled_indices <- indices_list[[it]]$labeled_indices
      unlabeled_indices <- indices_list[[it]]$unlabeled_indices
      
      iter_dtm <- corpus_DTM[c(labeled_indices, unlabeled_indices),]
      iter_dtm <-iter_dtm[,colSums(iter_dtm[1:length(labeled_indices),])>2]
      iter_categoryVec <- as.character( corpus_categoryVec[ c(labeled_indices, unlabeled_indices) ]  ) 
      iter_labeledIndicator <- rep(0, times = length( c(labeled_indices, unlabeled_indices) )  )
      iter_labeledIndicator[c(1:length(labeled_indices))] <- 1
      iter_labeledIndicator[-c(1:length(labeled_indices))] <- 0 
      iter_dtm <- iter_dtm[,colMeans(iter_dtm)>0.005]
      iter_docSummaries <- docSummaries[c(labeled_indices,unlabeled_indices),]
      
      #################### Run the relevant code ####################### 
      #sort( sapply(ls(),function(x){object.size(get(x))})) 
      outer_start_time <-  proc.time(); predicted_prD <- try( eval(parse(text = eval_text ) ), T); print("outer_time:"); print( proc.time()- outer_start_time)
      
      #################### Save estimates ##############################
      if( class(predicted_prD) == "try-error"  ) {print(predicted_prD); browser()}
      if( class(predicted_prD) != "try-error"  ) 
      { 
        new_error <-  try(data.frame(dataset=paste(csv_name, " - Analysis",sep=""), 
                                     readme_error = predicted_prD$error_readme , 
                                     readme2_error = predicted_prD$error_readme2, 
                                     
                                     continuousRegression_error = predicted_prD$ErrorResultsEnsemble_continuous$Regression_est,
                                     discreteRegression_error = predicted_prD$ErrorResultsEnsemble_discrete$Regression_est,
                                     
                                     #continuous comparisons 
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
                                     
                                     nCat = length( unique(iter_categoryVec) ), 
                                     iteration = it, nboot = nboot, 
                                     n_labeled = sum(iter_labeledIndicator==1),  n_unlabeled = sum(iter_labeledIndicator==0), 
                                     sampling_scheme = sampling_scheme_used[it], 
                                     nWords = ncol(iter_dtm)), T)
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
 