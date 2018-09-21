if(T == T){ 
rm(list=ls())
RCE_indicator <- F; pre_computed <- F
options(repos=structure(c(CRAN='http://lib.stat.cmu.edu/R/CRAN/')),  INSTALL_opts = c('--no-lock' ))

if(RCE_indicator==T){
  .libPaths(unique(c(.libPaths(), 
              "/usr/lib64/R/library", "/usr/share/R/library", 
              "/nfs/home/C/cjerzak/.R/library-x86_64/")))
  mainDir <- "~/shared_space/ps_ctj"
  setwd( mainDir )
  args <- commandArgs(TRUE)
  ijack <- as.integer(args[1]) + 1
  
  #iterations <- 5; nboot <- 5; global_iter_seq <- ijack
  iterations <- 17*25; nboot <- 5; global_iter_seq <- ijack
}

if(RCE_indicator == F){
  mainDir <- "~/Dropbox/AWS Infrastructure" 
  setwd( mainDir )
  source('./Readme2_SupportCode/readme2_buildPackage.R')
  install.packages("./readme.tar.gz", lib = "./", repos = NULL, type ="source",INSTALL_opts = c('--no-lock')) 
  
  iterations <- 1; nboot <- 1
  global_iter_seq <- 1:73; global_iter_seq[1:69] <- sample(1:69, 69)
  #manyCats <- c(44,04,67,43,39,56,05,45); global_iter_seq <- sample(global_iter_seq[global_iter_seq %in% manyCats])
} 


# Parse arguments
sampling_scheme <- "RandomCombination"
#sampling_scheme <- "Historical"
#sampling_scheme <- "firat2018"
folder_name <- sprintf("Results_%s_%s", sampling_scheme , Sys.Date() ) 
labeled_sz <- 300; unlabeled_sz <- 300
tm_control_list <- list(removeNumbers = TRUE,stopwords = TRUE,removePunctuation = TRUE, bounds = list(global = c(5, Inf)), stemming = TRUE)
 
library(readme, lib.loc = "./")

support_files <- c("readme2_sampling_helper_fxns.R","readme2_sampling.R","readme2_testing.R","readme2_testing_helper_fxns.R")
for(support_file in support_files){ print(support_file);source(sprintf("./Readme2_SupportCode/%s", support_file)) }

if( !dir.exists(sprintf("%s/%s", mainDir, folder_name))){ dir.create(sprintf("%s/%s", mainDir, folder_name))}

eval_text <- 'try( readme2_testing(dtm=iter_dtm, labeledIndicator=iter_labeledIndicator, 
categoryVec=iter_categoryVec, dfm = iter_docSummaries, 
nboot = nboot, compareClassifiers = T, compareQuantifiers = F, verbose = !RCE_indicator), T)'

### List all of the Crimson Hexagon CSVs
csv_names <- list.files("./validationdata/HandcodedCSVs")

if(pre_computed == F){ 
  library2("data.table", loadin = F)
  #wordVecs_corpus <- data.table::fread(sprintf("./%s/glove.twitter.27B.50d.txt", "glove.twitter.27B"))
  wordVecs_corpus <- data.table::fread(sprintf("./%s/glove.twitter.27B.200d.txt", "glove.twitter.27B"))
} 

#global_iter_seq = 6
ijack_counter <- 0 
for(ijack in global_iter_seq){
  ijack_counter <- ijack_counter +1 
  print( c(ijack_counter)  ) 
  ### Initialize placeholder for storing the errors
  errors <- data.frame()
  indices_list <- list()
    
  if(ijack <=  length(csv_names)){csv_name <- csv_names[ijack];out_file <- sprintf("ch_%s.csv", ijack)}
  if(ijack ==  length(csv_names)+1){csv_name <- "enron"; out_file <- "enron_results.csv"}
  if(ijack ==  length(csv_names)+2){csv_name <- "clinton"; out_file <- "clinton_results.csv"}
  if(ijack ==  length(csv_names)+3){csv_name <- "immigration";out_file <- "immigration_results.csv"}
  if(ijack ==  length(csv_names)+4){csv_name <- "stanford";out_file <- "stanford_results.csv"}
      
  if(pre_computed == T){ 
      corpus_DTM <- try(read.csv(file = paste("./Precomputed Material/DTM_", csv_name, sep = ""))[,-1], T)
      corpus_categoryVec <- corpus_DTM[,1]; corpus_DTM <- corpus_DTM[,-1]
      docSummaries <- try(read.csv(file = paste("./Precomputed Material/DFM_", csv_name, sep = "")), T)
      if(class(docSummaries) == "try-error" | class(corpus_DTM) == "try-error"){
        pre_computed <- F
        library2("data.table",loadin = F)
        wordVecs_corpus <- data.table::fread(sprintf("./%s/glove.twitter.27B.50d.txt", "glove.twitter.27B"))
      }
    }

  if(pre_computed == F){
      if(ijack == length(csv_names) + 1){ 
        csv_name <- "enron"; out_file <- "enron_results.csv" 
        csv_input <- read.table("./validationdata/enrontexts/controlenron.txt", sep = ",", header = T)
        time_stamp <- read.csv("./validationdata/enron_date_time_data.csv", stringsAsFactors=F)
        enron_files <- list.files("./validationdata/enrontexts/")
        csv_input <- cbind(csv_input, sapply(as.character(csv_input[,1]), function(x){paste(readLines(x), collapse = " ")}))
        
        ### Load time-stamps dataset
        csv_input[,1] <- gsub(x = csv_input[,1], pattern = "validationdata/enrontexts/",  replace = "")
        time_stamp$path <- gsub(time_stamp$path,  pattern = "validationdata/enrontexts/",  replace = "")
        dates_mat = as.data.frame( matrix(NA, nrow = nrow(time_stamp), ncol =0 ))  
        dates_mat$year_XXX <- time_stamp$year_XXX[base::match(time_stamp$path,csv_input[,1])]
        dates_mat$month_XXX <- time_stamp$month_XXX[base::match(time_stamp$path,csv_input[,1])]
        dates_mat$day_XXX <- time_stamp$day_XXX[base::match(time_stamp$path,csv_input[,1])]
        enron_ordering <- order(dates_mat$year_XXX, dates_mat$month_XXX, dates_mat$day_XXX) 
        
        csv_input = csv_input[enron_ordering,]
        my_text <- as.character( csv_input[,4] )  
        corpus_DTM <- as.data.frame(as.matrix( tm::weightBin( tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(tolower(my_text))), control = tm_control_list) )))
        corpus_categoryVec = csv_input[,2]
        
        rm(enron_ordering);rm(dates_mat)
        #myMat <- cbind(csv_undergrad$CATEGORY,my_text); colnames(myMat) <- c("CATEGORY", "RAWTEXT")
        #write.csv(file = "./enron.csv", myMat, row.names = F)
      } 
      
      if(ijack == length(csv_names) + 2){
        csv_input <- read.table("./validationdata/clintonposts/control.txt", sep = ",", header = T)
        csv_input <- cbind(csv_input,sapply(as.character(csv_input[,1]), function(x){paste(readLines(x), collapse = " ")}))
        csv_input = csv_input[csv_input[,2]!=0,]
        csv_input[,4] = iconv(csv_input[,4], from = "UTF-8", to = "ASCII", sub = '', mark = T)
        csv_input <- csv_input[sapply(csv_input[,4], nchar)>200,]#remove outliers 
        csv_input <- csv_input[sapply(csv_input[,4], nchar)<10000,]#remove outliers 
        corpus_DTM <- as.data.frame(as.matrix( tm::weightBin(tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(tolower(csv_input[,4]))), control = tm_control_list) )))
        corpus_categoryVec <- csv_input[,2]
        my_text <- csv_input[,4]

        rm(csv_input)
        #myMat <- cbind(csv_undergrad$CATEGORY,my_text); colnames(myMat) <- c("CATEGORY", "RAWTEXT")
        #write.csv(file = "./clinton.csv", myMat, row.names = F)
      } 
      
      if(ijack == length(csv_names) + 3){ 
        csv_input <- read.table("./validationdata/immigtexts/controlimmig.txt", sep = ",", header = T)
        csv_input <- cbind(csv_input,tolower(  sapply(as.character(csv_input[,1]), function(x){enc2utf8(paste(readLines(x), collapse = " ") )}) ))
        
        ### sort immigration_undergrad by 
        date_vec <- read.table( "validationdata/immigtexts/controlimmig.txt", sep = ",", header = T)[,1]
        date_vec <- gsub("^.*([[:digit:]]{8}).*$","\\1",as.character(date_vec))
        ## Fix non-matched element
        date_vec[grep("immigtexts",date_vec)] <- "25062007"
        
        ## Parse date_vec as a date object
        immigration_dates <- strptime(date_vec, "%d%m%Y")
        
        ## Reorder thing
        csv_input = csv_input[order(immigration_dates),]
        my_text <- as.character(csv_input[,ncol(csv_input)])
        corpus_DTM <- as.data.frame(as.matrix( tm::weightBin(tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(my_text )),
                                                                                    control = tm_control_list) )))
        corpus_categoryVec <- csv_input$truth
        
        #myMat <- cbind(csv_undergrad$CATEGORY,my_text); colnames(myMat) <- c("CATEGORY", "RAWTEXT")
        #write.csv(file = "./immigration.csv", myMat, row.names = F)
      }
      
      if(ijack == length(csv_names) + 4){
        Dictionary <- do.call(rbind, strsplit(readLines("./Quantification Replication/stanfordSentimentTreebank/dictionary.txt", 
                                                        encoding = "UTF-8"), split = "\\|") )
        Dictionary[,2] <- iconv(Dictionary[,2], from="UTF-8", to="LATIN1")
        DataSentences <- do.call(rbind, strsplit(readLines("./Quantification Replication/stanfordSentimentTreebank/datasetSentences.txt", 
                                                           encoding = "macintosh"), split = "\\t") )[-1,]
        DataSentences[,2] <- iconv(DataSentences[,2], from="UTF-8", to="LATIN1")
        DataSentences[,2] <- gsub( DataSentences[,2], pattern = "-LRB-", replace = "(") 
        DataSentences[,2] <- gsub( DataSentences[,2], pattern = "-RRB-", replace = ")")
        DataSplits <- do.call(rbind, strsplit(readLines("./Quantification Replication/stanfordSentimentTreebank/datasetSplit.txt", encoding = "UTF-8"), 
                                              split = ",") )[-1,]
        SentimentLabels <- do.call(rbind, strsplit(readLines("./Quantification Replication/stanfordSentimentTreebank/sentiment_labels.txt", encoding = "UTF-8"), 
                                                   split = "\\|"))[-1,]
        DictTextVec <- Dictionary[,1]
        DictIDVec <- f2n(Dictionary[,2])
        PhraseLabelsIDVec <- f2n(SentimentLabels[,1])
        PhraseLabelsValueVec <- f2n(  SentimentLabels[,2] )  
        SentenceTextVec <- DataSentences[,2]
        MasterSentencesDf <- cbind(DataSentences, matrix(logical(), ncol = 4, nrow = length(SentenceTextVec)))
        SentencePhraseIDVec <- SentenceSentimentCatVec <- SentenceSentimentValueVec <- rep(NA, times = length(SentenceTextVec))
        colnames(MasterSentencesDf) <- c("SentenceID", "Sentence", "SentenceIDAsPhrase", 
                                         "SentimentValue", "SentimentLabel", "FromSet")
        bad_indices <- c()
        for(badger in 1:length(SentenceTextVec)){
          if(badger %% 10 == 0){print(badger/length(SentenceTextVec))}
          SentPhraseID <- DictIDVec[which(DictTextVec == SentenceTextVec[badger])]
          if(length(SentPhraseID) == 0){ bad_indices <- c(badger, bad_indices) } 
          if(length(SentPhraseID) > 0){ 
            SentencePhraseIDVec[badger] <- SentPhraseID
            SentenceSentimentValueVec[badger] <- PhraseLabelsValueVec[which(PhraseLabelsIDVec == SentPhraseID)]
          } 
        }
        
        #[0, 0.2], (0.2, 0.4], (0.4, 0.6], (0.6, 0.8], (0.8, 1.0]
        SentenceSentimentCatVec[SentenceSentimentValueVec <=0.20] <- 1
        SentenceSentimentCatVec[SentenceSentimentValueVec > 0.20 & SentenceSentimentValueVec <= 0.40] <- 2
        SentenceSentimentCatVec[SentenceSentimentValueVec > 0.40 & SentenceSentimentValueVec <= 0.60] <- 3
        SentenceSentimentCatVec[SentenceSentimentValueVec > 0.60 & SentenceSentimentValueVec <= 0.80] <- 4
        SentenceSentimentCatVec[SentenceSentimentValueVec > 0.80] <- 5
        
        MasterSentencesDf[,"SentenceIDAsPhrase"] <- SentencePhraseIDVec
        MasterSentencesDf[,"SentimentValue"] <- SentenceSentimentValueVec
        MasterSentencesDf[,"SentimentLabel"] <- SentenceSentimentCatVec
        MasterSentencesDf[,"FromSet"] <- DataSplits[,2]
        
        MasterSentencesDf <- MasterSentencesDf[!is.na(MasterSentencesDf[,"SentimentLabel"]),]
        my_text <-   MasterSentencesDf[,"Sentence"]
        corpus_DTM <- as.matrix( tm::weightBin( tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(tolower(my_text) )), 
                                                        control = tm_control_list) ) )   
        colnames(corpus_DTM)[-c(1)] <- paste("V", 1:(ncol(corpus_DTM)-1), sep = "")
        
        corpus_categoryVec = SentenceSentimentCatVec
        corpus_DTM[,c(1:ncol(corpus_DTM))] <- apply(corpus_DTM[,c(1:ncol(corpus_DTM))], 2, f2n)
        
        #myMat <- cbind(MasterSentencesDf[,"SentimentLabel"],MasterSentencesDf[,"Sentence"]); colnames(myMat) <- c("CATEGORY", "RAWTEXT")
        #write.csv(file = "./stanford.csv", myMat, row.names = F)
      } 
      
      if(ijack <= length(csv_names)){ 
        ### For each CH CSV, Load CSV file
        csv_name <- csv_names[ijack]
        cat(paste(csv_name, "\n"))
        csv_csvfile <- try(read.csv(paste("./validationdata/HandcodedCSVs/",csv_name, sep=""), sep=",", stringsAsFactors=F, encoding="latin1"), T)
        
        ### Sort CSV file by Timestamp
        csv_csvfile$Timestamp <- try( strptime(csv_csvfile$Timestamp, format="%m/%e/%y %H:%M"), T)
        csv_csvfile <- try( csv_csvfile[order(csv_csvfile$Timestamp),], T)
        
        my_text <- csv_csvfile$Contents
        corpus_DTM <- as.matrix( tm::weightBin( tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(tolower(my_text))), 
                                                        control = tm_control_list) ) ) 
        corpus_categoryVec <- csv_csvfile$Category
        rm(csv_csvfile)
      }
      
      { 
        print( prop.table(table( rowSums( corpus_DTM )== 0)   )  )
        docSummaries <- undergrad(cleanText_fxn(my_text),  wordVecs_corpus = wordVecs_corpus)
        rm(my_text);

        ## Make "CATEGORY" coding a factor variable
        corpus_categoryVec <- gsub(as.character(corpus_categoryVec), pattern = "[[:punct:]]", replace = "_")
        corpus_categoryVec <- gsub(corpus_categoryVec, pattern = "[[:space:]]", replace = "_")
        corpus_categoryVec <- as.factor(corpus_categoryVec)
        
        corpus_DTM <- cbind(corpus_categoryVec, as.data.frame( corpus_DTM) ) 
        colnames(corpus_DTM)[1] <- "CATEGORY"
        
        #save for precomputations 
        write.csv(corpus_DTM, file = paste("./Precomputed Material/DTM_", csv_name, sep = ""))
        write.csv(docSummaries, file = paste("./Precomputed Material/DFM_", csv_name, sep = ""))
        corpus_categoryVec <- corpus_DTM[,1]; corpus_DTM <- corpus_DTM[,-1]
      }
    } 
    
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
          if(RCE_indicator == F){browser()}
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
          write.csv(errors, sprintf("./%s/%s", folder_name, out_file) ) 
          print( errors )  
      
          if(RCE_indicator == F){ 
            CHECKIN_my_data <- c() 
            CHECKIN_my_csvs <- list.files(sprintf("./%s/", folder_name))
            for(CHECKIN_my_csv in CHECKIN_my_csvs){ df_i <- try(read.csv(sprintf("./%s/%s", folder_name, CHECKIN_my_csv  ))[,-1], T) ; CHECKIN_my_data <- rbind(CHECKIN_my_data, df_i)}
            
            try(plot( apply(CHECKIN_my_data[,2:36],1,function(x){min(f2n(x),na.rm=T)}),CHECKIN_my_data$readme2_error, cex = 10*CHECKIN_my_data$PrDDiv), T); abline(a = 0, b = 1 )
            print(sprintf("KEY METRIC: %s", round(coef(lm(CHECKIN_my_data$readme2_error~0+apply(CHECKIN_my_data[,2:36],1,function(x){min(f2n(x),na.rm=T)})))[1],2) ) ) 
          } 
        } 
    }
  }
}


if(T == F){
  #folder_name <- "Results_Historical_2018-09-09"
  folder_name <- "Results_Historical_2018-09-13_k"
  folder_name <- "Results_RandomCombination_2018-09-17_k"
  options("scipen"=4, "digits"=4)
  par(mfrow = c(1,1))
  
  my_data <- c()  
  my_csvs <- list.files(sprintf("./%s/", folder_name))
  for(my_csv in my_csvs){ df_i <- try(read.csv(sprintf("./%s/%s", folder_name, my_csv  ))[,-1], T) ; my_data <- rbind(my_data, df_i)}
  try(plot( apply(my_data[,2:36],1,function(x){min(f2n(x),na.rm=T)}),my_data$readme2_error, cex = 10*my_data$PrDDiv), T); abline(a = 0, b = 1 )
  
  #my_data <- my_data[my_data$sampling_scheme == "Historical",]
  #my_data <- my_data[my_data$sampling_scheme == "firat2018",]
  #my_data <- my_data[grepl(my_data$dataset,pattern = "clinton"),]
  plot(my_data$continuousRegression_error,my_data$readme2_error, cex = 0.1*(my_data$nCat)); abline(a = 0, b = 1 )
  PartSummary_examine <- 4
  plot_all <- sort(  t(  apply(my_data, 2, function(x) summary(as.numeric(as.character((x))))[PartSummary_examine]) )[,-c(1,(ncol(my_data)-9):ncol(my_data))] ) 
  col_all <- rep("black", times = length(plot_all));col_all[names(plot_all) == "readme2_error" ] <- "red"
  col_all[grepl(names(plot_all),pattern= "continuous")] <- "green"
  plot(plot_all, cex = 0, ylim = c(min(plot_all),summary(plot_all)[5]));text(plot_all, labels = names(plot_all), cex = 0.5, col = col_all)
  head( plot_all,max(5,grep(names(plot_all),pattern = "readme2")))
  
  
  head(sort(sort(  t(  apply(my_data[my_data$sampling_scheme=="JustPermutations",], 2,function(x) 
                    summary(as.numeric(as.character((x))))[PartSummary_examine]) )[,-c(1,(ncol(my_data)-9):ncol(my_data))] ) ))
  sort( tapply(my_data$PrDDiv, my_data$sampling_scheme, function(x){mean(x, na.rm = T)}) ) 
  sort( tapply(my_data$ESGivenDDiv, my_data$sampling_scheme, function(x){mean(x, na.rm = T)}) ) 
  
  #immigration - Analysis, clinton - Analysis
  t(  apply(my_data, 2, function( x) summary(as.numeric(as.character((x))))[1:6]) )
  mean(my_data$continuousRegression_error<=my_data$readme2_error); summary((my_data$readme2-my_data$continuousRegression_error)/my_data$continuousRegression_error)
  mean(my_data$readme_error<=my_data$readme2_error); summary((my_data$readme2-my_data$readme_error)/my_data$readme_error)
  
  head( sort(  tapply(1:nrow(my_data),my_data$dataset,function(x){mean(my_data[x,]$continuousRegression_error-my_data[x,]$readme2_error)}) )   ) 
  head( sort(  tapply(1:nrow(my_data),my_data$dataset,function(x){mean(my_data[x,]$discreteRegression_error-my_data[x,]$readme2_error)}) )   ) 

  plot(  tapply(1:nrow(my_data),my_data$nCat,function(x){mean(my_data[x,]$readme_error>=my_data[x,]$readme2_error)}) );abline(h=0.5)
  plot(  tapply(1:nrow(my_data),my_data$nCat,function(x){mean(my_data[x,]$continuousRegression_error>=my_data[x,]$readme2_error)}) );abline(h=0.5)
  plot(  tapply(1:nrow(my_data),my_data$nCat,function(x){mean(my_data[x,]$quantify_prob_error>=my_data[x,]$readme2_error)}) );abline(h=0.5)

  plot(my_data$readme_error,my_data$readme2_error); abline(a = 0, b = 1 )
  plot(my_data$continuousRegression_error,my_data$readme2_error); abline(a = 0, b = 1 )
  
  #give full names to sampling schemes
  if(T == T){ 
    my_data$sampling_scheme_fullName = as.character( my_data$sampling_scheme ) 
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "Historical"] <- "*Empirical*"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "HistoricalVarySampSize"] <- "Empirical, Varied Train. Size"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "Historical2"] <- "Sequential"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "Historical3"] <- "Empirical, Max. Train. Size"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "JustPermutations"] <- "Random Subsets"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "MinDiv"] <- "Min. Prop. Div."
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "Ahistorical_NoReuse"] <- "Unif. Random Prop. Div."
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "MaxDiv"] <-  "Max Prop. Div."
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "Min_XDiv"] <- "Min. S. Change"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "Uniform_XDiv"] <- "Random S. Change"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "Max_XDiv"] <- "Max. S. Change"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "Ahistorical_aykut"] <- "Random Walk"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "Ahistorical_aykut_quantification"] <- "Chron. Train., Unif. Test"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "firat2018"] <- "Random Train., Unif. Test"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "breakdown_sampling"] <- "Random Samp. Size w. Extr. Shift"
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "Ahistorical_NoReuse2"] <- "Unif. Random Props."
    my_data$sampling_scheme_fullName[my_data$sampling_scheme == "breakdown_sampling2"] <- "Extr. Features in Labeled Set"  
  }
  
  #comparison table 
  if(T == T){ 
    t1 <- t(  apply(my_data, 2, function(x) summary(as.numeric(as.character((x))))[1:6]) )
    key_mat <- cbind(row.names(t1), row.names(t1))
    
    compare_names <- c("readme_error", "readme2_error",
                       grep(key_mat, pattern = "discrete", value = T), 
                       grep(key_mat, pattern = "continuous", value = T))
    expand_grid <- expand.grid(compare_names, compare_names)
    
    grid_rez <- grid_rez2 <- c() 
    for(ia in 1:nrow(expand_grid)){ 
      compare1 <- my_data[,as.character( expand_grid[ia,1] )  ]
      compare2 <- my_data[,as.character( expand_grid[ia,2] ) ]
      base_error_vec <- tapply(compare1, my_data$dataset, function(x) mean(x, na.rm = T) )
      alt_error_vec <- tapply(compare2, my_data$dataset, function(x) mean(x, na.rm = T)  )
      compare_error_vec <- tapply(1:length(compare2), my_data$dataset, function(x) { 
        mean(compare2[x] <= compare1[x], na.rm = T)
      } )
      grid_rez <- c(grid_rez, mean(alt_error_vec<= base_error_vec, na.rm = T ) )  
      grid_rez2 <- c(grid_rez2, mean(compare_error_vec) )  
    }
    
    grid_rez <- cbind(expand_grid, grid_rez, grid_rez2)
    explore_method <- "continuousRegression_error" 
    t3 <- grid_rez[grid_rez[,2] == explore_method,c(1,3:4)]
    colnames(t3) <- c("Comparison Method", " % Corpora Better", "% Subsamples Better")
    t3[,1] <- as.character( t3[,1] )  
    t3[,2:3] <- 100 * t3[,2:3]
    t3[,2:3] <- apply(t3[,2:3] , 2, function(x){ round(x, 2)})
    t3 <- t3[t3$`Comparison Method` != explore_method,]
    print( t3[order(t3$` % Corpora Better`, decreasing = T),] )
  } 
  
  pdf("~/Downloads/BIGSIM_readme.pdf");
  set.seed(122)
  if(T == T){ 
    yTag <- "readme"; ylab_use <- "Improvement in SAE, Readme2 vs. Readme"
    X_axis <- tapply(my_data$PrDDiv, my_data$sampling_scheme_fullName, mean);
    XLab <- "Average Proportion Divergence"
  
    if(yTag == "readme"){ Y_axis <- -1*tapply((my_data$readme2-my_data$readme_error), my_data$sampling_scheme_fullName, function(x){mean(x,na.rm = T)})  }
    #if(yTag == "readme"){ Y_axis <- -1*tapply((my_data$readme2-my_data$va2), my_data$sampling_scheme_fullName, function(x){mean(x,na.rm = T)})  }
    if(T == T){ 
      my_cols <-  rep("black", times = length(X_axis));my_cols[names(X_axis) == "*Empirical*"] <- "red"
      my_cols2 <-  rep("black", times = length(X_axis));my_cols2[names(X_axis) == "*Empirical*"] <- "red"
      my_cex <- rep(0.75, times = length(X_axis))
      my_cex[names(X_axis) == "*Empirical*"] <- 1
      par(mar=c(5, 5, 4, 2) )
      xaxt_use <- c(min(X_axis)-0.55*(max(X_axis)-min(X_axis)), 
                max(X_axis)+1*(max(X_axis)-min(X_axis)))
      plot(X_axis,Y_axis, 
           ylim = c(min(0-0.01,Y_axis),  max(Y_axis)), 
           xlim = xaxt_use, 
           cex.lab = 1.5, 
           xaxt = "n", 
           col = my_cols, 
           xlab = XLab, ylab = ylab_use, 
           cex = 0.85, pch = 3 ); 
      xaxt_seq <- seq(min(X_axis),  max(X_axis), length.out = 5)
      axis(side = 1,  at = xaxt_seq, labels = round(xaxt_seq, 2))
      abline(h = 0, lty = 3)
      text((xaxt_use[1]+xaxt_use[2])/2, 0.005, labels = "higher SAE than readme2", col = "black")
      text((xaxt_use[1]+xaxt_use[2])/2, -0.005, labels = "lower SAE than readme2", col = "black")
      yjig <- rep(0, length(X_axis))
      xjig <- 0.06 *  (log(nchar(names(X_axis)))) + 0.0055*nchar(names(X_axis))
      xjig[order(Y_axis) %% 2 == 1] <- -1*xjig[order(Y_axis) %% 2 == 1]
      xjig[Y_axis %in% Y_axis[order(Y_axis)[1:2]]] <- -1*abs(xjig[Y_axis %in% Y_axis[order(Y_axis)[1:2]]] )
      xjig[Y_axis %in% Y_axis[order(Y_axis)[(length(Y_axis)-3):length(Y_axis)]]] <- 1*abs(xjig[Y_axis %in% Y_axis[order(Y_axis)[(length(Y_axis)-3):length(Y_axis)]]])
      xjig[grepl(names(X_axt_jigged), pattern = "\\*Empirical")] <- 0 
      Y_axt_jigged <- Y_axis+yjig; 
      X_axt_jigged <- X_axis+xjig
      X_BIG <- cbind(X_axt_jigged, Y_axt_jigged)
      X_BIG_dist <- as.matrix(  dist(X_BIG) )  
      X_BIG_dist_min <- apply(X_BIG_dist, 1, function(x){min(x[x>0])})
      min_dist_jig <- 0.04; 
      #X_axt_jigged[X_BIG_dist_min < min_dist_jig] <-   (X_axt_jigged[X_BIG_dist_min < min_dist_jig] + 
                                                                       #runif(length(X_axt_jigged[X_BIG_dist_min < min_dist_jig]), -0.05, 0.05))
      #Y_axt_jigged[X_BIG_dist_min < min_dist_jig] <-   (Y_axt_jigged[X_BIG_dist_min < min_dist_jig] + runif(length(Y_axt_jigged[X_BIG_dist_min < min_dist_jig]), -0.01, 0.01))
      Y_axt_jigged[grepl(names(X_BIG_dist_min), pattern="\\*Empirical\\*")] <-   (Y_axt_jigged[grepl(names(X_BIG_dist_min), pattern="\\*Empirical\\*")]  + runif(length(Y_axt_jigged[grepl(names(X_BIG_dist_min), pattern="\\*Empirical\\*")]), -0.01, 0.01))
      text(X_axt_jigged,Y_axt_jigged, labels = names(X_axis), 
           col = my_cols2, cex = my_cex)
    } 
  }
  dev.off()
  
  pdf("~/Downloads/BIGSIM_all2.pdf");
  if(T == T){ 
    AggFxn <- function(.){mean(., na.rm = T)}
    set.seed(1)
    par(mar=c(16, 5, 4, 2) )
    Y_axis_mat <- c()
    y_names <- c(grep(colnames(my_data), pattern = "readme", value = T), 
                 grep(colnames(my_data), pattern = "va2",value = T),
                 grep(colnames(my_data), pattern = "quantify", value = T),
                 grep(colnames(my_data), pattern = "discrete", value = T), 
                 grep(colnames(my_data), pattern = "continuous", value = T))
    y_names <- y_names[!grepl(y_names, pattern = "readme2")]
    plot(0,0, 
         ylim = c(-0.20, 0.60),
         xlim = c(1,length(unique(my_data$sampling_scheme))), 
         cex.lab = 1.5, xaxt = "n", xlab = "",
         ylab = "Improvement in SAE", 
         cex = 0, pch = 3 ); 
    abline(h = 0, lty = 3)
    counter_iter <- 0 
    overall_value <- rep(0, times = length(y_names))
    names(overall_value) <- y_names
    for(outer_y in y_names){ 
    Y_axis <- -1*tapply((my_data$readme2-my_data[,outer_y]), my_data$sampling_scheme_fullName, AggFxn) 
    X_axis <- rank(tapply(my_data$PrDDiv, my_data$sampling_scheme_fullName, mean)); 
    my_col <- rep("black", length(X_axis))
    my_col[names(X_axis) == "*Empirical*"] <- "red"
    if(outer_y=="va2_error"){my_col[] <- "green"}
    if(T == T){ 
      counter_iter <- counter_iter + 1 
      xaxt <- c(min(X_axis)-0.55*(max(X_axis)-min(X_axis)), 
                max(X_axis)+0.55*(max(X_axis)-min(X_axis)))
      X_axis <- X_axis + runif(length(X_axis), -0.02, 0.2)
      if(counter_iter == 1){
        text(X_axis, -0.25, srt = 90, adj = 1,
             labels = names(X_axis), xpd = TRUE, col = my_col)
        text(length(X_axis), -1.34, srt = 0, adj = 1,cex = 1.5, 
             labels =  "Sim. Design (Ordered by Average Proportion Div.)", xpd = TRUE)
      } 
      points(X_axis,Y_axis,# labels = my_label, 
           cex.lab = 1.5, 
           col = my_col, 
           cex = 0.50, pch = 1)#3 ); 
      overall_value[counter_iter] <- mean(Y_axis, na.rm = T) 
      Y_axis_mat <- rbind(Y_axis_mat , Y_axis)
    } 
    } 
    points(X_axis, apply(Y_axis_mat,2,median), pch = "-----",cex = 2, col = "darkgray")
    row.names(Y_axis_mat) <- y_names
    winner_indices <- apply(Y_axis_mat, 2, function(x){
      my_winner <- which(x == min(x, na.rm = T))
      #if(length(my_winner) > 1){ my_winner <- sample(which(x == min(x, na.rm = T)), 1) } 
      return( my_winner )  
      })
    print(  table(  row.names(Y_axis_mat)[winner_indices] )   ) 
    print(  prop.table(table(  row.names(Y_axis_mat)[winner_indices] )   ) )
    length(row.names(Y_axis_mat)[winner_indices])
    length(unique(row.names(Y_axis_mat)[winner_indices]) )
    } 
  dev.off()

  pdf("~/Downloads/FULL_prop_bysubsample.pdf")
  my_q <- 10  
  if(T == T){ 
    set.seed(2)
    baseline_name <- "readme2_error"
    axis_vec <- c(my_data$PrDDiv)
    competitor_names <- c("continuousRegression_error","continuousEnsemble_error", "continuousBayes_error", 
                          "continuousSVM_error", "continuousForest_error","continuousRegression_error_count", 
                          "continuousEnsemble_error_count", "continuousBayes_error_count", "continuousSVM_error_count", 
                          "continuousForest_error_count", 
                          "discreteEnsemble_error", "discreteBayes_error", "discreteForest_error", 
                          "discreteRegression_error", "discreteSVM_error", "discreteEnsemble_error_count", 
                          "discreteBayes_error_count", "discreteForest_error_count", "discreteRegression_error_count",
                          "discreteSVM_error_count", 
                           
                          "quantify_adjCount_error", "quantify_friedman_error", "quantify_hdx_error", 
                          "quantify_medianSweep_error", "quantify_mixtureHPMF_error", "quantify_mixtureL1_error", 
                          "quantify_mixtureL2_error", "quantify_prob_error", 
                          
                          "readme_error", "va2_error")
    EstimateTypeKey <- SpaceKey <- rep("", times = length(competitor_names))
    SpaceKey[grepl(competitor_names, pattern = "continuous")] <- "Cont."
    SpaceKey[!grepl(competitor_names, pattern = "continuous")] <- "Discrete"
    EstimateTypeKey[grepl(competitor_names, pattern = "quantify")] <- "Quant."
    EstimateTypeKey[grepl(competitor_names, pattern = "va2")] <- "Quant."
    EstimateTypeKey[grepl(competitor_names, pattern = "readme_error")] <- "Quant."
    EstimateTypeKey[grepl(competitor_names, pattern = "count")] <- "CountClass"
    EstimateTypeKey[EstimateTypeKey==""] <- "AveProbs"
    
    TypeKey <- paste(SpaceKey, EstimateTypeKey )
    
    for(asdf in 1:length(competitor_names)){ 
      eval(parse(text = sprintf("base_error_vec%s <- c(my_data$%s)", 
                                asdf, competitor_names[asdf]) ) )
    }
    alt_error_vec <- c(my_data[,baseline_name])
    
    par(mfrow = c(1,1))
    library(gtools)
    
    axis_vec_orig <- axis_vec
    axis_vec <- c(gtools::quantcut(axis_vec, q = my_q))
    names(axis_vec) <- names(axis_vec_orig)
    NComparisons <- length(competitor_names)
    
    for(iter_i in 1:NComparisons){ 
      eval(parse(text  = sprintf("v%s <- (alt_error_vec <= base_error_vec%s)", iter_i, iter_i)))
      eval(parse(text = sprintf("v%s <-  tapply(v%s, axis_vec, mean)", iter_i,iter_i)))
    } 
  
    if(T == T){ 
      par(mar=c(5, 5, 4, 2) )
      plot(names(v1),  v1, 
           pch = 1,
           ylim = c(0, 1.05),
           cex.lab = 2, 
           xaxt = "n", 
           main = "", 
           cex.main = 2, 
           col = "darkgray", 
           ylab = c("Proportion of Datasets with Higher SAE"), 
           xlab = "", # "Proportion Divergence (Quantiles)", 
           type = "b")
      axis(1,names(v1), names(v1))
      abline(h = 0.5, lwd = 3, lty = 2, col = "black")
      text(length(v1) - 2, 0.52, label = "higher SAE than readme2")
      text(length(v1) - 2, 0.48, label = "lower SAE than readme2")
      
      
      myDF <- eval(parse(text =  paste("data.frame(", paste(sprintf("v%s = v%s", 1:NComparisons, 1:NComparisons), 
                                                            collapse = ","), ")")))
      best_vec <- c()
      for(i in 1:ncol(myDF)){
        accept_x_locals <- c(1,2,3)
        target <- myDF[accept_x_locals,i] 
        error_mat <- matrix(NA, nrow = length(accept_x_locals), ncol = ncol(myDF))
        for(j in (1:ncol(myDF))[!1:ncol(myDF) %in% i] ){ 
          error_mat[,j] <- abs(myDF[accept_x_locals,j] - target)
        }
        my_max <- apply(error_mat, 1, function(x){min(x, na.rm = T)})
        which_best <- which(my_max == max(my_max, na.rm = T))
        best_vec[i] <- which_best[sample(1:length(which_best), 1)]
      }
      
      col_vec <- as.numeric(as.factor(TypeKey))
      
      for(iter_i in 1:NComparisons){ 
        if(eval(parse(text = sprintf("!all(is.na(v%s))", iter_i)))){ 
          eval(parse(text = sprintf("v%s[v%s == 1] <-v%s[v%s == 1] +  rnorm(length(v%s[v%s == 1]), sd = 0.01)",  iter_i, iter_i, iter_i, iter_i, iter_i, iter_i)))
          eval(parse(text = sprintf("points( f2n(names(v%s))+rnorm(length( names(v%s)), sd = 0.01), v%s, 
                                    col = f2n(col_vec[iter_i]),
                                    type = 'b')", iter_i, iter_i, iter_i)))
        }
      }
      
      legend("bottomright",legend = c("Ave Probs (C)", "Count+Classify (C)", "Ave Probs (D)", "Count+Classify (D)", 
                        "Quantification (D)"), col = unique(col_vec), lty = 1, pch = 1, bty = "n")
    }
  } 
  dev.off() 
  
  #Arrow plots
  pdf("~/Downloads/FArrow.pdf")
  if(T == T){ 
    AggFxn <- function( x){mean(x, na.rm = T)}
    arrow_fxn <- function(alt_error_vec, base_error_vec, 
                          xlab = NULL, 
                          x_data = NULL){
      if(is.null(xlab)){xlab <- "Ordered by Improvement"}
      
      if(is.null(x_data)){order_by <-  -((alt_error_vec - base_error_vec)) } 
      if(!is.null(x_data)){order_by <-  x_data } 
      lty_vec = rep(1, times = length( alt_error_vec ) ) 
      lty_vec[base_error_vec < alt_error_vec] <- 3
      par(mar=c(5, 5, 4, 2) ) 
      base_error_vec <- base_error_vec[order(order_by)]
      alt_error_vec <- alt_error_vec[order(order_by)]
      lty_vec <- lty_vec[order(order_by)]
      plot(0, 
           cex = 0, 
           cex.main = 1, 
           cex.lab = 1.75, 
           xaxt = "n", 
           xlim = c(1, length(base_error_vec)), 
           ylim = c(min(c(base_error_vec, alt_error_vec)), max(c(base_error_vec, alt_error_vec))), 
           #ylim = c(0, max(alt_error_vec, base_error_vec)+0), 
           ylab  = "Sum of Absolute Errors (SAE)", 
           xlab = xlab)
      col_vec <- rep("black", times = length( base_error_vec)) 
      col_vec[grepl(names(alt_error_vec), pattern = "enron", ignore.case = T)] <- "red"
      col_vec[grepl(names(alt_error_vec), pattern = "clinton", ignore.case = T)] <- "orange"
      col_vec[grepl(names(alt_error_vec), pattern = "immigration", ignore.case = T)] <- "green"
      col_vec[grepl(names(alt_error_vec), pattern = "stanford", ignore.case = T)] <- "purple"
      arrows(
        x0 = 1:length(alt_error_vec), y0 = base_error_vec,
        x1 = 1:length(alt_error_vec), y1 = alt_error_vec, 
        length = 0.10,
        #length = 0.1, 
        col = col_vec, 
        lty = lty_vec)
      try(text(x = which(grepl(names(alt_error_vec), pattern = "immigration", ignore.case = T)),  
               y = base_error_vec[grepl(names(alt_error_vec), pattern = "immigration", ignore.case = T)]+0.08, 
               label = "Immigration", col = "green", srt = 90), T)
      try(text(x = which(grepl(names(alt_error_vec), pattern = "clinton", ignore.case = T)),  
               y = base_error_vec[grepl(names(alt_error_vec), pattern = "clinton", ignore.case = T)]+0.05, 
               label = "Clinton", col = "orange", srt = 90), T)
      try(text(x = which(grepl(names(alt_error_vec), pattern = "enron", ignore.case = T)),  
               y = base_error_vec[grepl(names(alt_error_vec), pattern = "enron", ignore.case = T)]+0.04, 
               label = "Enron", col = "red", srt = 90), T)
      try(text(x = which(grepl(names(alt_error_vec), pattern = "stanford", ignore.case = T)),  
               y = base_error_vec[grepl(names(alt_error_vec), pattern = "stanford", ignore.case = T)]-0.13, 
               label = "Stanford", col = "purple", srt = 90), T)
    }
    
    #ARROW 1
    base_error_vec11 <- tapply(my_data$readme_error, my_data$dataset, function(x) AggFxn(x))
    alt_error_vec <- tapply(my_data$readme2_error, my_data$dataset, function(x) AggFxn(x))
    summary( (alt_error_vec-base_error_vec11)/base_error_vec11) 
    mean(alt_error_vec <= base_error_vec11)
    
    arrow_fxn(alt_error_vec = alt_error_vec, 
              base_error_vec = base_error_vec11, 
              xlab = NULL, 
              x_data = NULL)
  }
  dev.off()
}


 