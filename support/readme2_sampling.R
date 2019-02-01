GetLabeledUnlabeledIndices <- function(csv_category, labeled_sz, unlabeled_sz, sampling_scheme, 
                                       docSummaries_input = NULL, ProjectionsMat_input = NULL){ 
if(sampling_scheme == "JustPermutations"){ 
  labeled_indices <- sample(1:length(csv_category), labeled_sz, replace = F)
  remaining_indices <- (1:length(csv_category))
  remaining_indices <- remaining_indices[!remaining_indices %in% labeled_indices]
  unlabeled_indices <- sample(remaining_indices, min(length(remaining_indices), 
                                                unlabeled_sz), replace = F)
} 
  
if(sampling_scheme == "firat2018"){ 
    temp_ <- firat2018(csv_category_ = csv_category, INPUT_labeled_sz = labeled_sz, 
                       INPUT_unlabeled_sz = unlabeled_sz)
    labeled_indices  <- temp_$labeled_indices 
    unlabeled_indices   <- temp_$unlabeled_indices 
} 
  
if(sampling_scheme == "MinDiv"){ 
  bestV <- Inf
  for(i in 1:1000){ 
      labeled_indices <- sample(1:length(csv_category), labeled_sz, replace = F)
      remaining_indices <- (1:length(csv_category))
      remaining_indices <- remaining_indices[!remaining_indices %in% labeled_indices]
      unlabeled_indices <- sample(remaining_indices, min(length(remaining_indices), 
                                                    unlabeled_sz), replace = F)
      
      train_pd <- table(  as.character( csv_category[labeled_indices] )   )  
      unlabeled_pd <- table(   as.factor(csv_category)[unlabeled_indices] ) 
      unlabeled_pd <- unlabeled_pd[names( train_pd)]
      unlabeled_pd[is.na(unlabeled_pd)] <- 0 
      unlabeled_pd <- unlabeled_pd/sum(unlabeled_pd)
      train_pd <- train_pd/sum(train_pd)
      
      myV <- sum(abs(train_pd - unlabeled_pd[names(unlabeled_pd)]))
      if(myV <= bestV){ 
        bestV <- myV  
        labeled_indices_best <- labeled_indices
        unlabeled_indices_best <- unlabeled_indices
      }
  }
  unlabeled_indices <- unlabeled_indices_best
  labeled_indices <- labeled_indices_best
} 

if(sampling_scheme == "MaxDiv"){ 
  bestV <- -Inf
  for(i in 1:1000){ 
    labeled_indices <- sample(1:length(csv_category), labeled_sz, replace = F)
    remaining_indices <- (1:length(csv_category))
    remaining_indices <- remaining_indices[!remaining_indices %in% labeled_indices]
    unlabeled_indices <- sample(remaining_indices, min(length(remaining_indices),
                                                  unlabeled_sz), replace = F)
    
    train_pd <- table(  as.character( csv_category[labeled_indices] )   )  
    unlabeled_pd <- table(   as.factor(csv_category)[unlabeled_indices] ) 
    unlabeled_pd <- unlabeled_pd[names( train_pd)]
    unlabeled_pd[is.na(unlabeled_pd)] <- 0 
    unlabeled_pd <- unlabeled_pd/sum(unlabeled_pd)
    train_pd <- train_pd/sum(train_pd)
    
    myV <- sum(abs(train_pd - unlabeled_pd[names(unlabeled_pd)]))
    if(myV >= bestV){ 
      bestV <- myV  
      labeled_indices_best <- labeled_indices
      unlabeled_indices_best <- unlabeled_indices
    }
  }
  unlabeled_indices <- unlabeled_indices_best
  labeled_indices <- labeled_indices_best
}

if(sampling_scheme == "Historical"){ 
  HistoricalIndices <- historical_fxn(INPUT_CAT = csv_category , INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz)
  unlabeled_indices <- HistoricalIndices$unlabeled_indices
  labeled_indices <- HistoricalIndices$labeled_indices
} 

if(sampling_scheme == "HistoricalVarySampSize"){ 
  train_sz_sampled = sample(c(100, 300, 500, 1000), 1)
  test_sz_sampled =  unlabeled_sz
  if(train_sz_sampled+test_sz_sampled >  length(csv_category)){
    train_sz_sampled = length(csv_category) / 2
    test_sz_sampled =  min(length(csv_category)-train_sz_sampled, unlabeled_sz)
  }
  HistoricalIndices <- historical_fxn(INPUT_CAT = csv_category , 
                                      INPUT_labeled_sz = train_sz_sampled, 
                                      INPUT_unlabeled_sz = min(length(csv_category)-train_sz_sampled, 
                                                                unlabeled_sz) )  
  unlabeled_indices <- HistoricalIndices$unlabeled_indices
  labeled_indices <- HistoricalIndices$labeled_indices
} 

if(sampling_scheme == "Sequential"){ 
  HistoricalIndices <- sequential_fxn(INPUT_CAT = csv_category , INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz)
  unlabeled_indices <- HistoricalIndices$unlabeled_indices
  labeled_indices <- HistoricalIndices$labeled_indices
} 

if(sampling_scheme == "HistoricalMaxU"){ 
  HistoricalIndices <- historical_maxout(INPUT_CAT = csv_category , INPUT_labeled_sz = labeled_sz)
  unlabeled_indices <- HistoricalIndices$unlabeled_indices
  labeled_indices <- HistoricalIndices$labeled_indices
} 
  
  if(sampling_scheme == "HistoricalMaxL"){ 
    HistoricalIndices <- historical_maxout(INPUT_CAT = csv_category , INPUT_labeled_sz = length(csv_category)-unlabeled_sz)
    unlabeled_indices <- HistoricalIndices$unlabeled_indices
    labeled_indices <- HistoricalIndices$labeled_indices
  } 
  

if(sampling_scheme == "Ahistorical_NoReuse"){ 
  AHistoricalIndices <- ahistorical_fxn(INPUT_CAT = csv_category ,
                                        INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz)
  labeled_indices <- AHistoricalIndices$labeled_indices
  unlabeled_indices <- AHistoricalIndices$unlabeled_indices
} 

if(sampling_scheme == "Ahistorical_NoReuse2"){ 
  AHistoricalIndices <- ahistorical_fxn2(INPUT_CAT = csv_category ,
                                        INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz)
  labeled_indices <- AHistoricalIndices$labeled_indices
  unlabeled_indices <- AHistoricalIndices$unlabeled_indices
} 

previousTestSize <- NULL 
if(sampling_scheme == "Ahistorical_aykut"){
  AHistoricalIndices_aykut <- aykut_fxn(INPUT_CAT = csv_category ,
                                        INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz, 
                                          previousTestSize=previousTestSize) 
  labeled_indices <- AHistoricalIndices_aykut$labeled_indices
  unlabeled_indices <- AHistoricalIndices_aykut$unlabeled_indices
  previousTestSize <- table(  csv_category[unlabeled_indices] ) 
}

if(sampling_scheme == "Ahistorical_aykut_quantification"){
  previousTestSize <- NULL 
  AHistoricalIndices_aykut_quantification <- aykut_fxn_quantification(INPUT_CAT = csv_category ,
                                                                      INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz) 
  labeled_indices <- AHistoricalIndices_aykut_quantification$labeled_indices
  unlabeled_indices <- AHistoricalIndices_aykut_quantification$unlabeled_indices
}

if(sampling_scheme == "breakdown_sampling"){
  breakdown_sample_results <- breakdown_sample(INPUT_CAT=csv_category, 
                                               INPUT_labeled_sz = labeled_sz, 
                                               INPUT_unlabeled_sz = unlabeled_sz)
  labeled_indices <- breakdown_sample_results$labeled_indices
  unlabeled_indices <- breakdown_sample_results$unlabeled_indices
} 

if(sampling_scheme == "breakdown_sampling2"){
  breakdown_sample2_results <- breakdown_sample2(INPUT_CAT = csv_category, 
                                                 INPUT_labeled_sz = labeled_sz, 
                                                 INPUT_unlabeled_sz = unlabeled_sz, 
                                               PROJECTIONS_INPUT=ProjectionsMat_input)
  labeled_indices <- breakdown_sample2_results$labeled_indices
  unlabeled_indices <- breakdown_sample2_results$unlabeled_indices
} 

if(sampling_scheme == "Uniform_XDiv"){
  Uniform_XDiv_results <- Uniform_XDiv(INPUT_CAT  = csv_category,
                                       INPUT_labeled_sz = labeled_sz, 
                                       INPUT_unlabeled_sz = unlabeled_sz, 
                                       PROJECTIONS_INPUT=ProjectionsMat_input)
  labeled_indices <- Uniform_XDiv_results$labeled_indices
  unlabeled_indices <- Uniform_XDiv_results$unlabeled_indices
} 

if(sampling_scheme == "Max_XDiv"){
  Max_XDiv_results <- Max_XDiv(INPUT_CAT=csv_category, 
                               INPUT_labeled_sz = labeled_sz, 
                               INPUT_unlabeled_sz = unlabeled_sz, 
                               PROJECTIONS_INPUT=ProjectionsMat_input)
  labeled_indices <- Max_XDiv_results$labeled_indices
  unlabeled_indices <- Max_XDiv_results$unlabeled_indices
} 

if(sampling_scheme == "Min_XDiv"){
  Min_XDiv_results <- Min_XDiv(INPUT_DATA = csv_category, 
                               INPUT_CAT=csv_category, 
                               INPUT_labeled_sz = labeled_sz, 
                               INPUT_unlabeled_sz = unlabeled_sz, 
                               PROJECTIONS_INPUT=ProjectionsMat_input)
  labeled_indices <- Min_XDiv_results$labeled_indices
  unlabeled_indices <- Min_XDiv_results$unlabeled_indices
} 

return(list(labeled_indices = labeled_indices, 
            unlabeled_indices = unlabeled_indices) )
} 