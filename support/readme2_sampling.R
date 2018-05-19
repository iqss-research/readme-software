GetLabeledUnlabeledIndices <- function(csv_undergrad_input, labeled_sz, unlabeled_sz, sampling_scheme, 
                                       vecs_input_ordered){ 
if(sampling_scheme == "JustPermutations"){ 
  print("BEGIN Just Permutations")
  labeled_indices <- sample(1:nrow(csv_undergrad_input), labeled_sz, replace = F)
  remaining_indices <- (1:nrow(csv_undergrad_input))
  remaining_indices <- remaining_indices[!remaining_indices %in% labeled_indices]
  unlabeled_indices <- sample(remaining_indices, min(length(remaining_indices), 
                                                unlabeled_sz), replace = F)
} 

if(sampling_scheme == "MinDiv"){ 
  print("BEGIN Min Div.")
  bestV <- Inf
  for(i in 1:1000){ 
      labeled_indices <- sample(1:nrow(csv_undergrad_input), labeled_sz, replace = F)
      remaining_indices <- (1:nrow(csv_undergrad_input))
      remaining_indices <- remaining_indices[!remaining_indices %in% labeled_indices]
      unlabeled_indices <- sample(remaining_indices, min(length(remaining_indices), 
                                                    unlabeled_sz), replace = F)
      
      train_pd <- table(  as.character( csv_undergrad_input$CATEGORY[labeled_indices] )   )  
      unlabeled_pd <- table(   as.factor(csv_undergrad_input$CATEGORY)[unlabeled_indices] ) 
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
  print("BEGIN Max Div.")
  bestV <- -Inf
  for(i in 1:1000){ 
    labeled_indices <- sample(1:nrow(csv_undergrad_input), labeled_sz, replace = F)
    remaining_indices <- (1:nrow(csv_undergrad_input))
    remaining_indices <- remaining_indices[!remaining_indices %in% labeled_indices]
    unlabeled_indices <- sample(remaining_indices, min(length(remaining_indices),
                                                  unlabeled_sz), replace = F)
    
    train_pd <- table(  as.character( csv_undergrad_input$CATEGORY[labeled_indices] )   )  
    unlabeled_pd <- table(   as.factor(csv_undergrad_input$CATEGORY)[unlabeled_indices] ) 
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
  print("BEGIN Historical")
  HistoricalIndices <- historical_fxn(INPUT_DATA = csv_undergrad_input, INPUT_CAT = csv_undergrad_input$CATEGORY , INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz)
  unlabeled_indices <- HistoricalIndices$unlabeled_indices
  labeled_indices <- HistoricalIndices$labeled_indices
} 

if(sampling_scheme == "HistoricalHugeTrain"){ 
  print("HistoricalHugeTrain")
  HistoricalIndices <- historical_fxn(INPUT_DATA = csv_undergrad_input, INPUT_CAT = csv_undergrad_input$CATEGORY , 
                                      INPUT_labeled_sz = nrow(csv_undergrad_input)-unlabeled_sz, 
                                      INPUT_unlabeled_sz = unlabeled_sz)
  unlabeled_indices <- HistoricalIndices$unlabeled_indices
  labeled_indices <- HistoricalIndices$labeled_indices
} 

if(sampling_scheme == "HistoricalVarySampSize"){ 
  print("BEGIN_HistoricalVarySampSize")
  HistoricalIndices <- historical_fxn(INPUT_DATA = csv_undergrad_input, INPUT_CAT = csv_undergrad_input$CATEGORY , 
                                      INPUT_labeled_sz = sample(c(100, 300, 500, 1000), 1), 
                                      INPUT_unlabeled_sz = unlabeled_sz)
  unlabeled_indices <- HistoricalIndices$unlabeled_indices
  labeled_indices <- HistoricalIndices$labeled_indices
} 

if(sampling_scheme == "Historical2"){ 
  print("BEGIN Historical2")
  HistoricalIndices <- historical_fxn2(INPUT_DATA = csv_undergrad_input, INPUT_CAT = csv_undergrad_input$CATEGORY , INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz)
  unlabeled_indices <- HistoricalIndices$unlabeled_indices
  labeled_indices <- HistoricalIndices$labeled_indices
} 

if(sampling_scheme == "Historical3"){ 
  print("BEGIN Historical3")
  HistoricalIndices <- historical_fxn3(INPUT_DATA = csv_undergrad_input, INPUT_CAT = csv_undergrad_input$CATEGORY , INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz)
  unlabeled_indices <- HistoricalIndices$unlabeled_indices
  labeled_indices <- HistoricalIndices$labeled_indices
} 

if(sampling_scheme == "Ahistorical_NoReuse"){ 
  print("BEGIN Ahistorical NoReuse 1")
  AHistoricalIndices <- ahistorical_fxn(INPUT_DATA = csv_undergrad_input, INPUT_CAT = csv_undergrad_input$CATEGORY ,
                                        INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz)
  labeled_indices <- AHistoricalIndices$labeled_indices
  unlabeled_indices <- AHistoricalIndices$unlabeled_indices
} 

if(sampling_scheme == "Ahistorical_NoReuse2"){ 
  print("BEGIN Ahistorical NoReuse 2")
  AHistoricalIndices <- ahistorical_fxn2(INPUT_DATA = csv_undergrad_input, INPUT_CAT = csv_undergrad_input$CATEGORY ,
                                        INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz)
  labeled_indices <- AHistoricalIndices$labeled_indices
  unlabeled_indices <- AHistoricalIndices$unlabeled_indices
} 

previousTestSize <- NULL 
if(sampling_scheme == "Ahistorical_aykut"){
  print("BEGIN Aykut Ahistorical")
  AHistoricalIndices_aykut <- aykut_fxn(INPUT_DATA = csv_undergrad_input, INPUT_CAT = csv_undergrad_input$CATEGORY ,
                                        INPUT_labeled_sz = labeled_sz, INPUT_unlabeled_sz = unlabeled_sz, 
                                          previousTestSize=previousTestSize) 
  labeled_indices <- AHistoricalIndices_aykut$labeled_indices
  unlabeled_indices <- AHistoricalIndices_aykut$unlabeled_indices
  previousTestSize <- table(  csv_undergrad_input$CATEGORY[unlabeled_indices] ) 
}
  
if(sampling_scheme == "Ahistorical_aykut_quantification"){
  previousTestSize <- NULL 
  print("BEGIN Aykut Ahistorical 2")
  AHistoricalIndices_aykut_quantification <- aykut_fxn_quantification(INPUT_DATA = csv_undergrad_input, 
                                                                      INPUT_CAT = csv_undergrad_input$CATEGORY ,
                                                                      labeled_sz = labeled_sz, unlabeled_sz = unlabeled_sz) 
  labeled_indices <- AHistoricalIndices_aykut_quantification$labeled_indices
  unlabeled_indices <- AHistoricalIndices_aykut_quantification$unlabeled_indices
}

if(sampling_scheme == "breakdown_sampling"){
  print("BEGIN Breakdown Sampling 1")
  breakdown_sample_results <- breakdown_sample(INPUT_DATA = csv_undergrad_input, 
                   INPUT_CAT=csv_undergrad_input$CATEGORY, 
                   labeled_sz = ceiling(runif(1, 75, 500)), 
                   unlabeled_sz = unlabeled_sz)
  labeled_indices <- breakdown_sample_results$labeled_indices
  unlabeled_indices <- breakdown_sample_results$unlabeled_indices
} 

if(sampling_scheme == "breakdown_sampling2"){
  print("BEGIN Breakdown Sampling 2")
  breakdown_sample2_results <- breakdown_sample2(INPUT_DATA = csv_undergrad_input, 
                                               INPUT_CAT=csv_undergrad_input$CATEGORY, 
                                               labeled_sz = labeled_sz, 
                                               unlabeled_sz = unlabeled_sz, 
                                               VECS_INPUT=DocSummaries_input)
  labeled_indices <- breakdown_sample2_results$labeled_indices
  unlabeled_indices <- breakdown_sample2_results$unlabeled_indices
} 

if(sampling_scheme == "Uniform_XDiv"){
  print("BEGIN Uniform_XDiv")
  Uniform_XDiv_results <- Uniform_XDiv(INPUT_DATA = csv_undergrad_input, 
                                                 INPUT_CAT=csv_undergrad_input$CATEGORY, 
                                                 labeled_sz = labeled_sz, 
                                                 unlabeled_sz = unlabeled_sz, 
                                                 VECS_INPUT=DocSummaries_input)
  labeled_indices <- Uniform_XDiv_results$labeled_indices
  unlabeled_indices <- Uniform_XDiv_results$unlabeled_indices
} 

if(sampling_scheme == "Max_XDiv"){
  print("BEGIN Max_XDiv")
  Max_XDiv_results <- Max_XDiv(INPUT_DATA = csv_undergrad_input, 
                                       INPUT_CAT=csv_undergrad_input$CATEGORY, 
                                       labeled_sz = labeled_sz, 
                                       unlabeled_sz = unlabeled_sz, 
                                       VECS_INPUT=DocSummaries_input)
  labeled_indices <- Max_XDiv_results$labeled_indices
  unlabeled_indices <- Max_XDiv_results$unlabeled_indices
} 

if(sampling_scheme == "Min_XDiv"){
  print("BEGIN Min_XDiv")
  Min_XDiv_results <- Min_XDiv(INPUT_DATA = csv_undergrad_input, 
                               INPUT_CAT=csv_undergrad_input$CATEGORY, 
                               labeled_sz = labeled_sz, 
                               unlabeled_sz = unlabeled_sz, 
                               VECS_INPUT=DocSummaries_input)
  labeled_indices <- Min_XDiv_results$labeled_indices
  unlabeled_indices <- Min_XDiv_results$unlabeled_indices
} 

return(list(labeled_indices = labeled_indices, 
            unlabeled_indices = unlabeled_indices) )
} 