#########################
##INSTRUCTIONS FOR USE 
#########################

##A. SET UP CORPORA
#Download 4 csv files from https://dataverse.harvard.edu/dataverse/fdfdferer323213asreadme2adme2
#Place them in "./readme-software/data"
#Add any other corpora as .csv files in this directory. 
#The .csv files should have 2 columns. 
#Column 1 should be named "CATEGORY" and should contain the category factors. 
#Column 2 should be named "RAWTEXT" and should contain the raw text. 

##B. SET UP WORK VECTOR DATABASE 
#Download a word vectors database of your choice. 
#We recommend the 50-dimensional GloVe embeddings. See https://nlp.stanford.edu/projects/glove/
#Save the embeddings in "./readme-software/data" as a .txt file where the first entry is the word key and the remaining entries are the features.
#The formatting should be the same as in this database of pre-trained vectors: http://nlp.stanford.edu/data/glove.twitter.27B.zip

#Set directory, modify as needed. 
mainDir <- "~/Downloads/readme-software" 
setwd(mainDir)

#Historical is chronological sampling. Other options are available. See Jerzak, King, and Strezhnev (2018). 
sampling_scheme <- "Historical"

#How many iterations to use per corpus? For stable results, try iterations setting > 25. 
iterations <- 2 

#Run battery
source("./support/readme2_master_public.R")

#Check out the results! 
#global_results contains the error values for ~30 quantification algorithms, as well as various diagnostics. 
head(global_results)