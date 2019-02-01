#########################
##INSTRUCTIONS FOR USE 
#########################

##A. SET UP CORPORA
#Download csv files from https://dataverse.harvard.edu/dataverse/fdfdferer323213asreadme2adme2
#Place them in "./readme-software/data"
#Add any other corpora as .csv files in this directory. 
#The .csv files have 2 columns. 
#Column 1 is named "CATEGORY" and contains the category factors. 
#Column 2 is named "RAWTEXT" and contains the raw text. 

##B. SET UP WORK VECTOR DATABASE 
#Download a word vectors database of your choice. 
#We recommend the GloVe embeddings. See https://nlp.stanford.edu/projects/glove/
#Save the embeddings in "./readme-software/data" as a .txt file where the first entry is the word key and the remaining entries are the features.
#The formatting should be the same as in this database of pre-trained vectors: http://nlp.stanford.edu/data/glove.twitter.27B.zip

#clear workspace 
rm(list=ls())

#Set directory, modify as needed. 
mainDir <- "~/Downloads/readme-software" 
setwd(mainDir)

#Historical is chronological sampling. Other options are available. See Jerzak, King, and Strezhnev (2018). 
sampling_scheme <- "Historical"

#How many iterations to use per corpus? For stable results, try iterations setting > 5. 
iterations <- 1

#where should we look for the word vector corpus? 
wordVecs_pointer <- "~/Downloads/glove.twitter.27B.200d.txt" #from <http://nlp.stanford.edu/data/glove.twitter.27B.zip>

#Run battery. 
source("./support/readme2_sim_internals.R")

#Check out the results! 
#global_results contains the error values for ~30 other algorithms 
head(global_results)
