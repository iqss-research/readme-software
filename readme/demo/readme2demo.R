##########
### An example for how to generate estimates using readme2
##########

## Installation
library(readme)
library(tensorflow)

## Load the data
data(clinton, package="readme")

## Load in the word vectors
download_wordvecs()

## Generate a word vector summary for each document using the cleaned text
word_vectors = undergrad(documentText = cleanme(clinton$TEXT), wordVecs = NULL)

# Estimate category proportions
set.seed(2138) # Set a seed
readme.estimates <- readme(dfm = as.matrix(word_vectors) , labeledIndicator = clinton$TRAININGSET, categoryVec = clinton$TRUTH)

# Output proportions estimate
readme.estimates$point_readme

# Compare to the truth
table(clinton$TRUTH[clinton$TRAININGSET == 0])/sum(table((clinton$TRUTH[clinton$TRAININGSET == 0])))