##########
### An example for how to generate estimates using readme2
##########

## Installation
library(readme)

## Set up TensorFlow backend (one-time setup)
# build_backend()  # Uncomment and run once to create the conda environment

## Initialize TensorFlow (run at start of each session)
initialize_tensorflow()

## Load the data
data(clinton, package = "readme")

## Download word vectors (one-time setup)
# download_wordvecs()  # Uncomment and run once to download GloVe vectors

## Generate a word vector summary for each document using the cleaned text
word_vectors <- undergrad(documentText = cleanme(clinton$TEXT), wordVecs = NULL)

## Estimate category proportions
set.seed(2138)  # Set a seed for reproducibility
readme.estimates <- readme(
  dfm = as.matrix(word_vectors),
  labeledIndicator = clinton$TRAININGSET,
  categoryVec = clinton$TRUTH
)

## Output proportions estimate
cat("Estimated proportions:\n")
print(readme.estimates$point_readme)

## Compare to the truth
cat("\nTrue proportions:\n")
true_props <- table(clinton$TRUTH[clinton$TRAININGSET == 0]) /
  sum(table(clinton$TRUTH[clinton$TRAININGSET == 0]))
print(true_props)
