# readme2

An R package for estimating category proportions in an unlabeled set of documents given a labeled set, by implementing the method described in [Jerzak, King, and Strezhnev (2019)](http://GaryKing.org/words). This method is meant to improve on the ideas in Hopkins and King (2010), which introduced a quantification algorithm to estimate category proportions without directly classifying individual observations. This version of the software refines the original method by implementing a technique for selecitng optimal textual features in order to minimize the error of the estimated category proportions. Automatic differentiation, stochastic gradient descent, and batch re-normalization are used to carry out the optimization. Other pre-processing functions are available, as well as an interface to the earlier version of the algorithm for comparison. The package also provides users with the ability to extract the generated features for use in other tasks.

(*Here's the abstract from our paper:*  Computer scientists and statisticians are often interested in classifying textual documents into chosen categories. Social scientists and others are often less interested in any one document and instead try to estimate the proportion falling in each category. The two existing types of techniques for estimating these category proportions are parametric "classify and count" methods and "direct" nonparametric estimation of category proportions without an individual classification step. Unfortunately, classify and count methods can sometimes be highly model dependent or generate more bias in the proportions even as the percent correctly classified increases. Direct estimation avoids these problems, but can suffer when the meaning and usage of language is too similar across categories or too different between training and test sets. We develop an improved direct estimation approach without these problems by introducing continuously valued text features optimized for this problem, along with a form of matching adapted from the causal inference literature. We evaluate our approach in analyses of a diverse collection of 73 data sets, showing that it substantially improves performance compared to existing approaches. As a companion to this paper, we offer easy-to-use software that implements all ideas discussed herein.)

## Installation

The most recent version of `readme2` can be installed directly from the repository using the `devtools` package

```
devtools::install_github("iqss-research/readme-software/readme")
```

`readme2` depends on the `limSolve` and `FNN` packages which can be installed directly from CRAN

It also utilizes `tensorflow`, the R interface to the TensorFlow API -- an open source machine learning library. To install `tensorflow` follow the instructions available at the [RStudio page on R and TensorFlow](https://tensorflow.rstudio.com/tensorflow/).

First, install the R package via github

```
devtools::install_github("rstudio/tensorflow")
```

Then, install TensorFlow itself via the R function `install_tensorflow()`.

```
library(tensorflow)
install_tensorflow()
```

You will also need to obtain a set of word embeddings that map terms in the texts to a vector representation. We recommend the pre-trained GloVe vectors available at [https://nlp.stanford.edu/projects/glove/](https://nlp.stanford.edu/projects/glove/), although you will want different embeddings for other languages and unusual contexts. In the vignette below, we will use the 50-dimensional vectors trained on Wikpedia articles and Gigaword 5: "glove.6B.50d.txt"

## Walkthrough

In this section, we provide a step-by-step vignette illustrating how to use `readme2` to estimate category proportions in an unlabeled set of documents. To begin, we assume that the user has a set of *labeled* documents, each of which has been as assigned a single, mutually exclusive category label, $D_i$ (for observation $i$). Often this is done by manual coding. We observe an unlabeled set of documents and want to estimate the proportion of documents with the category label $d$ -- denoted $Pr(D_i = d)$ -- for all categories, $\{1, 2, \dotsc, |D|\}$. The central intuition of the original `readme` is that for any individual feature $S$ in both the labeled and unlabeled set, we know from the Law of Total Probability that the following relationship holds.

$$ E[S] = \sum_{d=1}^{|D|} E[S|D = d] Pr(D_i = d) $$

While we observe $S$ in the unlabeled set, we do not observe the conditional frequency of $S$ in each category $d$ (denoted $E[S|D=d]$). We estimate the unlabeled set conditional frequency using the labeled set conditional frequency and solve for the vector $Pr(D) = \{Pr(D_i = 1), Pr(D_i = 2), \dotsc, Pr(D_i = |D|)\}$ via standard linear programming methods.

There are many possible features $S$ that can be extracted from the text. The main contribution of `readme2` is to develop a way for selecting optimal sets of features from a large space of potential document summaries. We start by converting each document into a vector of summaries based on the word vector representations of each term in the document.

### Processing the text documents 

We illustrate the method using the provided `clinton` dataset of a subset of handcoded blogposts from the original Hopkins and King (2010) paper. 

```
library(readme)
data(clinton, package="readme")
```

This dataset is comprised of 1676 documents coded into 6 mutually exclusive categories (`TRUTH`). 

The first task is to convert the raw text for each document (`TEXT`) into a document-feature matrix using the word vector summaries. We start by loading in the word vector summaries into a table that can be referenced by the `undergrad()` function.

```
## Load in the word vectors (we assume they are saved as a .txt file in your Downloads folder) in the format used at <https://nlp.stanford.edu/projects/glove/>
wordVecs_corpus <- data.table::fread("./Downloads/glove.twitter.27B.200d.txt")
wordVecs_keys <- wordVecs_corpus[[1]]## first row is the name of the term
wordVecs_corpus <- as.matrix (  wordVecs_corpus[,-1] )  #
row.names(wordVecs_corpus) <- wordVecs_keys
rm(wordVecs_keys)## Remove the original loaded table to save space
```

The `undergrad()` function then takes as input the raw document texts and the word vector dictionary and returns a set of feature summaries for each document in the dataset. `cleanme()` pre-processes the text:
```
## Generate a word vector summary for each document
wordVec_summaries = undergrad(documentText = cleanme(clinton$TEXT), wordVecs = wordVecs_corpus)
```

### Estimating topic proportions with `readme2`

With the topic, training set labels and features we can start estimating the model.

```
# Estimate category proportions
set.seed(2138) # Set a seed
readme.estimates <- readme(dfm = wordVec_summaries , labeledIndicator = clinton$TRAININGSET, categoryVec = clinton$TRUTH)
```

We can compare the output with the true category codings

```
# Output proportions estimate
readme.estimates$point_readme
# Compare to the truth
table(clinton$TRUTH[clinton$TRAININGSET == 0])/sum(clinton$TRUTH[clinton$TRAININGSET == 0])
```

## Versions



## License


## Acknowledgments

