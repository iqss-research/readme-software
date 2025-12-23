pkgname <- "readme"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "readme-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('readme')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("build_backend")
### * build_backend

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: build_backend
### Title: Build TensorFlow Backend
### Aliases: build_backend

### ** Examples

## Not run: 
##D # Build the default environment
##D build_backend()
##D 
##D # Build with a custom environment name
##D build_backend(conda_env = "my_readme_env")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("build_backend", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("clinton")
### * clinton

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: clinton
### Title: Clinton Email Dataset
### Aliases: clinton
### Keywords: datasets

### ** Examples

data(clinton)
head(clinton)

# See structure
str(clinton)

# Count labeled vs unlabeled
table(clinton$TRAININGSET)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("clinton", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("initialize_tensorflow")
### * initialize_tensorflow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: initialize_tensorflow
### Title: Initialize TensorFlow Backend
### Aliases: initialize_tensorflow

### ** Examples

## Not run: 
##D # Initialize the default environment
##D initialize_tensorflow()
##D 
##D # Initialize a custom environment
##D initialize_tensorflow(conda_env = "my_readme_env")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("initialize_tensorflow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readme-package")
### * readme-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readme-package
### Title: A algorithm for quantification that harnesses the Law of Total
###   Expectations in an optimal feature space
### Aliases: readme-package
### Keywords: internal

### ** Examples

## Not run: 
##D #set seed
##D set.seed(1)
##D 
##D #Generate synthetic 25-d word vector corpus.
##D my_wordVecs <- matrix(rnorm(11*25), ncol = 25)
##D row.names(my_wordVecs) <- c("the","true", "thine", "stars", "are",
##D                             "fire", ".", "to", "own", "self", "be")
##D 
##D #Generate 100 ``documents'' of 5-10 words each.
##D my_documentText <- replicate(100,
##D                              paste(sample(row.names(my_wordVecs),
##D                                           sample(5:10, 1),
##D                                           replace = TRUE),
##D                                    collapse = " ") )
##D 
##D #Assign labeled/unlabeled sets. The first 50 will be labeled; the rest unlabeled.
##D my_labeledIndicator <- rep(1, times = 100)
##D my_labeledIndicator[51:100] <- 0
##D 
##D #Assign category membership randomly
##D my_categoryVec <- sample(c("C1", "C2", "C3", "C4"), 100, replace = TRUE)
##D true_unlabeled_pd <- prop.table(table(my_categoryVec[my_labeledIndicator==0]))
##D my_categoryVec[my_labeledIndicator == 0] <- NA
##D 
##D #Get word vector summaries
##D my_dfm <- undergrad(documentText = my_documentText, wordVecs = my_wordVecs)
##D 
##D #perform estimation
##D readme_results <- readme(dfm = my_dfm,
##D                          labeledIndicator = my_labeledIndicator,
##D                          categoryVec = my_categoryVec,
##D                          nBoot = 2)
##D print(readme_results$point_readme)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readme-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readme")
### * readme

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readme
### Title: readme
### Aliases: readme

### ** Examples

## Not run: 
##D #set seed
##D set.seed(1)
##D 
##D #Generate synthetic 25-d word vector corpus.
##D my_wordVecs <- matrix(rnorm(11*25), ncol = 25)
##D row.names(my_wordVecs) <- c("the","true", "thine", "stars", "are",
##D                             "fire", ".", "to", "own", "self", "be")
##D 
##D #Generate 100 ``documents'' of 5-10 words each.
##D my_documentText <- replicate(100,
##D                              paste(sample(row.names(my_wordVecs),
##D                                           sample(5:10, 1),
##D                                           replace = TRUE),
##D                                    collapse = " ") )
##D 
##D #Assign labeled/unlabeled sets. The first 50 will be labeled; the rest unlabeled.
##D my_labeledIndicator <- rep(1, times = 100)
##D my_labeledIndicator[51:100] <- 0
##D 
##D #Assign category membership randomly
##D my_categoryVec <- sample(c("C1", "C2", "C3", "C4"), 100, replace = TRUE)
##D true_unlabeled_pd <- prop.table(table(my_categoryVec[my_labeledIndicator==0]))
##D my_categoryVec[my_labeledIndicator == 0] <- NA
##D 
##D #Get word vector summaries
##D my_dfm <- undergrad(documentText = my_documentText, wordVecs = my_wordVecs)
##D 
##D #perform estimation
##D readme_results <- readme(dfm = my_dfm,
##D                          labeledIndicator = my_labeledIndicator,
##D                          categoryVec = my_categoryVec,
##D                          nBoot = 2, sgdIters = 500)
##D print(readme_results$point_readme)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readme", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tensorflow_available")
### * tensorflow_available

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tensorflow_available
### Title: Check TensorFlow Availability
### Aliases: tensorflow_available

### ** Examples

## Not run: 
##D if (tensorflow_available()) {
##D   result <- readme(dfm, labeledIndicator, categoryVec)
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tensorflow_available", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("undergrad")
### * undergrad

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: undergrad
### Title: undergrad
### Aliases: undergrad

### ** Examples

#set seed
set.seed(1)

#Generate synthetic word vector corpus.
my_wordVecs <- matrix(rnorm(11*50), ncol = 50)
row.names(my_wordVecs) <- c("the","true", "thine", "stars", "are" ,
                              "fire", ".", "to", "own", "self", "be")

#Setup ``documents''
my_documentText <- c(
"the stars are fire .", #document 1
"to thine own self be true ", #document 2
"true stars be true ." #document 3
)

#Get document-level word vector summaries.
my_dfm <- undergrad(documentText = my_documentText, wordVecs = my_wordVecs)
print( my_dfm )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("undergrad", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
