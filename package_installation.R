##################################################
##INSTRUCTIONS FOR README PACKAGE USE ############
##################################################

# Generate documentation
{
 setwd("~/Documents/readme-software")
 devtools::document("./readme/")
 try(file.remove("./readme.pdf"),T)
 system("R CMD Rd2pdf readme")
}


#Install package
devtools::install_github("iqss-research/readme-software/readme")

#Load in package to environment
library(readme)

#For further instructions on use, see ?readme and ?undergrad, as well as readme.pdf

