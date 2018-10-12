##################################################
##INSTRUCTIONS FOR readme PACKAGE USE ############
##################################################

#Set directory to the readme-software folder. 
setwd("~/Downloads/readme-software")
devtools::build("./readme")

#Install package 
install.packages("./readme.tar.gz", lib = "./", repos = NULL, type ="source",INSTALL_opts = c('--no-lock'))

#Load in package to environment  
library(readme, lib.loc = "./")

#For further instructions on use, see ?readme and ?undergrad, as well as readme.pdf

