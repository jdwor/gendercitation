setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(pbmcapply);library(mgcv)

# Load in article dataset from step 7
load("df7_articledata_withgenders.RData")

# Clean up reference list column
Encoding(article.data$CR)="latin1"
article.data$CR=replace_non_ascii(article.data$CR)
article.data$CR=tolower(article.data$CR)

# Get indices of cited papers within reference lists
cited.indices=pbmclapply(1:nrow(article.data),get.cited.indices,
                         DI=article.data$DI,CR=article.data$CR,
                         mc.cores=cores)

# Create new variable in article.data that gives list of cited papers
article.data$CI=unlist(cited.indices)



