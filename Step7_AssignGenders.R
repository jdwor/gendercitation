setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(rjson);library(pbmcapply)

# Load in article dataset from step 5
load("df5_articledata_matchednames.RData")

# Load in gender dataset from step 6
load("df6_namegends.RData")


article_auth_gends=pbmclapply(first_last_auths,gend.to.auths,
                              namegends,threshold=0.7)

article.data$AG=unlist(article_auth_gends)