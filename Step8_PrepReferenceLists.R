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
cited.papers=pbmclapply(1:nrow(article.data),get.cited.indices,
                        DI=article.data$DI,CR=article.data$CR,
                        mc.cores=cores)

# Find potential self-citations
self.authored=pbmclapply(1:length(first_auths),get.self.cites,
                         first_auths,last_auths,mc.cores=cores)

# Create new variables in article.data that give a list of cited papers (CP)
# and a list of other self-authored papers in the dataset (SA)
article.data$CP=unlist(cited.papers)
article.data$SA=unlist(self.authored)

# Isolate author information
all_auth_names=lapply(article.data$AF,authsplit)

# Get variables for article gender model
num_papers=unlist(lapply(self.authored,str_count, ", "))+1
log_seniority=log(num_papers)
review=article.data$DT=="Review"
log_teamsize=log(lengths(all_auth_names))
journal=article.data$SO
month_from_base=(article.data$PY-min(article.data$PY))+(article.data$PD/12)
gender_cat=unlist(pbmclapply(article.data$AG,transform.cat))

# Run GAM for predicted gender given relevant article characteristics



