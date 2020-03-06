setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(pbmcapply)

# Load in data from step 9
load("df9_articledata_propdata.RData")

# Save number of cores on machine
cores=detectCores()

# Isolate author information
all_auth_names=lapply(article.data$AF,authsplit)
first_auths=unlist(lapply(all_auth_names,head,1))
last_auths=unlist(lapply(all_auth_names,tail,1))

# Get genders for individual authors
first_gend=unlist(lapply(article.data$AG,substr,1,1))
last_gend=unlist(lapply(article.data$AG,substr,2,2))
author_gends=data.frame(name=c(first_auths,last_auths),
                        gend=c(first_gend,last_gend))
author_gends=unique(author_gends)

# Find previous co-authors for each article in dataset
# NOTE: This function takes some power and some time (best on a cluster)
month_from_base=article.data$MB
prev_coauths=pbmclapply(1:length(all_auth_names),get.prev.coauths,first_auths,
                        last_auths,all_auth_names,month_from_base,mc.cores=cores)

# Calculate local man-author overrepresentation in each article's network
# NOTE: This function takes some power and some time (best on a cluster)
ma_overrep=pbmclapply(1:length(all_auth_names),get.ma.overrep,prev_coauths,
                      all_auth_names,month_from_base,author_gends,mc.cores=cores)
ma_overrep=unlist(ma_overrep)

# Calculate local man/man-paper overrepresentation in each article's network
# NOTE: This function takes some power and some time (best on a cluster)
article_gends=article.data$AG
mmp_overrep=pbmclapply(1:length(all_auth_names),get.mmp.overrep,prev_coauths,
                       all_auth_names,month_from_base,article_gends,mc.cores=cores)
mmp_overrep=unlist(mmp_overrep)

# Save article data, citation proportion data, and network data
save(article.data,uncond_expecs,cond_expecs,ref_proportions,
     ma_overrep,mmp_overrep,file="df10_articledata_propdata_netdata.RData")
