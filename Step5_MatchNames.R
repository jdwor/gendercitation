setwd("path/to/project/folder")
source("HelperFunctions.R")
library(pbmcapply);library(stringr)
library(textclean)

load("df4_articledata_cleannames.RData")
nicknames=as.matrix(read.csv("nicknames.csv",header=F))
nicknames=tolower(nicknames)

cores=detectCores()

all_auth_names=lapply(as.list(data.frame$AF),strsplit,split="; ")
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=cores)
last_names=pbmclapply(1:length(all_auth_names),get.all.family,
                       authlist=all_auth_names,mc.cores=cores)
initials=unlist(lapply(first_names,is.initials))


