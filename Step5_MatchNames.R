setwd("path/to/project/folder")
source("HelperFunctions.R")
library(tidyverse);library(parallel)
library(pbmcapply)

load("df4_articledata_cleannames.RData")

nicknames=as.matrix(read.csv("nicknames.csv",header=F))
namegends=read.csv("prelim.gends.for.matching.csv",
                   header=T,stringsAsFactors=F)[,-1]
nicknames=tolower(nicknames)

cores=detectCores()

all_auth_names=lapply(as.list(data.frame$AF),strsplit,split="; ")
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=cores)
last_names=pbmclapply(1:length(all_auth_names),get.all.family,
                       authlist=all_auth_names,mc.cores=cores)
initials=unlist(lapply(first_names,is.initials))

allfirsts=as.vector(unlist(first_names))
alllasts=as.vector(unlist(last_names))

first_names=pbmclapply(1:length(first_names),match.initials,first_names,
                       last_names,allfirsts,alllasts,mc.cores=cores)
allfirsts=as.vector(unlist(first_names))

lastname_occurrences=table(alllasts)
multiple_occurrences=lastname_occurrences[lastname_occurrences>1]

may_have_variants=pbmclapply(names(multiple_occurrences),find.variants,
                             allfirsts,alllasts,mc.cores=cores)
may_have_variants=unlist(may_have_variants)

may_have_variants=names(multiple_occurrences[may_have_variants==1])



