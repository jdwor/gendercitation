setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(tidyverse);library(parallel)
library(pbmcapply);library(plyr)
library(rlist)

# Load in all-journal dataset from step 4
load("df4_articledata_cleannames.RData")

# Read in dataset of common nicknames for variant matching
# E.g., to match Ray Dolan to Raymond Dolan
nicknames=as.matrix(read.csv("nicknames.csv",header=F))
nicknames=tolower(nicknames)

# Read in dataset of likely genders for nicknames
# Used to avoid matching e.g. Chris Smith & Christina Smith
nickname.gends=read.csv("nickname.gends.csv",header=T,stringsAsFactors=F)[,-1]

# Save number of cores on machine
# Note: Some of the functions in this step take a fair bit of compute power
# This file should ideally be performed by parallelizing over many cores
cores=detectCores()

# Separate out author names and find entries with initials
all_auth_names=lapply(as.list(article.data$AF),strsplit,split="; ")
unique_names=unique(unlist(all_auth_names))
allfirsts=unlist(pbmclapply(1:length(unique_names),get.all.given,
                            authlist=unique_names,mc.cores=cores))
alllasts=unlist(pbmclapply(1:length(unique_names),get.all.family,
                           authlist=unique_names,mc.cores=cores))
initials=unlist(lapply(allfirsts,is.initials))

# Match names with only initials to similar full names
# E.g., DS Bassett --> Danielle S Bassett
# Inspect the 'match.initials' function for more detail
# NOTE: This function may take some time (best on multiple cores)
newfirsts=unlist(pbmclapply(which(initials==T),match.initials,allfirsts,
                            alllasts,initials,mc.cores=cores))
allfirsts[initials==T]=newfirsts

# Find last names that repeat
lastname_occurrences=table(alllasts)
multiple_occurrences=names(lastname_occurrences[lastname_occurrences>1])

# Determine whether last names have potential variants on the same first name
# I.e., Raymond Dolan and Ray Dolan (vs. Raymond Dolan and Emily Dolan)
may_have_variants=do.call(rbind,pbmclapply(multiple_occurrences,find.variants,
                                           allfirsts,alllasts,mc.cores=cores))
may_have_variants=may_have_variants[may_have_variants[,1]==T,]

# Detect name variants and assign all instances to the most detailed version
# E.g., Ray Dolan, Raymond Dolan, & Ray J Dolan --> Raymond J Dolan
# Inspect 'match.variants.inner' for more detail
# NOTE: This function may take some time (best on multiple cores)
fn_matched=pbmclapply(which(alllasts%in%may_have_variants[,2]),
                      match.variants.outer,allfirsts,alllasts,
                      may_have_variants,nickname.gends,mc.cores=cores)
allfirsts_matched=allfirsts
allfirsts_matched[alllasts%in%may_have_variants[,2]]=unlist(fn_matched)

# Test out whether it worked as expected by comparing variants before/after
# Can substitute Dolan for any name in your dataset that had variants
sort(allfirsts[alllasts=="Dolan"])
sort(allfirsts_matched[alllasts=="Dolan"])

# Concatenate first and last names
unique_names_matched=paste0(alllasts,", ",allfirsts_matched)
updated=which(unique_names!=unique_names_matched)
to_change=cbind(unique_names,unique_names_matched)[updated,]

# Replace article.data names with matched names
auth_vec=unlist(all_auth_names)
auth_vec=mapvalues(auth_vec, from=to_change[,1], to=to_change[,2])
all_auth_names=list.flatten(relist(auth_vec, skeleton=all_auth_names))
article.data$AF=unlist(lapply(all_auth_names,paste,collapse="; "))

# Save matched dataset
save(article.data, file="df5_articledata_matchednames.RData")

