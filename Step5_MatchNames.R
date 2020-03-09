setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(tidyverse);library(parallel)
library(pbmcapply)

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
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=cores)
last_names=pbmclapply(1:length(all_auth_names),get.all.family,
                       authlist=all_auth_names,mc.cores=cores)
initials=unlist(lapply(first_names,is.initials))

# Save vectors of all first names and all last names
allfirsts=as.vector(unlist(first_names))
alllasts=as.vector(unlist(last_names))

# Match names with only initials to similar full names
# E.g., DS Bassett --> Danielle S Bassett
# Inspect the 'match.initials' function for more detail
# NOTE: This function takes some power and some time (best on a cluster)
first_names=pbmclapply(1:length(first_names),match.initials,first_names,
                       last_names,allfirsts,alllasts,mc.cores=cores)
allfirsts=as.vector(unlist(first_names))

# Find last names that repeat
lastname_occurrences=table(alllasts)
multiple_occurrences=lastname_occurrences[lastname_occurrences>1]

# Determine whether last names have potential variants on the same first name
# I.e., Raymond Dolan and Ray Dolan (vs. Raymond Dolan and Emily Dolan)
may_have_variants=unlist(pbmclapply(names(multiple_occurrences),find.variants,
                         allfirsts,alllasts,mc.cores=cores))
may_have_variants=names(multiple_occurrences[may_have_variants==1])

# Detect name variants and assign all instances to the most detailed version
# E.g., Ray Dolan, Raymond Dolan, & Ray J Dolan --> Raymond J Dolan
# Inspect 'match.variants.inner' for more detail
# NOTE: This function takes some power and some time (best on a cluster)
fn_matched=pbmclapply(1:length(first_names),match.variants.outer,
                      first_names,last_names,allfirsts,alllasts,
                      may_have_variants,nickname.gends,mc.cores=cores)
allfirsts_matched=as.vector(unlist(fn_matched))

# Test out whether it worked as expected by comparing variants before/after
# Can substitute Dolan for any name in your dataset that had variants
sort(unique(allfirsts[alllasts=="Dolan"]))
sort(unique(allfirst_matched[alllasts=="Dolan"]))

# Create new first names list, and concatenate first and last names
first_names=fn_matched; rm(fn_matched)
fullnames_matched=pbmclapply(1:length(first_names),paste.first.last,
                             first_names,last_names,mc.cores=cores)

# Replace article.data names with new matched names
article.data$AF=unlist(fullnames_matched)

# Save matched dataset
save(article.data, file="df5_articledata_matchednames.RData")

