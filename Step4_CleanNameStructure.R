setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(pbmcapply);library(stringr)
library(RJSONIO);library(textclean)

# Load in all-journal dataset from step 3
load("df3_articledata.RData")

# Find number of authors in each article
num.auths=unlist(lapply(article.data$AF,str_count, "; "))+1
# Find number of first-/last-name delineations in each author list
num.commas=unlist(lapply(article.data$AF,str_count, ", "))

# Find articles where at least one author did not have delineated first-/last-name
missing.comma=which(num.auths>num.commas)
r=0; num.missing=length(missing.comma)

# For articles with missing delineation...
for(i in missing.comma){
  
  # Pull name data from crossref to get delineated first-/last-names
  delineated.names=add.miss.comma(i,article.data$AF,article.data$DI)
  
  # Enter the newly formatted names into dataset
  # Note: gsub call just adds a space (" ") for names with no provided first name
  article.data$AF[i]=gsub(",;",", ;",delineated.names)
  
  # Iterate and provide progress update
  r=r+1; cat(r,"of",num.missing,"\n")
  
  # Pause to space out pull requests
  time=round(runif(1,1,3),0)
  for(t in time:1){
    Sys.sleep(1)
    cat("Countdown:",t,"\n")
  }
}

# Recalculate author counts and delineations
num.auths=unlist(lapply(article.data$AF,str_count, "; "))+1
num.commas=unlist(lapply(article.data$AF,str_count, ", "))

# Check whether any articles are still missing first/last name delineation
which(num.auths>num.commas)

# Now look for instances where there are more comma delineations than authors
# (this is usually because WoS styles "John Doe Jr." as "Doe, John, Jr.")
extra.comma=which(num.auths<num.commas)

# Remove those extra commas to get clean first-/last-name delineation
new.ec.names=unlist(lapply(article.data$AF[extra.comma],rm.extra.comma))
# Repalce instances in dataset
article.data$AF[extra.comma]=new.ec.names

# Recalculate author counts and delineations
num.auths=unlist(lapply(article.data$AF,str_count, "; "))+1
num.commas=unlist(lapply(article.data$AF,str_count, ", "))

# Check whether any articles still have an extra comma
which(num.auths!=num.commas)

# Save cleaned up dataset
save(article.data, file="df4_articledata_cleannames.RData")

