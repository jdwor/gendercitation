setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")

# Change to the names of your journal folders within project folder
journal_folders=c("journal1","journal2","journal3")

# Create empty data frame for all-journal data
article.data=NULL

# For each journal...
for(i in journal_folders){
  
  # Load in original WoS data frame from step 1...
  load(paste0(i,"_df1_webofscience.RData"))
  # and new crossref names from step 2
  load(paste0(i,"_df2_missingnames.RData"))
  
  # Separate out author names and find entries with initials
  all_auth_names=lapply(as.list(data.frame$AF),strsplit,split="; ")
  first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                         authlist=all_auth_names,mc.cores=2)
  initials=unlist(lapply(first_names,is.initials))
  
  # Replace entries with initials with the new names you got from crossref
  data.frame$AF[initials==T]=new.names$AF
  
  # Append this new data to the full dataset
  article.data=rbind(article.data,data.frame)
}

# Save out full dataset with info from all journals
save(article.data, file="df3_articledata.RData")

