setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(pbmcapply);library(rvest)
library(RJSONIO);library(textclean)

# Change to the names of your journal folders within project folder
journal_folders=c("journal1","journal2","journal3")

# Select journal -- Will need to go back and repeat for each journal!
this_journal=journal_folders[1]
load(paste0(this_journal,"_df1_webofscience.RData"))

# Separate out individual authors for each article
all_auth_names=lapply(as.list(data.frame$AF),strsplit,split="; ")

# Get first names of each author based on comma location
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=2)

# Find whether each first name only contains initials
initials=unlist(lapply(first_names,is.initials))

# Determine which articles only have initial information
needed_dois=data.frame$DI[initials==T]
needed_names=all_auth_names[initials==T]

# Prep urls for crossref pull requests
base_url="https://api.crossref.org/v1/works/http://dx.doi.org/"
urls=paste0(base_url,needed_dois)

# If you have already started this process and saved an interim file, read it
# If not, create a new file to store supplemented names from crossref
# Note: This process takes a while, so you might have to pause and pick
# it back up every once in a while
if(paste0(this_journal,"_df2_missingnames.RData")%in%list.files()){
  load(paste0(this_journal,"_df2_missingnames.RData"))
}else{
  new.names=data.frame(DI=needed_dois,
                       AF=data.frame$AF[initials==T],
                       done=rep(0,length(needed_dois)))
  new.names$AF=as.character(new.names$AF)
  new.names$DI=as.character(new.names$DI)
}

# Determine which articles' missing names have yet to be pulled from crossref
still.to.do=which(new.names$done==0)
for(i in still.to.do){
  
  # For each article, get original names and total number of authors
  orig_author_names=new.names$AF[i]
  num_authors=length(needed_names[[i]][[1]])
  
  # Pull data from crossref for article i
  json_file=urls[i]
  json_data=try(RJSONIO::fromJSON(json_file),silent=T)
  
  # If the pull request works...
  if(class(json_data)!="try-error"){
    
    # And if there is actually author data...
    if(!is.null(json_data$message$author)){
      
      # Get their names from the resulting data pull
      crossref=get.cr.auths(json_data$message$author)

      # If crossref has data for all authors 
      # (sometimes they only have the first author for some reason),
      # and if they are full names and not just initials...
      if(length(crossref$firsts)==num_authors & 
         !identical(crossref$firsts,toupper(crossref$firsts))){
           
        # Replace the WoS names with the crossref names in new data frame
        new.names$AF[i]=crossref$all
        print("changed by crossref")
        
      }else{
        ### INSERT CUSTOMIZED CODE HERE FROM PULLING JOURNAL WEBSITE DATA
        ### (if desired, otherwise live with data from WoS and crossref)
        
        # Inspect 'get.jneuro.auths' for an example journal-scraping function
        # Once function is customized, could be used as follows:
        
        # journal_names=get.journal.auths(json_data$message$URL)
        # new.names$AF[i]=journal_names
        # print("tried journal website")
      }
      
      # Output comparison of original WoS names and new names (if desired)
      print(orig_author_names)
      print(new.names$AF[i])
    }else{
      
      # If crossref didn't have author data, say that
      print("Couldn't find authors")
    }
  }else{
    
    # If crossref couldn't find the relevant DOI, say that
    print("Couldn't find DOI")
  }
  
  # Make note that you completed the pull for this article
  new.names$done[i]=1
  cat(i,"of",nrow(new.names),"\n")
  
  # Save interim file with updated data
  save(new.names,file=paste0(this_journal,"_df2_missingnames.RData"))
  
  # Pause to space out pull requests
  time=round(runif(1,1,3),0)
  for(t in time:1){
    Sys.sleep(1)
    cat("Countdown:",t,"\n")
  }
}
