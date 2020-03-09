setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(rjson);library(pbmcapply)

# Load in dataset from step 5
load("df5_articledata_matchednames.RData")

# Save number of cores on machine
cores=detectCores()

# If you have already started this process and saved an interim file, read it
# If not, create a new file to store names and gender probabilities
# Note: This process takes a while, so you might have to pause and pick
# it back up every once in a while
if("df6_namegends.RData"%in%list.files()){
  load("df6_namegends.RData")
}else{
  all_auth_names=lapply(as.list(article.data$AF),strsplit,split="; ")
  first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                         authlist=all_auth_names,mc.cores=cores)
  
  # Isolate first- and last-authors' first names
  first_last_auths=pbmclapply(first_names,get.first.last,mc.cores=cores)
  
  # Get unique first names for gender estimation
  name_list=unique(unlist(first_last_auths))
  initials=unlist(lapply(name_list,is.initials))
  
  # Create dataset for names and predicted genders
  namegends=data.frame(name=name_list,
                       prob.m=rep(NA,length(name_list)),
                       prob.w=rep(NA,length(name_list)))
  
  # Make name variable chr type
  namegends$name=as.character(namegends$name)
  
  # Insert "-1" for initials, so you don't waste gender credits on them
  namegends$prob.m[initials==T]=-1
  namegends$prob.w[initials==T]=-1
  
  # Fill in some data using pre-built common names database
  commonnames=read.csv("CommonNamesDatabase.csv",stringsAsFactors=F)[,-1]
  names_in_common=which(namegends$name%in%commonnames$name)
  in_common_data=pbmclapply(names_in_common,match.common,
                            namegends,commonnames,mc.cores=cores)
  namegends[names_in_common,]=do.call(rbind,in_common_data)
  
  # Save this new dataset to be filled in as you go
  save(namegends,file="df6_namegends.RData")
}

# Enter your API key here for gender-api.com
# Run 'sum(is.na(namegends$prob.m))' to see how many credits you'll need
gender_api_key="example_key"

# Determine which names have yet to be queried from gender-api
r=which(is.na(namegends$prob.m))

# For the remaining unqueried names...
for(i in r){
  
  # Isolate name to be queried
  this_name=namegends$name[i]
  
  # Pull json data from gender-api
  json_file=paste0("https://gender-api.com/get?name=",this_name,
                   "&key=",gender_api_key)
  json_data=fromJSON(file=json_file)
  
  # Save gender probabilities to the namegends dataset
  # Enter -1 if gender-api doesn't have any data for that name
  if(json_data$gender=="male"){
    namegends$prob.m[i]=json_data$accuracy/100
    namegends$prob.w[i]=1-json_data$accuracy/100
  }else if(json_data$gender=="female"){
    namegends$prob.w[i]=json_data$accuracy/100
    namegends$prob.m[i]=1-json_data$accuracy/100
  }else{
    namegends$prob.w[i]=-1
    namegends$prob.m[i]=-1
  }
  
  # Output the name and associated gender probability (optional)
  print(paste0(this_name,"=",json_data$gender,
               ": confidence=",json_data$accuracy))
  
  # Save the interim file so you can pick back up later if you decide to stop
  save(namegends,file="df6_namegends.RData")
  
  # Pause to space out pull requests
  time=round(runif(1,1,3),0)
  for(t in time:1){
    Sys.sleep(1)
    cat("Countdown:",t,"\n")
  }
}



