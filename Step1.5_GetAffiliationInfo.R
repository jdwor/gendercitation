setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(bibliometrix);library(pbmcapply)
library(countrycode);library(dplyr)

# Change to the names of your journal folders within project folder
journal_folders=c("journal1","journal2","journal3")

# Create and fill data.frame of affiliation information from WoS files
affil.data=NULL
for(i in journal_folders){
  # For each journal, find all data files within folder
  files=list.files(i)
  data.frame=NULL
  
  for(j in files){
    # For each file, read data in, convert to data frame, and concatenate
    this.data.frame=readFiles(paste0(i,"/",j))
    this.data.frame=createdf(this.data.frame)
    
    if(!is.null(data.frame)){
      data.frame=merge(data.frame,this.data.frame,all=T,sort=F)
    }else{
      data.frame=this.data.frame
    }
  }
  
  # Select relevant variables
  # AF=authors, DI=doi, C1=affiliations, RP=corresponding info, 
  # DT=document type, PY=year
  data.frame=data.frame %>% select(AF, DI, C1, RP, DT, PY)
  
  # Optional: subset data by date if you did so in the full data
  # e.g., data.frame=data.frame[data.frame$PY>=1995,]
  # e.g., data.frame=data.frame[data.frame$DT%in%c("Article","Review"),]
 
  # Append this new data to the full dataset
  affil.data=rbind(affil.data,data.frame)
  rm(data.frame, this.data.frame, files)
}
# Clean up environment
rm(i, j, journal_folders)

# Clean up capitalization and punctuation
affil.data$AF=tolower(affil.data$AF)
affil.data$C1=tolower(affil.data$C1) 
affil.data$RP=tolower(affil.data$RP)
affil.data$C1=gsub("\\.","",affil.data$C1)
affil.data$RP=gsub("\\.","",affil.data$RP)

# Save number of desired cores for parallelization
cores=detectCores()

# Separate out author names and save first/last authors' family names
all_auth_names=lapply(as.list(affil.data$AF),strsplit,split="; ")
last_names=pbmclapply(1:length(all_auth_names),get.all.family,
                      authlist=all_auth_names,mc.cores=cores)
fas=unlist(lapply(last_names,head,1))
las=unlist(lapply(last_names,tail,1))

# Locate country names within affiliation data. Returns the following:
# 1) country named in corresponding author affiliation,
# 2-3) countries linked with first and last author affiliations,
# 4) all countries listed, and 5) first country listed in affiliations
countries=pbmclapply(1:nrow(affil.data),getCountries,
                     affil.data,fas,las,mc.cores=cores)
countries=do.call(rbind,countries)
countries=data.frame(countries, stringsAsFactors=F)
colnames(countries)=c("Corresponding","FirstAuthor","LastAuthor",
                      "AllListed","FirstListed")

# Check output manually
head(countries)
head(sort(table(countries$Corresponding),decreasing=T),10)

# Pull out all unique countries listed within affiliations
all.countries=paste(countries$AllListed,collapse=', ')
all.countries=strsplit(all.countries,", ")[[1]]
unique.countries=unique(all.countries)

# Create dataset that links countries to continents/regions
# and fill in regions for countries not matched in countrycode
country.to.cont=countrycode(sourcevar = unique.countries,
                            origin = "country.name",
                            destination = "continent")
country.to.cont=cbind(unique.countries,country.to.cont)
country.to.cont[country.to.cont[,1]=="england",2]="Europe"
country.to.cont[country.to.cont[,1]=="scotland",2]="Europe"
country.to.cont[country.to.cont[,1]=="wales",2]="Europe"
country.to.cont[country.to.cont[,1]=="africa",2]="Africa"
country.to.cont[country.to.cont[,1]=="yugoslavia",2]="Europe"
country.to.cont[country.to.cont[,1]=="antilles",2]="Americas"

# Check output manually
head(country.to.cont)

# Match countries to corresponding continents/regions. Returns the following:
# 1) region of corresponding author affiliation,
# 2-3) regions linked with first and last author affiliations,
# 4) all regions listed, and 5) first region in affiliations
continents=pbmclapply(1:nrow(countries),getContinents,
                      countries,country.to.cont,mc.cores=cores)
continents=do.call(rbind,continents)
continents=data.frame(continents, stringsAsFactors=F)
colnames(continents)=c("Corresponding","FirstAuthor","LastAuthor",
                       "AllListed","FirstListed")

# Check output manually
head(continents)

# Save affiliation datasets
save(affil.data,countries,continents,file="df1.5_affiliationdata.RData")

