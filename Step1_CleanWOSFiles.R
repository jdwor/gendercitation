setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(bibliometrix)
library(rvest)
library(dplyr)

# Change to the names of your journal folders within project folder
journal_folders=c("journal1","journal2","journal3")

for(i in journal_folders){
  # For each journal, find all data files within folder
  files=list.files(i)
  
  data.frame=NULL
  for(j in files){
    # For each file, read data in, convert to data frame, and concatenate
    this.data.frame=readFiles(paste0(i,"/",j))
    this.data.frame=convert2df(this.data.frame,
                               dbsource="wos",format="plaintext")
    data.frame=rbind(data.frame,this.data.frame)
  }
  
  # Find article entries that don't have DOI but do have PubMed ID
  without.DOI=which((data.frame$DI=="" | is.na(data.frame$DI)) & 
                      !is.na(data.frame$PM))
  if(length(without.DOI)>0){
    
    # For articles with PubMed ID but no DOI
    for(j in without.DOI){
      
      # Find relevant DOI from PMC website and enter into data frame
      this.pubmed=data.frame$PM[j]
      turl=paste0("https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=",
                  this.pubmed)
      xml=read_xml(turl,as_html=T)
      doi=xml %>% html_nodes("record") %>% html_attr("doi")
      if(nchar(doi)>0){
        data.frame$DI[j]=doi
      }
      
      # Pause to space out pull requests
      Sys.sleep(2)
    }
  }
  
  # Select relevant variables
  # AF=authors, SO=journal, DT=article type, CR=reference list
  # TC=total citation, PD=month/day, PY=year, DI=DOI
  data.frame=data.frame %>% 
    select(AF, SO, DT, CR, TC, PD, PY, DI)
  
  # Translate month/day to numeric month
  data.frame$PD=unlist(lapply(1:nrow(data.frame),get.date,pd=data.frame$PD))
  data.frame$PD=as.numeric(data.frame$PD)
  data.frame=data.frame[data.frame$PD%in%c(1:12),]
  
  # Subset to only articles (i.e., remove editorial content etc.)
  data.frame=data.frame[data.frame$DT%in%c("Article","Review"),]
  
  # Standardize dois and reference lists to lowercase
  data.frame$DI=tolower(data.frame$DI)
  data.frame$CR=tolower(data.frame$CR)
  
  # Optional: subset data by date
  #data.frame=data.frame[data.frame$PY>=1995,]
  
  # Optional (would need to change code to be journal specific):
  # Locate DOIs that may have typos (WoS sometimes messes up DOIs)
  #weirdindices=which(data.frame$DI!="" & !grepl("10.1038",data.frame$DI))
  #weirddois=data.frame$DI[weirdindices]
  
  # Save new data frame of this journal's complete data
  save(data.frame,file=paste0(i,"_df1_webofscience.RData"))
}





