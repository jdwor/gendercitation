## New functions for Step1_CleanWOSFiles.R
get.date=function(x,pd){
  pd=pd[x]
  pd=tolower(pd)
  if(grepl("jan",pd)){
    pd=1
  }else if(grepl("feb",pd)){
    pd=2
  }else if(grepl("mar",pd)){
    pd=3
  }else if(grepl("apr",pd)){
    pd=4
  }else if(grepl("may",pd)){
    pd=5
  }else if(grepl("jun",pd)){
    pd=6
  }else if(grepl("jul",pd)){
    pd=7
  }else if(grepl("aug",pd)){
    pd=8
  }else if(grepl("sep",pd)){
    pd=9
  }else if(grepl("oct",pd)){
    pd=10
  }else if(grepl("nov",pd)){
    pd=11
  }else if(grepl("dec",pd)){
    pd=12
  }else{
    pd=NA
  }
  return(pd)
}

## New functions for Step2_LocateFullNames.R
get.given=function(x){
  if(grepl(", ",x)){
    name=strsplit(x,split=", ")[[1]]
    return(ifelse(length(name)>1,name[2],""))
  }else if(grepl(" ",x)){
    name=strsplit(x,split=" ")[[1]]
    len=length(name)
    return(paste(name[-len],collapse=" "))
  }else{
    return("")
  }
}
get.all.given=function(x,authlist){
  sub=authlist[[x]][[1]]
  fnames=unlist(lapply(sub,get.given))
  return(fnames)
}
is.initials=function(x){
  return(identical(x,toupper(x)))
}
get.cr.sep=function(x){
  name=paste0(x$family,", ",x$given)
  return(name)
}
get.cr.first=function(x){
  return(x$given)
}
get.cr.auths=function(json_author){
  firsts=unlist(lapply(json_author,get.cr.first))
  Encoding(firsts)="latin1"
  firsts=replace_non_ascii(firsts)
  
  names_sep=unlist(lapply(json_author,get.cr.sep))
  Encoding(names_sep)="latin1"
  names_sep=replace_non_ascii(names_sep)
  
  names_togeth=paste(names_sep,collapse="; ")
  names_togeth=replace_non_ascii(names_togeth)
  
  return(list(firsts=firsts,all=names_togeth))
}
get.jneuro.auths=function(json_url){
  html=html_session(json_url)
  html=read_html(html$url)
  auths=html %>% html_nodes("meta[name='DC.Contributor']")
  auths=auths %>% html_attr("content")
  auths=unlist(lapply(auths,fix.jneuro.names))
  auths=paste(auths,collapse="; ")
  auths=replace_non_ascii(auths)
}
fix.jneuro.names=function(x){
  tauth=strsplit(x," ")[[1]]
  inlast=c(which(tauth==tolower(tauth)),length(tauth))
  inlast=c(inlast,which(tolower(tauth)%in%c("van","der","de","di","von")))
  inlast=sort(unique(inlast),decreasing=F)
  infirst=c(1:length(tauth))[!c(1:length(tauth))%in%inlast]
  tauth=paste0(paste(tauth[inlast],collapse=" "),
               ", ",paste(tauth[infirst],collapse=" "))
}

## New functions for Step4_CleanNameStructure.R
split.auths=function(x){
  strsplit(x,split="; ")[[1]]
}
rm.extra.comma=function(x){
  auths=split.auths(x)
  ncoms=unlist(lapply(auths,str_count, ", "))
  extra=which(ncoms>1)
  for(i in extra){
    newauth=strsplit(auths[extra],", ")[[1]]
    newauth=paste0(newauth[1],", ",paste(newauth[-1],collapse=" "))
    auths[extra]=newauth
  }
  return(paste(auths,collapse="; "))
}
add.miss.comma=function(x,authlist,dois){
  auths=authlist[x]
  doi=dois[x]
  auths=split.auths(auths)
  ncoms=unlist(lapply(auths,str_count, ", "))
  missing=which(ncoms<1)
  for(i in missing){
    name=auths[i]
    json_file=paste0("https://api.crossref.org/v1/works/http://dx.doi.org/",doi)
    json_data=try(RJSONIO::fromJSON(json_file),silent=T)
    
    if(class(json_data)!="try-error"){
      if(!is.null(json_data$message$author)){
        json_author=json_data$message$author
        names_sep=unlist(lapply(json_author,get.cr.sep))
        Encoding(names_sep)="latin1"
        names_sep=replace_non_ascii(names_sep)

        if(length(names_sep)==length(auths)){
          auths[i]=names_sep[i]
        }else{
          name=strsplit(name,split=" ")[[1]]
          len=length(name)
          auths[missing]=paste0(name[len],", ",paste(name[-len],collapse=" "))
        }
      }else{
        name=strsplit(name,split=" ")[[1]]
        len=length(name)
        auths[missing]=paste0(name[len],", ",paste(name[-len],collapse=" "))
      }
    }else{
      name=strsplit(name,split=" ")[[1]]
      len=length(name)
      auths[missing]=paste0(name[len],", ",paste(name[-len],collapse=" "))
    }
  }
  return(paste(auths,collapse="; "))
}

## New functions for Step5_MatchNames.R
get.family=function(x){
  if(grepl(", ",x)){
    name=strsplit(x,split=", ")[[1]]
    return(name[1])
  }else if(grepl(" ",x)){
    name=strsplit(x,split=" ")[[1]]
    len=length(name)
    return(name[len])
  }else{
    return("")
  }
}
get.all.family=function(x,authlist){
  sub=authlist[[x]][[1]]
  lnames=unlist(lapply(sub,get.family))
  return(lnames)
}
get.preferred=function(x){
  if(grepl("\\.",x)==TRUE & x!=toupper(x)){
    name=gsub("\\."," ",x)
    name=strsplit(name," ")[[1]]
    notup=which(name!=toupper(name))
    name=name[notup][1]
    if(substr(name,1,1)!="-"){
      return(name)
    }else{
      return(substr(name,2,nchar(name)))
    }
  }else if(grepl(" ",x)==TRUE & x!=toupper(x)){
    name=strsplit(x," ")[[1]]
    notup=which(name!=toupper(name))
    name=name[notup][1]
    if(substr(name,1,1)!="-"){
      return(name)
    }else{
      return(substr(name,2,nchar(name)))
    }
  }else if(grepl("-",substr(x,1,2))){
    if(substr(x,1,1)=="-"){
      return(substr(x,2,nchar(x)))
    }else if(substr(x,2,2)=="-" & 
             substr(x,1,1)!="A" &
             substr(x,1,1)!="I"){
      return(substr(x,3,nchar(x)))
    }else{
      return(x)
    }
  }else{
    return(x)
  }
}

