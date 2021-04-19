## New functions for Step1_CleanWOSFiles.R
readFiles=function(...){
  
  
  
  arguments <- unlist(list(...))
  
  k=length(arguments)
  
  D=list()
  
  enc="UTF-8"
  
  origEnc=getOption("encoding")
  
  if (origEnc=="UTF-8"){options(encoding = "native.enc")}
  
  for (i in 1:k){
    
    D[[i]]=suppressWarnings(
      
      iconv(readLines(arguments[i],encoding = "UTF-8"),"latin1", "ASCII", sub="")
      
      #conv(readLines(arguments[[i]]))
      
    )
    
  }
  
  D=unlist(D)
  
  options(encoding = origEnc)
  
  Encoding(D) <- "UTF-8"
  
  return(D)
  
  
  
}
createdf.internal=function(D){
  Papers = which(regexpr("PT ", D) == 1)
  nP = length(Papers)
  Tag = which(regexpr("  ", D) == -1)
  lt = length(Tag)
  st1 = seq(1, (lt - 1))
  uniqueTag = unique(substr(D[Tag[st1]], 1, 2))
  uniqueTag = uniqueTag[nchar(uniqueTag) == 2]
  uniqueTag = uniqueTag[uniqueTag != "FN" & uniqueTag != "VR"]
  DATA = data.frame(matrix(NA, nP, length(uniqueTag)))
  names(DATA) = uniqueTag
  specialSep = c("AU", "AF", "CR", "C1", "RP")
  for (i in 1:nP) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shiny::incProgress(1/nP)
    }
    if (i%%100 == 0 | i == nP) 
      cat("Articles extracted  ", i, "\n")
    iStart = Papers[i]
    if (i == nP) {
      iStop = length(D)
    }
    else {
      iStop = Papers[i + 1] - 1
    }
    Seq = seq(iStart, iStop)
    pTag = iStart + which(regexpr("  ", D[Seq]) == 1) - 1
    for (j in uniqueTag) {
      if (j %in% specialSep) {
        sep = ";"
      }
      else {
        sep = " "
      }
      indTag = iStart + which(regexpr(j, D[Seq]) == 1) - 
        1
      if (length(indTag) > 0) {
        it = 0
        repeat {
          if (sum(pTag > (indTag + it)) == 0) {
            break
          }
          if (pTag[pTag > indTag + it][1] - (indTag + 
                                             it) == 1) {
            it = it + 1
          }
          else {
            break
          }
        }
        DATA[[j]][i] = paste(D[indTag:(indTag + it)], 
                             collapse = sep)
        DATA[[j]][i] = substr(DATA[[j]][i], 4, nchar(DATA[[j]][i]))
      }
      else {
        DATA[[j]][i] = NA
      }
    }
  }
  DATA <- data.frame(lapply(DATA, function(x) {
    x = gsub("\\s+", " ", x)
  }), stringsAsFactors = FALSE)
  DATA$UT = gsub("WOS:", "ISI", DATA$UT)
  if ("PY" %in% names(DATA)) {
    DATA$PY = as.numeric(DATA$PY)
  }
  DATA$DB = "ISI"
  listAU = strsplit(DATA$AU, ";")
  listAU = lapply(listAU, function(l) {
    l = gsub(",", " ", l, fixed = TRUE)
    l = gsub(".", "", l, fixed = TRUE)
    l = gsub("\\s+", " ", l)
    l = trim(l)
    l = paste(l, collapse = ";")
  })
  DATA$AU = unlist(listAU)
  if (names(DATA)[1] != "PT") {
    DATA = DATA[, -(which(names(DATA) == "X.U.FEFF.F"))]
  }
  return(DATA)
}
createdf=function(file){
  cat("\nConverting your wos collection into a bibliographic dataframe\n\n")
  M <- createdf.internal(file)
  if ("PY" %in% names(M)) {
    M$PY = as.numeric(M$PY)
  }else {
    M$PY = NA
  }
  if ("TC" %in% names(M)) {
    M$TC = as.numeric(M$TC)
  }else {
    M$TC = NA
  }
  if (!("CR" %in% names(M))) {
    M$CR = "none"
  }
  M$AU = gsub(intToUtf8(8217), intToUtf8(39), M$AU)
  cat("Done!\n\n")
  return(M)
}
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

## New functions for Step1.5_GetAffiliationInfo.R
innerSplit=function(split){
  strsplit(split,c(" |\\,"))[[1]]
}
fixCountries=function(finals){
  finals[nchar(finals)==2]="usa"
  finals[suppressWarnings(!is.na(as.numeric(finals)))]="usa"
  finals[finals=="kong"]="hong kong"
  finals[finals=="zealand"]="new zealand"
  finals[finals=="republic"]="czech republic"
  finals[finals=="arabia"]="saudi arabia"
  finals[finals=="caledonia"]="new caledonia"
  finals[finals=="lanka"]="sri lanka"
  finals[finals=="rica"]="costa rica"
  finals[finals=="salvador"]="el salvador"
  finals[finals=="herceg"]="bosnia and herzegovina"
  finals[finals=="monteneg"]="montenegro"
  finals[finals=="nevi"]="st kitts & nevis"
  finals[finals=="nevis"]="st kitts & nevis"
  return(finals)
}
getCountries=function(x,affil.data,fas,las){
  t.corresp=affil.data$RP[x]
  if(!is.na(t.corresp)){
    fin.corr=tail(innerSplit(t.corresp),1)
    fin.corr=fixCountries(fin.corr)
  }else{
    fin.corr=NA
  }
  
  t.affil=affil.data$C1[x]
  fa=fas[x]
  la=las[x]
  
  if(is.na(t.affil)){
    split1=NULL
  }else if(grepl("\\[",t.affil)){
    split1=strsplit(t.affil,"; \\[")[[1]]
  }else{
    split1=strsplit(t.affil,"; ")[[1]]
  }
  if(length(split1)>0){
    split2=lapply(split1,innerSplit)
    finals=unlist(lapply(split2,tail,1))
    finals=fixCountries(finals)
    
    if(length(unique(finals))==1){
      only=unique(finals)
      return(c(fin.corr,only,only,only,only))
    }else{
      fa.ind=which(grepl(paste0(fa,", "),split1))
      fa.affils=ifelse(length(fa.ind)>0,
                       paste(unique(finals[fa.ind]),
                             collapse=", "),NA)
      la.ind=which(grepl(paste0(la,", "),split1))
      la.affils=ifelse(length(la.ind)>0,
                       paste(unique(finals[la.ind]),
                             collapse=", "),NA)
      all=paste(unique(finals),collapse=", ")
      first=finals[1]
      return(c(fin.corr,fa.affils,la.affils,all,first))
    }
  }else{
    return(c(fin.corr,NA,NA,NA,NA))
  }
}
getContinents=function(x,countries,country.to.cont){
  cdat=as.character(countries[x,])
  conts=rep(NA,length(cdat))
  for(i in 1:length(cdat)){
    if(!is.na(cdat[i])){
      tc=strsplit(cdat[i],", ")[[1]]
      wc=which(country.to.cont[,1]%in%tc)
      tconts=unique(country.to.cont[wc,2])
      conts[i]=paste(tconts,collapse=", ")
    }
  }
  return(conts)
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
  if(!is.null(x$given)){
    name=paste0(x$family,", ",x$given)
    return(name)
  }else if(is.null(x$given) & grepl(" ",x$family)){
    sep=strsplit(x$family," ")[[1]]
    last=tail(sep,1)
    first=paste(head(sep,length(sep)-1),collapse=" ")
    name=paste0(last,", ",first)
    return(name)
  }else{
    name=paste0(x$family,", ")
    return(name)
  }
}
get.cr.first=function(x){
  if(!is.null(x$given)){
    return(x$given)
  }else if(is.null(x$given) & grepl(" ",x$family)){
    sep=strsplit(x$family," ")[[1]]
    return(sep[1])
  }else{
    return("")
  }
}
get.no.name=function(x){
  return(is.null(x$family) & is.null(x$given))
}
get.cr.auths=function(json_author){
  no.name=unlist(lapply(json_author,get.no.name))
  json_author=json_author[!no.name]
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
extract.initials=function(name){
  name=gsub("[[:punct:][:blank:]]","",name)
  name=gsub("[:a-z:]","",name)
  return(name)
}
match.initials=function(x,allfirsts,alllasts,initials){
  if(initials[x]==F){
    return(target.first)
  }else{
    target.first=allfirsts[x]
    target.initials=extract.initials(target.first)
    target.last=alllasts[x]
    others=which(tolower(target.last)==tolower(alllasts) & 
                   initials==F)
    if(length(others)>0){
      allsimilar.full=NULL
      allsimilar.concat=NULL
      allsimilar.clean=NULL
      for(j in others){
        samelast.full=allfirsts[j]
        samelast.concat=tolower(gsub("[[:punct:][:blank:]]","",samelast.full))
        samelast.clean=get.preferred(samelast.full)
        samelast.initials=extract.initials(samelast.full)
        if(samelast.full!=toupper(samelast.full) & 
           (samelast.initials==target.initials)){
          allsimilar.full=c(allsimilar.full,samelast.full)
          allsimilar.concat=c(allsimilar.concat,samelast.concat)
          allsimilar.clean=c(allsimilar.clean,samelast.clean)
        }
      }
      unique.clean=unique(allsimilar.clean)
      name.lengths=nchar(unique.clean)
      longest=unique.clean[which.max(name.lengths)]
      others=unique.clean[-which.max(name.lengths)]
      contained=unlist(lapply(others,grepl,longest))
      if(length(unique(allsimilar.concat))==1 |
         length(unique(allsimilar.clean))==1){
        matched.name=sort(table(allsimilar.full),decreasing=T)
        matched.name=names(matched.name)[1]
        return(matched.name)
      }else if(length(contained)>0 & 
               sum(contained)==length(contained)){
        matched.name=sort(table(allsimilar.full),decreasing=T)
        matched.name=names(matched.name)[1]
        return(matched.name)
      }else{
        return(target.first)
      }
    }else{
      return(target.first)
    }
  }
}
find.variants=function(lastname,allfirsts,alllasts){
  samelasts=unique(allfirsts[alllasts==lastname])
  same.initials=substr(samelasts,1,1)
  tab=table(same.initials)
  if(max(tab)>1){
    return(c(T,lastname,paste(names(tab[tab>1]),collapse=", ")))
  }else{
    return(c(F,lastname,""))
  }
}
match.gend=function(name,namegends){
  which.name=which(namegends$name==name)
  if(length(which.name)==1){
    return(namegends$gend[which.name])
  }else{
    return(0.5)
  }
}
match.variants.inner=function(name,allfirsts,alllasts,nickname.gends){
  first=name[1]; last=name[2]
  samelast.full=unique(allfirsts[alllasts==last])
  samelast.clean=unlist(lapply(samelast.full,get.preferred))
  samelast.initials=extract.initials(samelast.full)
  samelast.gends=unlist(lapply(samelast.clean,match.gend,nickname.gends))
  
  name.index=which(samelast.full==first)
  
  this.full=samelast.full[name.index]
  samelast.full=samelast.full[-name.index]
  
  this.clean=samelast.clean[name.index]
  samelast.clean=samelast.clean[-name.index]
  
  this.initials=samelast.initials[name.index]
  samelast.initials=samelast.initials[-name.index]
  
  this.gend=samelast.gends[name.index]
  samelast.gends=samelast.gends[-name.index]
  
  this.nicknames=which(nicknames[,1]==tolower(this.clean))
  this.nicknames=unique(as.vector(nicknames[this.nicknames,-1]))
  this.nicknames=this.nicknames[this.nicknames!=""]
  
  matches=which((samelast.clean==this.clean | 
                   tolower(samelast.clean)%in%this.nicknames) & 
                grepl(this.initials,samelast.initials) & 
                  samelast.gends==this.gend)
  if(length(matches)==1){
    if(nchar(samelast.full[matches])>nchar(first)){
      return(samelast.full[matches])
    }else{
      return(first)
    }
  }else if(length(matches)>1){
    sl.full.matches=samelast.full[matches]
    sl.initials.matches=samelast.initials[matches]
    initial.variants=gsub(this.initials,"",sl.initials.matches)
    initial.variants=unique(initial.variants[initial.variants!=""])
    if(length(initial.variants)<=1 & max(nchar(sl.full.matches))>nchar(first)){
      return(sl.full.matches[which.max(nchar(sl.full.matches))])
    }else{
      return(first)
    }
  }else{
    return(first)
  }
}
match.variants.outer=function(x,allfirsts,alllasts,may_have_variants,
                              nickname.gends){
  first=allfirsts[x]
  last=alllasts[x]
  has_variants=sum(may_have_variants[,2]==last & 
                     grepl(substr(first,1,1),may_have_variants[,3]))
  if(has_variants>0){
    matched_name=match.variants.inner(c(first,last),allfirsts,
                                      alllasts,nickname.gends)
  }else{
    matched_name=first
  }
  return(matched_name)
}
paste.first.last=function(x,first_names,last_names){
  fn=first_names[[x]]
  ln=last_names[[x]]
  return(paste0(ln,", ",fn,collapse="; "))
}

## New functions for Step6_BuildGenderData.R
get.first.last=function(x){
  fa=get.preferred(head(x,1))
  la=get.preferred(tail(x,1))
  return(c(fa,la))
}
match.common=function(x,namegends,commonnames){
  this_name=namegends$name[x]
  cn_index=which(commonnames$name==this_name)
  if(length(cn_index)>0){
    return(commonnames[cn_index,])
  }else{
    return(namegends[x,])
  }
}

## New functions for Step7_AssignGenders.R
gend.to.auths=function(first_last_auths,namegends,threshold=0.7){
  fa_index=which(namegends$name==first_last_auths[1])
  la_index=which(namegends$name==first_last_auths[2])
  
  fa_gend=ifelse(namegends$prob.m[fa_index]>threshold,"M",
                 ifelse(namegends$prob.w[fa_index]>threshold,"W","U"))
  la_gend=ifelse(namegends$prob.m[la_index]>threshold,"M",
                 ifelse(namegends$prob.w[la_index]>threshold,"W","U"))
  return(paste0(fa_gend,la_gend))
}

## New functions for Step8_PrepReferenceLists.R
authsplit=function(x){
  strsplit(x,"; ")[[1]]
}
get.cited.indices=function(x,DI,CR){
  cited.split=strsplit(CR[x]," ")[[1]]
  cited.dois=which(cited.split=="doi")+1
  cited.dois=cited.split[cited.dois]
  cited.dois=tolower(gsub(";|,|\\[|\\]","",cited.dois))
  cited.dois=cited.dois[cited.dois!="doi"]
  
  cited.indices=paste(which(DI%in%cited.dois),collapse=", ")
  return(cited.indices)
}
get.self.cites=function(x,first_auths,last_auths){
  these_auths=c(first_auths[[x]],last_auths[[x]])
  
  self=paste(which(first_auths%in%these_auths | 
                     last_auths%in%these_auths),collapse=", ")
  return(self)
}
transform.cat=function(x){
  ifelse(x=="MM",0,
         ifelse(x=="WM",1,
                ifelse(x=="MW",2,
                       ifelse(x=="WW",3,NA))))
}
get.uncond.exp=function(x,gender_cat,month_from_base){
  if(!is.na(x)){
    if(x>min(month_from_base,na.rm=T)){
      citable_papers=gender_cat[month_from_base<x]
      prop_tab=table(factor(citable_papers,lev=0:3))
      expecs_uncond=prop_tab/sum(prop_tab)
      return(expecs_uncond)
    }else{
      return(rep(NA,4))
    }
  }else{
    return(rep(NA,4))
  }
}
match.uncond.exp=function(x,uncond_expecs,unique_months){
  if(!is.na(x)){
    return(uncond_expecs[[which(unique_months==x)]])
  }else{
    return(uncond_expecs[[which(is.na(unique_months))]])
  }
}

## New functions for Step9_GetReferenceMeasures.R
get.ref.props=function(x,article.data,uncond_expecs,cond_expecs){
  cited.papers=strsplit(article.data$CP[x],", ")[[1]]
  self.authored=strsplit(article.data$SA[x],", ")[[1]]
  cited.notself=as.numeric(cited.papers[!(cited.papers%in%self.authored)])
  cited.notself=cited.notself[!is.na(article.data$GC[cited.notself])]
  if(length(cited.notself)>0){
    cited.genders=article.data$GC[cited.notself]
    gender.table=table(factor(cited.genders,lev=0:3))
    observed.props=gender.table/sum(gender.table)
    uncond.exp.props=uncond_expecs[x,]
    
    if(length(cited.notself)>1){
      cond.exp.props=apply(cond_expecs[cited.notself,],2,mean)
    }else{
      cond.exp.props=cond_expecs[cited.notself,]
    }
    
    return(c(as.numeric(observed.props),
             as.numeric(uncond.exp.props),
             as.numeric(cond.exp.props),
             length(cited.notself)))
  }else{
    return(c(rep(NA,12),0))
  }
}

## New functions for Step10_GetNetworkMeasures.R
get.prev.coauths=function(x,all_auth_names,month_from_base,year){
  this_auths=all_auth_names[[x]]
  
  if(sum(is.na(month_from_base))==0){
    date=month_from_base
  }else if(sum(is.na(year))==0){
    date=year
  }else{
    stop("Entries cannot be NA for both month and year")
  }
  this_date=date[x]
  earlier_than_this=(date<=this_date)
  
  auths_in=rep(FALSE,length(all_auth_names))
  
  sub_auth_names=all_auth_names[earlier_than_this]
  auths_in_sub=sapply(seq_along(sub_auth_names),
                      function(y){this_auths %in% sub_auth_names[[y]]})
  if(!is.null(nrow(auths_in_sub))){
    auths_in_sub=apply(auths_in_sub,2,sum)>0
  }
  auths_in[earlier_than_this]=auths_in_sub
  auths_in=which(auths_in)
  
  prev_coauths=unique(unlist(all_auth_names[auths_in]))
  prev_coauths=prev_coauths[!(prev_coauths%in%this_auths)]
  return(prev_coauths)
}
get.ma.overrep=function(x,prev_coauths,all_auth_names,
                        month_from_base,year,author_gends){
  if(sum(is.na(month_from_base))==0){
    date=month_from_base
  }else if(sum(is.na(year))==0){
    date=year
  }else{
    stop("Entries cannot be NA for both month and year")
  }
  this_date=date[x]
  
  prev_coauth=prev_coauths[[x]]
  prev_coauth_gends=author_gends$gend[author_gends$name%in%prev_coauth]
  
  if(length(prev_coauth_gends)>0){
    pc_man_prop=sum(prev_coauth_gends=="M")/sum(prev_coauth_gends!="U")
    
    earlier_than_this=(date<=this_date)
    
    whole_field=unique(unlist(all_auth_names[earlier_than_this]))
    whole_field_gends=author_gends$gend[author_gends$name%in%whole_field]
    wf_man_prop=sum(whole_field_gends=="M")/sum(whole_field_gends!="U")
    
    return(pc_man_prop-wf_man_prop)
  }else{
    return(NA)
  }
}
get.mmp.overrep=function(x,prev_coauths,all_auth_names,
                         month_from_base,year,article_gends){
  if(sum(is.na(month_from_base))==0){
    date=month_from_base
  }else if(sum(is.na(year))==0){
    date=year
  }else{
    stop("Entries cannot be NA for both month and year")
  }
  this_date=date[x]
  earlier_than_this=(date<=this_date)
  
  prev_coauth=prev_coauths[[x]]
  
  pc_papers=rep(0,length(all_auth_names))
  sub_auth_names=all_auth_names[earlier_than_this]
  
  pc_papers_sub=sapply(seq_along(sub_auth_names),
                       function(x){prev_coauth %in% sub_auth_names[[x]]})
  if(is.null(nrow(pc_papers_sub))){
    pc_papers[earlier_than_this]=pc_papers_sub
    pc_papers=which(pc_papers>0)
  }else{
    pc_papers[earlier_than_this]=apply(pc_papers_sub,2,sum)
    pc_papers=which(pc_papers>0)
  }
  
  if(length(pc_papers)>0){
    pc_paper_gends=article_gends[pc_papers]
    pc_mm_prop=sum(pc_paper_gends=="MM")/sum(!grepl("U",pc_paper_gends))
    
    all_paper_gends=article_gends[earlier_than_this]
    all_mm_prop=sum(all_paper_gends=="MM")/sum(!grepl("U",all_paper_gends))
    
    return(pc_mm_prop-all_mm_prop)
  }else{
    return(NA)
  }
}

## New functions for Step11_RunAnalyses.R
transform.cat.4=function(x){
  ifelse(x%in%c("MM","WM","MW","WW"),x,"NA")
}
transform.cat.2=function(x){
  ifelse(x=="MM",x,
         ifelse(x%in%c("WM","MW","WW"),"W|W","NA"))
}
citeprops=function(ref_proportions,i=NULL,type='conditional'){
  if(is.null(i)){i=1:nrow(ref_proportions)}
  if(type=='conditional'){
    nas=apply(is.na(ref_proportions[i,c(1:12)]),1,sum)!=0
    i=i[!nas]
    out=round(matrix(apply(ref_tot_sub[i,c(1:4,9:12)],2,sum)/
                       sum(apply(ref_tot_sub[i,1:4],2,sum)),
                     byrow=T,ncol=4),3)
    rownames(out)=c("Observed props.","Expected props.")
  }else if(type=='randomdraw'){
    nas=apply(is.na(ref_proportions[i,c(1:12)]),1,sum)!=0
    i=i[!nas]
    out=round(matrix(apply(ref_tot_sub[i,c(1:4,5:8)],2,sum)/
                       sum(apply(ref_tot_sub[i,1:4],2,sum)),
                     byrow=T,ncol=4),3)
    rownames(out)=c("Observed props.","Expected props.")
  }else{
    stop("'type' must be either 'conditional' or 'randomdraw'")
  }
  colnames(out)=c("MM","WM","MW","WW")
  return(out)
}
citegap=function(ref_proportions,i=NULL,type='conditional'){
  if(is.null(i)){i=1:nrow(ref_proportions)}
  if(type=='conditional'){
    nas=apply(is.na(ref_proportions[i,c(1:12)]),1,sum)!=0
    i=i[!nas]
    out=apply(ref_proportions[i,1:4],2,sum)/
      apply(ref_proportions[i,9:12],2,sum)-1
  }else if(type=='randomdraw'){
    nas=apply(is.na(ref_proportions[i,c(1:12)]),1,sum)!=0
    i=i[!nas]
    out=apply(ref_proportions[i,1:4],2,sum)/
      apply(ref_proportions[i,5:8],2,sum)-1
  }else{
    stop("'type' must be either 'conditional' or 'randomdraw'")
  }
  names(out)=c("MM","WM","MW","WW")
  return(out)
}
netgap=function(network_measure,i=NULL,groups,cites,verbose=F){
  if(is.null(i)){i=1:length(network_measure)}
  network_measure=network_measure[i]
  groups=groups[i]
  cites=cites[i]
  vals=suppressWarnings(summary(rq(network_measure~groups,
                                   weights=cites)))
  vals=as.numeric(vals$coefficients[1:4,1])
  vals=c(vals[1],vals[2:4]+vals[1])
  if(verbose==T){
    names(vals)=c("Among MM","Among WM","Among MW","Among WW")
    return(vals)
  }else{
    return(vals)
  }
  
}
medover=function(mm_overcite,i=NULL,groups,cites,verbose=F,
                 network=F,ma_overrep=NULL,mmp_overrep=NULL){
  if(is.null(i)){i=1:length(mm_overcite)}
  mm_overcite=mm_overcite[i]
  groups=groups[i]
  cites=cites[i]
  if(network==F){
    vals=suppressWarnings(summary(rq(mm_overcite~groups,
                                     weights=cites)))
  }else if(network==T){
    if(!is.null(ma_overrep) & !is.null(mmp_overrep)){
      ma_overrep=ma_overrep[i]
      mmp_overrep=mmp_overrep[i]
      vals=suppressWarnings(summary(rq(mm_overcite~groups+
                                         ma_overrep+mmp_overrep,
                                       weights=cites)))
    }else{
      stop("Must include ma_overrep and mmp_overrep")
    }
  }
  vals=as.numeric(vals$coefficients[1:4,1])
  vals=c(vals[1],vals[2:4]+vals[1])
  if(verbose==T){
    names(vals)=c("Among MM","Among WM","Among MW","Among WW")
    return(vals)
  }else{
    return(vals)
  }
}
citegap.temp=function(citation.totals,i=NULL,years,return='mm'){
  if(is.null(i)){i=1:nrow(citation.totals)}
  nas=apply(is.na(citation.totals[i,c(1:12)]),1,sum)!=0
  i=i[!nas]
  unique.years=sort(unique(years),decreasing=F)
  years=years[i]
  citation.totals=citation.totals[i,]
  if(return=='all'){
    year.vals=matrix(0,nrow=length(unique.years),ncol=4)
    for(j in 1:length(unique.years)){
      tyear=unique.years[j]
      if(sum(years==tyear)>1){
        year.vals[j,]=apply(citation.totals[years==tyear,1:4],2,sum)/
          apply(citation.totals[years==tyear,9:12],2,sum)-1
      }else{
        year.vals[j,]=NA
      }
    }
    out=year.vals
  }else if(return=='mm'){
    year.vals=rep(0,length(unique.years))
    for(j in 1:length(unique.years)){
      tyear=unique.years[j]
      if(sum(years==tyear)>1){
        year.vals[j]=(sum(citation.totals[years==tyear,1])-
                        sum(citation.totals[years==tyear,9]))/
          sum(apply(citation.totals[years==tyear,9:12],2,sum))
      }else{
        year.vals[j]=NA
      }
    }
    out=summary(lm(year.vals~unique.years))$coefficients[2,1]
    out=round(out*100,6)
    names(out)=c("MM overcitation yearly trend (perc. points)")
  }else{
    stop("'return' must be either 'mm' or 'all'")
  }
  return(out)
}
citegap.temp2=function(citation.totals,i=NULL,years){
  if(is.null(i)){i=1:nrow(citation.totals)}
  nas=apply(is.na(citation.totals[i,c(1:12)]),1,sum)!=0
  i=i[!nas]
  unique.years=sort(unique(years),decreasing=F)
  years=years[i]
  citation.totals=citation.totals[i,]
  
  year.vals=matrix(0,nrow=length(unique.years),ncol=8)
  for(j in 1:length(unique.years)){
    tyear=unique.years[j]
    if(sum(years==tyear)>1){
      obs=apply(citation.totals[years==tyear,1:4],2,sum)
      exp=apply(citation.totals[years==tyear,9:12],2,sum)
      tot=sum(obs)
      year.vals[j,]=c(obs/tot,exp/tot)
    }else{
      year.vals[j,]=c(NA,NA)
    }
  }
  out=year.vals
  
  return(out)
}
netgap.temp=function(network_measure,i=NULL,groups,cites,years){
  if(is.null(i)){i=1:length(network_measure)}
  unique.years=sort(unique(years),decreasing=F)
  network_measure=network_measure[i]
  groups=groups[i]
  cites=cites[i]
  years=years[i]
  
  year.vals=matrix(0,nrow=length(unique.years),ncol=4)
  for(j in 1:length(unique.years)){
    tyear=unique.years[j]
    vals=c(median(rep(network_measure[years==tyear & groups=="MM"],
                      cites[years==tyear & groups=="MM"]),na.rm=T),
           median(rep(network_measure[years==tyear & groups=="WM"],
                      cites[years==tyear & groups=="WM"]),na.rm=T),
           median(rep(network_measure[years==tyear & groups=="MW"],
                      cites[years==tyear & groups=="MW"]),na.rm=T),
           median(rep(network_measure[years==tyear & groups=="WW"],
                      cites[years==tyear & groups=="WW"]),na.rm=T))
    year.vals[j,]=vals
  }
  out=year.vals
  
  return(out)
}
get.timedf=function(article.data,journal=NULL){
  if(is.null(journal)){
    timedata=NULL
    for(i in unique(article.data$PY)){
      subgends=article.data$AG[article.data$PY==i & 
                                 article.data$AG%in%c("MM","WM","MW","WW")]
      timedata.i=data.frame(Year=rep(i,4),Gender=c("MM","WM","MW","WW"),
                            Prop=c(mean(subgends=="MM"),
                                   mean(subgends=="WM"),
                                   mean(subgends=="MW"),
                                   mean(subgends=="WW")),
                            Color=c("darkslateblue","darkslategray4",
                                    "lightcyan3","lightsalmon3"))
      timedata=rbind(timedata,timedata.i)
    }
  }else{
    timedata=NULL
    if(!journal%in%article.data$SO){
      stop("'journal' must be one entered in the same format as article.data$SO")
    }
    for(i in unique(article.data$PY)){
      subgends=article.data$AG[article.data$PY==i & article.data$SO==journal &
                                 article.data$AG%in%c("MM","WM","MW","WW")]
      timedata.i=data.frame(Year=rep(i,4),Gender=c("MM","WM","MW","WW"),
                            Prop=c(mean(subgends=="MM"),
                                   mean(subgends=="WM"),
                                   mean(subgends=="MW"),
                                   mean(subgends=="WW")),
                            Color=c("darkslateblue","darkslategray4",
                                    "lightcyan3","lightsalmon3"))
      timedata=rbind(timedata,timedata.i)
    }
  }
  timedata$Color=as.character(timedata$Color)
  return(timedata)
}
get.plotdf=function(boot){
  plotdf=data.frame(Group=c("Man &\nMan","Woman &\nMan",
                            "Man &\nWoman","Woman &\nWoman"),
                    Prop=boot$t0,
                    LB=apply(boot$t,2,quantile,.025),
                    UB=apply(boot$t,2,quantile,.975))
  plotdf$Group<-factor(plotdf$Group,
                       levels=c("Man &\nMan","Woman &\nMan",
                                "Man &\nWoman","Woman &\nWoman"))
  return(plotdf)
}
get.plotdf.temp=function(boot,unique.years){
  plotdf=data.frame(Group=rep(c("Man &\nMan","Woman &\nMan","Man &\nWoman",
                                "Woman &\nWoman"),each=length(unique.years)),
                    Year=rep(unique.years,4),
                    Prop=as.vector(boot$t0),
                    LB=apply(boot$t,2,quantile,.025),
                    UB=apply(boot$t,2,quantile,.975))
  plotdf$Group<-factor(plotdf$Group,
                       levels=c("Man &\nMan","Woman &\nMan",
                                "Man &\nWoman","Woman &\nWoman"))
  return(plotdf)
}
get.plotdf.temp2=function(boot,unique.years){
  plotdf=data.frame(Year=rep(unique.years,8),
                    Type=c(rep("Observed",4*length(unique.years)),
                           rep("Expected",4*length(unique.years))),
                    Group=rep(rep(c("Man &\nMan","Woman &\nMan","Man &\nWoman",
                                "Woman &\nWoman"),each=length(unique.years)),2),
                    Prop=as.vector(boot$t0),
                    LB=apply(boot$t,2,quantile,.025),
                    UB=apply(boot$t,2,quantile,.975))
  plotdf$Group<-factor(plotdf$Group,
                       levels=c("Man &\nMan","Woman &\nMan",
                                "Man &\nWoman","Woman &\nWoman"))
  plotdf=as.data.table(plotdf)

  return(plotdf)
}
get.ylim=function(plotdf1,plotdf2,group,type){
  if(type=="lower"){
    return(0.975*min(c(plotdf1$LB[plotdf1$Group==group],
                plotdf2$LB[plotdf2$Group==group])))
  }else if(type=="upper"){
    return(1.025*max(c(plotdf1$UB[plotdf1$Group==group],
                       plotdf2$UB[plotdf2$Group==group])))
  }else{stop("'type' must be either 'lower' or 'upper'")}
}
equate.plotdf.lims=function(plotdf1,plotdf2){
  plotdf1[Group=="Man &\nMan",y_min:=get.ylim(plotdf1,plotdf2,
                                             "Man &\nMan","lower")]
  plotdf1[Group=="Man &\nMan",y_max:=get.ylim(plotdf1,plotdf2,
                                             "Man &\nMan","upper")]
  plotdf1[Group=="Woman &\nMan",y_min:=get.ylim(plotdf1,plotdf2,
                                               "Woman &\nMan","lower")]
  plotdf1[Group=="Woman &\nMan",y_max:=get.ylim(plotdf1,plotdf2,
                                               "Woman &\nMan","upper")]
  plotdf1[Group=="Man &\nWoman",y_min:=get.ylim(plotdf1,plotdf2,
                                               "Man &\nWoman","lower")]
  plotdf1[Group=="Man &\nWoman",y_max:=get.ylim(plotdf1,plotdf2,
                                               "Man &\nWoman","upper")]
  plotdf1[Group=="Woman &\nWoman",y_min:=get.ylim(plotdf1,plotdf2,
                                                 "Woman &\nWoman","lower")]
  plotdf1[Group=="Woman &\nWoman",y_max:=get.ylim(plotdf1,plotdf2,
                                                 "Woman &\nWoman","upper")]
  
  plotdf2[Group=="Man &\nMan",y_min:=get.ylim(plotdf1,plotdf2,
                                              "Man &\nMan","lower")]
  plotdf2[Group=="Man &\nMan",y_max:=get.ylim(plotdf1,plotdf2,
                                              "Man &\nMan","upper")]
  plotdf2[Group=="Woman &\nMan",y_min:=get.ylim(plotdf1,plotdf2,
                                                "Woman &\nMan","lower")]
  plotdf2[Group=="Woman &\nMan",y_max:=get.ylim(plotdf1,plotdf2,
                                                "Woman &\nMan","upper")]
  plotdf2[Group=="Man &\nWoman",y_min:=get.ylim(plotdf1,plotdf2,
                                                "Man &\nWoman","lower")]
  plotdf2[Group=="Man &\nWoman",y_max:=get.ylim(plotdf1,plotdf2,
                                                "Man &\nWoman","upper")]
  plotdf2[Group=="Woman &\nWoman",y_min:=get.ylim(plotdf1,plotdf2,
                                                  "Woman &\nWoman","lower")]
  plotdf2[Group=="Woman &\nWoman",y_max:=get.ylim(plotdf1,plotdf2,
                                                  "Woman &\nWoman","upper")]
  
  return(list(plotdf1,plotdf2))
}
f1plot=function(data,title,yl=T,xl=T){
  p=ggplot(data,aes(x=Year, y=Prop, fill=Color)) + 
    geom_area(alpha=0.9,size=.5,color="black")+theme_bw()+
    scale_fill_identity()+
    scale_x_continuous(limits=c(min(data$Year),max(data$Year)),
                       expand=c(0,0))+
    scale_y_continuous(limits=c(0,1),expand=c(0,0))+
    ggtitle(title)
  if(yl==T & xl==T){
    p+xlab("Year")+ylab("Proportion")
  }else{
    p
  }
}
f2plot=function(data,title,ymin,ymax,yl=T,xl=T,shortlab=F){
  p=ggplot(data,aes(x=Group, y=Prop, fill=Group))+
    geom_bar(stat="identity", color="black",position=position_dodge())+
    geom_errorbar(aes(ymin=LB,ymax=UB),width=.2)+
    theme_bw()+theme(legend.position="n")+
    geom_hline(yintercept=0,color='black',lty=1)+
    ggtitle(title)
  if(xl==T){
    p=p+xlab("Cited authors' gender (first & last)")
  }else{
    p=p+xlab(NULL)
  }
  if(shortlab==T){
    p=p+xlab("Cited authors")+
      scale_fill_manual(values=c("Overall"="gray24",
                                 "MM"="darkslateblue",
                                 "WM"="darkslategray4",
                                 "MW"="lightcyan3",
                                 "WW"="lightsalmon3"))
  }else{
    p=p+scale_fill_manual(values=c("Overall"="gray24",
                                   "Man &\nMan"="darkslateblue",
                                   "Woman &\nMan"="darkslategray4",
                                   "Man &\nWoman"="lightcyan3",
                                   "Woman &\nWoman"="lightsalmon3"))
  }
  if(yl==T){
    p=p+ylab("Percent over/undercitation")
  }else{
    p=p+ylab(NULL)
  }
  min.break=ceiling(ymin*10)/10
  max.break=floor(ymax*10)/10
  breaks=seq(min.break,max.break,(max.break-min.break)/4)
  p=p+scale_y_continuous(breaks=breaks,
                         limits=c(ymin,ymax))
  return(p)
}
f4Aplot=function(data,title,ymin,ymax){
  p=ggplot(data,aes(x=Year, y=Prop, color=Group))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=LB,ymax=UB),width=.2)+
    theme_bw()+theme(legend.position="n")+
    #ylab("Citation gap (obs % / exp %)")+
    ggtitle(title)+geom_hline(yintercept=0,color='black',lty=2)+
    scale_color_manual(values=c("Overall"="gray24",
                                "Man &\nMan"="darkslateblue",
                                "Woman &\nMan"="darkslategray4",
                                "Man &\nWoman"="lightcyan3",
                                "Woman &\nWoman"="lightsalmon3"))+
    ylab("Percent over/undercitation")+xlab("Year")
  
  min.break=ceiling(ymin*10)/10
  max.break=floor(ymax*10)/10
  breaks=seq(min.break,max.break,(max.break-min.break)/4)
  p=p+scale_y_continuous(breaks=breaks,
                         limits=c(ymin,ymax))

  return(p)
}
f4Bplot=function(data,title){
  p=ggplot(data,aes(x=Year, y=Prop, color=Group, lty=Type))+
    geom_line()+geom_point()+
    geom_errorbar(aes(ymin=LB,ymax=UB),width=.2)+
    facet_wrap(~Group, scales="free", nrow=2)+
    geom_blank(aes(y = data$y_min))+
    geom_blank(aes(y = data$y_max))+
    theme_bw()+ggtitle(title)+
    scale_color_manual(values=c("Overall"="gray24",
                                "Man &\nMan"="darkslateblue",
                                "Woman &\nMan"="darkslategray4",
                                "Man &\nWoman"="lightcyan3",
                                "Woman &\nWoman"="lightsalmon3"))+
    scale_linetype_manual(values=c("Observed"="solid","Expected"="dashed"))+
    ylab("Proportion of citations")+xlab("Year")+
    theme(legend.position="n",strip.background = element_blank(),
          strip.text.x = element_blank())
  return(p)
}
f5plot=function(data,title,ymin,ymax){
  min.break=ceiling(ymin*10)/10
  max.break=floor(ymax*10)/10
  breaks=seq(min.break,max.break,(max.break-min.break)/4)
  p=ggplot(data,aes(x=Year, y=Prop, color=Group))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=LB,ymax=UB),width=.2)+
    theme_bw()+theme(legend.position="bottom")+
    ggtitle(title)+geom_hline(yintercept=0,color='black',lty=2)+
    scale_color_manual(values=c("Man &\nMan"="#FDBC53",
                                "Woman &\nMan"="#798081",
                                "Man &\nWoman"="#95D5B2",
                                "Woman &\nWoman"="steelblue3"),
                       name="Citing\nauthors")+
    ylab("Median overrepresentation")+xlab("Year")+
    scale_y_continuous(breaks=breaks,limits=c(ymin,ymax))
  return(p)
}
f6plot=function(data,title,type,ymin,ymax){
  if(type=="A"){
    p=ggplot(data[data$Type=="A",])+
      geom_bar(aes(x=Group, y=Prop, fill=Group),
               stat="identity",color="black",position=position_dodge())+
      geom_errorbar(aes(x=Group,ymin=LB,ymax=UB),width=.2)
    
  }else{
    p=ggplot(data[data$Type=="B",])+
      geom_bar(aes(x=Group, y=Prop, fill=Group),
               stat="identity",color="black",position=position_dodge())+
      geom_errorbar(aes(x=Group,ymin=LB,ymax=UB),width=.2)+
      geom_bar(aes(x=data$Group[data$Type=="A"],y=data$Prop[data$Type=="A"]), 
               fill=NA,stat="identity",color="black",position=position_dodge(),
               lty="longdash")
  }
  min.break=ceiling(ymin*10)/10
  max.break=floor(ymax*10)/10
  breaks=seq(min.break,max.break,(max.break-min.break)/4)
  p+theme_bw()+theme(legend.position="n")+
    ggtitle(title)+xlab("Cited authors")+
    ylab("Percent over/undercitation")+
    scale_fill_manual(values=c("Man &\nMan"="#FDBC53",
                               "Woman &\nMan"="#798081",
                               "Man &\nWoman"="#95D5B2",
                               "Woman &\nWoman"="steelblue3"))+
    ylab("Median MM overcitation")+xlab(NULL)+
    scale_y_continuous(breaks=breaks,
                       limits=c(ymin,ymax))+
    geom_hline(yintercept=0,color='black',lty=1)
}

## New functions for Step11.5_GetNullDists.R
null.gend=function(cond_expec){
  sample(0:3,1,prob=cond_expec)
}
get.ref.props.null=function(x,article.data,uncond_expecs,
                            cond_expecs,gend_groups){
  cited.papers=strsplit(article.data$CP[x],", ")[[1]]
  self.authored=strsplit(article.data$SA[x],", ")[[1]]
  cited.notself=as.numeric(cited.papers[!(cited.papers%in%self.authored)])
  cited.notself=cited.notself[!is.na(article.data$GC[cited.notself])]
  if(length(cited.notself)>0){
    cited.genders=gend_groups[cited.notself]
    gender.table=table(factor(cited.genders,lev=0:3))
    observed.props=gender.table/sum(gender.table)
    uncond.exp.props=uncond_expecs[x,]
    
    if(length(cited.notself)>1){
      cond.exp.props=apply(cond_expecs[cited.notself,],2,mean)
    }else{
      cond.exp.props=cond_expecs[cited.notself,]
    }
    
    return(c(as.numeric(observed.props),
             as.numeric(uncond.exp.props),
             as.numeric(cond.exp.props),
             length(cited.notself)))
  }else{
    return(c(rep(NA,12),0))
  }
}
get.null.vals=function(x,article.data,uncond_expecs,
                        cond_expecs,time_window,null=T){
  if(null){
    gend_groups=apply(cond_expecs,1,null.gend)
  }else{
    gend_groups=article.data$GC
  }
  ref_proportions=lapply(1:nrow(article.data),get.ref.props.null,
                         article.data,uncond_expecs,cond_expecs,
                         gend_groups)
  ref_proportions=do.call(rbind,ref_proportions)
  
  tw=article.data$PY%in%time_window
  has_citations=ref_proportions[,13]>0
  subset_articles=tw & has_citations
  
  gend_group_4=unlist(lapply(article.data$AG,transform.cat.4))
  gend_group_4=factor(gend_group_4,lev=c("MM","WM","MW","WW","NA"))
  gend_group_2=unlist(lapply(article.data$AG,transform.cat.2))
  gend_group_2=factor(gend_group_2,lev=c("MM","W|W","NA"))
  
  ref_prop_sub=ref_proportions[subset_articles,]
  ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]
  
  gap.ov=citegap(ref_tot_sub,type='conditional')
  
  gend2_sub=gend_group_2[subset_articles]
  gend4_sub=gend_group_4[subset_articles]
  year_sub=article.data$PY[subset_articles]
  
  gap.mm=citegap(ref_tot_sub[gend2_sub=="MM",],type='conditional')
  gap.wow=citegap(ref_tot_sub[gend2_sub=="W|W",],type='conditional')
  
  trend.ov=citegap.temp(ref_tot_sub,years=year_sub)
  trend.mm=citegap.temp(ref_tot_sub[gend2_sub=="MM",],years=year_sub[gend2_sub=="MM"])
  trend.wow=citegap.temp(ref_tot_sub[gend2_sub=="W|W",],years=year_sub[gend2_sub=="W|W"])
  
  res=c(gap.ov,gap.mm,gap.wow,trend.ov,trend.mm,trend.wow)
  names(res)=c("MM by all","WM by all","MW by all","WW by all",
               "MM by MM","WM by MM","MW by MM","WW by MM",
               "MM by WoW","WM by WoW","MW by WoW","WW by WoW",
               "MM trend by all","MM trend by MM","MM trend by WoW")
  return(res)
}
get.pvals=function(vec){
  return(mean(abs(vec[-1])>abs(vec[1])))
}
