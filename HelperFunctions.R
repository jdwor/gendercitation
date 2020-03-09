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
extract.initials=function(name){
  name=gsub("[[:punct:][:blank:]]","",name)
  name=gsub("[:a-z:]","",name)
  return(name)
}
match.initials=function(x,first_names,last_names,allfirsts,alllasts){
  authors=first_names[[x]]
  need=which(toupper(authors)==authors)
  if(length(need)==0){
    return(authors)
  }else{
    for(i in need){
      target.first=authors[i]
      target.initials=extract.initials(target.first)
      target.last=last_names[[x]][i]
      others=which(tolower(target.last)==tolower(alllasts))
      if(length(others)>1){
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
          authors[i]=matched.name
        }else if(length(contained)>0 & 
                 sum(contained)==length(contained)){
          matched.name=sort(table(allsimilar.full),decreasing=T)
          matched.name=names(matched.name)[1]
          authors[i]=matched.name
        }
      }
    }
    return(authors)
  }
}
find.variants=function(lastname,allfirsts,alllasts){
  samelasts=unique(allfirsts[alllasts==lastname])
  same.initials=substr(samelasts,1,1)
  if(max(table(same.initials))>1){
    return(1)
  }else{
    return(0)
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
match.variants.outer=function(x,first_names,last_names,allfirsts,
                              alllasts,may_have_variants,nickname.gends){
  sub_firsts=first_names[[x]]
  sub_lasts=last_names[[x]]
  sub_with_variants=which(sub_lasts%in%may_have_variants)
  if(length(sub_with_variants)>0){
    needed_names=cbind(sub_firsts[sub_with_variants],
                       sub_lasts[sub_with_variants])
    matched_names=apply(needed_names,1,match.variants.inner,allfirsts,
                        alllasts,nickname.gends)
    sub_firsts[sub_with_variants]=unlist(matched_names)
  }
  return(sub_firsts)
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
  if(x>min(month_from_base)){
    citable_papers=gender_cat[month_from_base<x]
    prop_tab=table(factor(citable_papers,lev=0:3))
    expecs_uncond=prop_tab/sum(prop_tab)
    return(expecs_uncond)
  }else{
    return(rep(NA,4))
  }
}
match.uncond.exp=function(x,uncond_expecs,unique_months){
  return(uncond_expecs[[which(unique_months==x)]])
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
get.prev.coauths=function(x,first_auths,last_auths,all_auth_names,month_from_base){
  fa_name=first_auths[[x]]
  la_name=last_auths[[x]]
  this_month=month_from_base[x]
  
  auths_in=rep(FALSE,length(all_auth_names))
  sub_auth_names=all_auth_names[month_from_base<=this_month]
  auths_in_sub=sapply(seq_along(sub_auth_names),
                      function(y){c(fa_name,la_name) %in% sub_auth_names[[y]]})
  auths_in_sub=apply(auths_in_sub,2,sum)>0
  auths_in[month_from_base<=this_month]=auths_in_sub
  auths_in=which(auths_in)
  
  prev_coauths=unique(unlist(all_auth_names[auths_in]))
  prev_coauths=prev_coauths[!(prev_coauths%in%c(fa_name,la_name))]
  return(prev_coauths)
}
get.ma.overrep=function(x,prev_coauths,all_auth_names,month_from_base,author_gends){
  this_month=month_from_base[x]
  
  prev_coauth=prev_coauths[[x]]
  prev_coauth_gends=author_gends$gend[author_gends$name%in%prev_coauth]
  
  if(length(prev_coauth_gends)>0){
    pc_man_prop=sum(prev_coauth_gends=="M")/sum(prev_coauth_gends!="U")
    
    whole_field=unique(unlist(all_auth_names[month_from_base<=this_month]))
    whole_field_gends=author_gends$gend[author_gends$name%in%whole_field]
    wf_man_prop=sum(whole_field_gends=="M")/sum(whole_field_gends!="U")
    
    return(pc_man_prop-wf_man_prop)
  }else{
    return(NA)
  }
}
get.mmp.overrep=function(x,prev_coauths,all_auth_names,month_from_base,article_gends){
  this_month=month_from_base[x]
  
  prev_coauth=prev_coauths[[x]]
  
  pc_papers=rep(0,length(all_auth_names))
  sub_auth_names=all_auth_names[month_from_base<this_month]
  
  pc_papers_sub=sapply(seq_along(sub_auth_names),
                       function(x){prev_coauth %in% sub_auth_names[[x]]})
  if(is.null(nrow(pc_papers_sub))){
    pc_papers[month_from_base<this_month]=pc_papers_sub
    pc_papers=which(pc_papers>0)
  }else{
    pc_papers[month_from_base<this_month]=apply(pc_papers_sub,2,sum)
    pc_papers=which(pc_papers>0)
  }
  
  if(length(pc_papers)>0){
    pc_paper_gends=article_gends[pc_papers]
    pc_mm_prop=sum(pc_paper_gends=="MM")/sum(!grepl("U",pc_paper_gends))
    
    all_paper_gends=article_gends[month_from_base<this_month]
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
citegap=function(ref_proportions,i=NULL,type='conditional'){
  if(is.null(i)){i=1:nrow(ref_proportions)}
  if(type=='conditional'){
    out=apply(ref_proportions[i,1:4],2,sum)/
      apply(ref_proportions[i,9:12],2,sum)-1
  }else if(type=='randomdraw'){
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
  unique.years=sort(unique(years),decreasing=F)
  years=years[i]
  citation.totals=citation.totals[i,]
  if(return=='all'){
    year.vals=matrix(0,nrow=length(unique.years),ncol=4)
    for(j in 1:length(unique.years)){
      tyear=unique.years[j]
      year.vals[j,]=apply(citation.totals[years==tyear,1:4],2,sum)/
        apply(citation.totals[years==tyear,9:12],2,sum)-1
    }
    out=year.vals
  }else if(return=='mm'){
    year.vals=rep(0,length(unique.years))
    for(j in 1:length(unique.years)){
      tyear=unique.years[j]
      year.vals[j]=(sum(citation.totals[years==tyear,1])-
                      sum(citation.totals[years==tyear,9]))/
        sum(apply(citation.totals[years==tyear,9:12],2,sum))
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
  unique.years=sort(unique(years),decreasing=F)
  years=years[i]
  citation.totals=citation.totals[i,]
  
  year.vals=matrix(0,nrow=length(unique.years),ncol=8)
  for(j in 1:length(unique.years)){
    tyear=unique.years[j]
    obs=apply(citation.totals[years==tyear,1:4],2,sum)
    exp=apply(citation.totals[years==tyear,9:12],2,sum)
    tot=sum(obs)
    year.vals[j,]=c(obs/tot,exp/tot)
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
get.plotdf=function(boot){
  plotdf=data.frame(Group=c("Man &\nMan","Woman &\nMan",
                            "Man &\nWoman","Woman &\nWoman"),
                    Prop=boot$t0,SE=apply(boot$t,2,sd))
  plotdf$Group<-factor(plotdf$Group,
                       levels=c("Man &\nMan","Woman &\nMan",
                                "Man &\nWoman","Woman &\nWoman"))
  return(plotdf)
}
get.plotdf.temp=function(boot){
  plotdf=data.frame(Group=rep(c("Man &\nMan","Woman &\nMan","Man &\nWoman",
                                "Woman &\nWoman"),each=10),Year=rep(2009:2018,4),
                    Prop=as.vector(boot$t0),SE=apply(boot$t,2,sd))
  plotdf$Group<-factor(plotdf$Group,
                       levels=c("Man &\nMan","Woman &\nMan",
                                "Man &\nWoman","Woman &\nWoman"))
  return(plotdf)
}
get.plotdf.temp2=function(boot){
  plotdf=data.frame(Year=rep(2009:2018,8),
                    Type=c(rep("Observed",40),rep("Expected",40)),
                    Group=rep(c("Man &\nMan","Woman &\nMan","Man &\nWoman",
                                "Woman &\nWoman"),each=10),
                    Prop=as.vector(boot$t0),SE=apply(boot$t,2,sd))
  plotdf$Group<-factor(plotdf$Group,
                       levels=c("Man &\nMan","Woman &\nMan",
                                "Man &\nWoman","Woman &\nWoman"))
  
  plotdf=as.data.table(plotdf)
  plotdf[Group=="Man &\nMan",y_min:=0.56]
  plotdf[Group=="Man &\nMan",y_max:=0.66]
  plotdf[Group=="Woman &\nMan",y_min:=0.20]
  plotdf[Group=="Woman &\nMan",y_max:=0.27]
  plotdf[Group=="Man &\nWoman",y_min:=0.077]
  plotdf[Group=="Man &\nWoman",y_max:=0.103]
  plotdf[Group=="Woman &\nWoman",y_min:=0.04]
  plotdf[Group=="Woman &\nWoman",y_max:=0.08]
  
  return(plotdf)
}
f2plot=function(data,title,yl=T,xl=T,all=F,shortlab=F){
  p=ggplot(data,aes(x=Group, y=Prop, fill=Group))+
    geom_bar(stat="identity", color="black",position=position_dodge())+
    geom_errorbar(aes(ymin=(Prop-SE),
                      ymax=(Prop+SE)),width=.2)+
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
  if(all==T){
    p=p+scale_y_continuous(breaks=seq(-.30,.30,.15),
                           labels=c("-30%","-15%","0%","+15%","+30%"),
                           limits=c(-.31,.31))
  }else{
    p=p+scale_y_continuous(breaks=seq(-.4,.4,.2),
                           labels=c("-40%","-20%","0%","+20%","+40%"),
                           limits=c(-.4,.4))
  }
}
f4Aplot=function(data,title){
  p=ggplot(data,aes(x=Year, y=Prop, color=Group))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=(Prop-SE),
                      ymax=(Prop+SE)),width=.2)+
    theme_bw()+theme(legend.position="n")+
    #ylab("Citation gap (obs % / exp %)")+
    ggtitle(title)+geom_hline(yintercept=0,color='black',lty=2)+
    scale_color_manual(values=c("Overall"="gray24",
                                "Man &\nMan"="darkslateblue",
                                "Woman &\nMan"="darkslategray4",
                                "Man &\nWoman"="lightcyan3",
                                "Woman &\nWoman"="lightsalmon3"))+
    ylab("Percent over/undercitation")+xlab("Year")+
    scale_y_continuous(breaks=seq(-.4,.4,.2),
                       labels=c("-40%","-20%","0%","+20%","+40%"),
                       limits=c(-.4,.4))
  return(p)
}
f4Bplot=function(data,title){
  p=ggplot(data,aes(x=Year, y=Prop, color=Group, lty=Type))+
    geom_line()+geom_point()+
    geom_errorbar(aes(ymin=(Prop-SE),ymax=(Prop+SE)),width=.2)+
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
f5plot=function(data,title){
  p=ggplot(data,aes(x=Year, y=Prop, color=Group))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=(Prop-SE),
                      ymax=(Prop+SE)),width=.2)+
    theme_bw()+theme(legend.position="bottom")+
    ggtitle(title)+geom_hline(yintercept=0,color='black',lty=2)+
    scale_color_manual(values=c("Man &\nMan"="#FDBC53",
                                "Woman &\nMan"="#798081",
                                "Man &\nWoman"="#95D5B2",
                                "Woman &\nWoman"="steelblue3"),
                       name="Citing\nauthors")+
    ylab("Median overrepresentation")+xlab("Year")+
    scale_y_continuous(breaks=seq(-.1,.1,.05),
                       labels=c("-0.10","-0.05","0.00","0.05","0.10"),
                       limits=c(-.1,.1))
  return(p)
}


