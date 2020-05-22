setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(ggplot2);library(mgcv);library(patchwork)
library(boot);library(pbmcapply)
library(data.table);library(quantreg)

# Load in data from step 10
load("df10_articledata_propdata_netdata.RData")

# Save number of cores on machine
cores=detectCores()

## Find subset of articles for analysis
## I.e., articles in a specific window that contain at least one relevant reference
time_window=article.data$PY%in%c(2009:2019)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations

## Create gender category vectors
gend_group_4=unlist(lapply(article.data$AG,transform.cat.4))
gend_group_4=factor(gend_group_4,lev=c("MM","WM","MW","WW","NA"))
gend_group_2=unlist(lapply(article.data$AG,transform.cat.2))
gend_group_2=factor(gend_group_2,lev=c("MM","W|W","NA"))

###################################
## Recreate graphs from Figure 1 ##
###################################

# Get overall authorship breakdown by year
timedata=get.timedf(article.data)
p.ov=f1plot(timedata,"Overall authorship by year")

# Get journal-specific graphs
# Change functions to the journal names in your data
td.1=get.timedf(article.data,'JOURNAL 1')
td.2=get.timedf(article.data,'JOURNAL 2')
# ...
p.1=f1plot(td.1,'Journal 1',yl=F,xl=F)
p.2=f1plot(td.2,'Journal 2',yl=F,xl=F)
# ...

# View plots
p.ov/(p.1+p.2)

#########################################################
## Calculate citation gaps across cited author genders ##
#########################################################

# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

###################################
## Recreate graphs from Figure 2 ##
###################################

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=500,type='randomdraw')
boot.cn=boot(ref_tot_sub,citegap,R=500,type='conditional')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)
plot.df.conditional=get.plotdf(boot.cn)
p.all.rd=f2plot(plot.df.randomdraw,"Gap relative to literature",
                ymin=-0.35,ymax=0.35)
p.all.cn=f2plot(plot.df.conditional,"Gap conditional on characteristics",
                ymin=-0.35,ymax=0.35)

# View plots
p.all.rd
p.all.cn

##################################################
## Get breakdowns based on citing author gender ##
##################################################

# Get subset of group labels
gend2_sub=gend_group_2[subset_articles]
gend4_sub=gend_group_4[subset_articles]

# Gap within reference lists of MM papers
citegap(ref_tot_sub[gend2_sub=="MM",],type='conditional')

# Gap within reference lists of W|W papers
citegap(ref_tot_sub[gend2_sub=="W|W",],type='conditional')

# Gap within subgroups of W|W papers
citegap(ref_tot_sub[gend4_sub=="WM",],type='conditional')
citegap(ref_tot_sub[gend4_sub=="MW",],type='conditional')
citegap(ref_tot_sub[gend4_sub=="WW",],type='conditional')

###################################
## Recreate graphs from Figure 3 ##
###################################

# Get bootstrap standard errors for gap values
boot.mm=boot(ref_tot_sub[gend2_sub=="MM",],citegap,R=500)
boot.worw=boot(ref_tot_sub[gend2_sub=="W|W",],citegap,R=500)
boot.wm=boot(ref_tot_sub[gend4_sub=="WM",],citegap,R=500)
boot.mw=boot(ref_tot_sub[gend4_sub=="MW",],citegap,R=500)
boot.ww=boot(ref_tot_sub[gend4_sub=="WW",],citegap,R=500)

# Create ggplot compatible data frames
plot.df.mm=get.plotdf(boot.mm)
plot.df.worw=get.plotdf(boot.worw)
plot.df.wm=get.plotdf(boot.wm)
plot.df.mw=get.plotdf(boot.mw)
plot.df.ww=get.plotdf(boot.ww)
p.mm=f2plot(plot.df.mm,"Citing: MM",ymin=-0.40,ymax=0.40)
p.worw=f2plot(plot.df.worw,"Citing: W or W",ymin=-0.40,ymax=0.40)
p.wm=f2plot(plot.df.wm,"Citing: WM",ymin=-0.40,ymax=0.40)
p.mw=f2plot(plot.df.mw,"Citing: MW",ymin=-0.40,ymax=0.40)
p.ww=f2plot(plot.df.ww,"Citing: WW",ymin=-0.40,ymax=0.40)

# View plots
p.mm
p.worw
p.wm
p.mw
p.ww

#####################################################
## Get temporal trends across citing author gender ##
#####################################################

# Get subset of year labels
year_sub=article.data$PY[subset_articles]

# Yearly MM overcitation trend within all reference lists
citegap.temp(ref_tot_sub,years=year_sub)

# Yearly MM overcitation trend within MM reference lists
citegap.temp(ref_tot_sub[gend2_sub=="MM",],
             years=year_sub[gend2_sub=="MM"])

# Yearly MM overcitation trend within W|W lists
citegap.temp(ref_tot_sub[gend2_sub=="W|W",],
             years=year_sub[gend2_sub=="W|W"])

####################################
## Recreate graphs from Figure 4A ##
####################################

# Get bootstrap standard errors for gap values
boot.mm.temp=boot(ref_tot_sub[gend2_sub=="MM",],citegap.temp,
                  years=year_sub[gend2_sub=="MM"],return="all",R=500)
boot.worw.temp=boot(ref_tot_sub[gend2_sub=="W|W",],citegap.temp,
                    years=year_sub[gend2_sub=="W|W"],return="all",R=500)

# Create ggplot compatible data frames
unique.years=sort(unique(year_sub))
plot.df.mm.temp=get.plotdf.temp(boot.mm.temp,unique.years)
plot.df.worw.temp=get.plotdf.temp(boot.worw.temp,unique.years)
p.mm.temp=f4Aplot(plot.df.mm.temp,"Citing: MM",ymin=-0.4,ymax=0.4)
p.worw.temp=f4Aplot(plot.df.worw.temp,"Citing: W or W",ymin=-0.4,ymax=0.4)

# View plots
p.mm.temp
p.worw.temp

####################################
## Recreate graphs from Figure 4B ##
####################################

# Get bootstrap standard errors for gap values
boot.mm.temp2=boot(ref_tot_sub[gend2_sub=="MM",],citegap.temp2,
                   years=year_sub[gend2_sub=="MM"],R=500)
boot.worw.temp2=boot(ref_tot_sub[gend2_sub=="W|W",],citegap.temp2,
                     years=year_sub[gend2_sub=="W|W"],R=500)

# Create ggplot compatible data frames
plot.df.mm.temp2=get.plotdf.temp2(boot.mm.temp2,unique.years)
plot.df.worw.temp2=get.plotdf.temp2(boot.worw.temp2,unique.years)
equated.ylims=equate.plotdf.lims(plot.df.mm.temp2,plot.df.worw.temp2)
p.mm.temp2=f4Bplot(equated.ylims[[1]],"Citing: MM")
p.worw.temp2=f4Bplot(equated.ylims[[2]],"Citing: W or W")

# View plots
p.mm.temp2
p.worw.temp2

#############################################################
## Get MA and MMP overreppresentation across author groups ##
#############################################################

# Get subset of network measures
ma_over_sub=ma_overrep[subset_articles]
mmp_over_sub=mmp_overrep[subset_articles]
num_cited_sub=ref_prop_sub[,13]

# Median man author overrepresentation
netgap(ma_over_sub,groups=gend4_sub,cites=num_cited_sub,verbose=T)

# Median man/man paper overrepresentation
netgap(mmp_over_sub,groups=gend4_sub,cites=num_cited_sub,verbose=T)

###################################
## Recreate graphs from Figure 5 ##
###################################

# Get bootstrap standard errors for overrep values - about 30 seconds each
boot.MA=boot(ma_over_sub,netgap.temp,groups=gend4_sub,
             cites=num_cited_sub,years=year_sub,R=500)
boot.MMP=boot(mmp_over_sub,netgap.temp,groups=gend4_sub,
              cites=num_cited_sub,years=year_sub,R=500)

# Create ggplot compatible data frames
plot.df.MA=get.plotdf.temp(boot.MA,unique.years)
plot.df.MMP=get.plotdf.temp(boot.MMP,unique.years)
p.MA=f5plot(plot.df.MA,"Man author overrepresentation",
            ymin=-0.10,ymax=0.10)
p.MMP=f5plot(plot.df.MMP,"MM paper overrepresentation",
             ymin=-0.10,ymax=0.10)

# View plots
p.MA
p.MMP

##############################################################
## Get MM overcitation before/after accounting for networks ##
##############################################################

# Calculate proportional overcitation of MM papers (observed %MM - expected %MM)
mm_overcite_sub=ref_prop_sub[,1]-ref_prop_sub[,9]

# Median MM overcitation without accounting for networks
medover(mm_overcite_sub,groups=gend4_sub,cites=num_cited_sub,
        verbose=T,network=F)

# Median MM overcitation after accounting for networks
medover(mm_overcite_sub,groups=gend4_sub,cites=num_cited_sub,verbose=T,
        network=T,ma_overrep=ma_over_sub,mmp_overrep=mmp_over_sub)

###################################
## Recreate graphs from Figure 6 ##
###################################

# Get bootstrap standard errors for overcite values - about 30 seconds each
boot.MO.nonet=boot(mm_overcite_sub,medover,groups=gend4_sub,
                   cites=num_cited_sub,network=F,R=500)
boot.MO.net=boot(mm_overcite_sub,medover,groups=gend4_sub,
                 cites=num_cited_sub,network=T,ma_overrep=ma_over_sub,
                 mmp_overrep=mmp_over_sub,R=500)

# Create ggplot compatible data frames
plot.df.MO=rbind(get.plotdf(boot.MO.nonet),get.plotdf(boot.MO.net))
plot.df.MO$Type=c(rep("A",4),rep("B",4))
p.MO.nonet=f6plot(plot.df.MO,"Unconditional MM overcitation",
                  type="A",ymin=-0.10,ymax=0.10)
p.MO.net=f6plot(plot.df.MO,"MM overcitation | Network",
                type="B",ymin=-0.10,ymax=0.10)

# View plots
p.MO.nonet
p.MO.net

