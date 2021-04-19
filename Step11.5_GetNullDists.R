setwd("path/to/project/folder") # Change to your project folder path
source("HelperFunctions.R")
library(pbmcapply)

# Load in data from step 10
load("df10_articledata_propdata_netdata.RData")

# Save number of desired cores for running analyses
cores=4

## Set time window of citing papers
## Should be the same as the time window set in Step 11
time_window=c(2009:2019)

## Save true measures for comparison to null distributions
observed_vals = get.null.vals(1,article.data,uncond_expecs,
                              cond_expecs,time_window,null=FALSE)

## Create matrix of null distributions for primary results
n.null = 1000 # set desired number of null runs
null_vals = pbmclapply(1:n.null,get.null.vals,article.data=article.data,
                       uncond_expecs=uncond_expecs,cond_expecs=cond_expecs,
                       time_window=time_window,null=TRUE,mc.cores=cores)
null_vals = do.call(rbind, null_vals)
summary(null_vals) # check that values are roughly centered around 0
hist(null_vals[,1]) # visualize null distribution of overall MM over/undercitation

## Get p-values for observed results
p_vals = apply(rbind(observed_vals, null_vals), 2, get.pvals)
p_vals

## Correct for multiple comparisons
p_vals_nozero = p_vals
p_vals_nozero[p_vals == 0] = 1/(n.null+1) ## set p=0 to the permuted upper bound (e.g., p<0.001)
p_vals_adj = p.adjust(p_vals_nozero,method="holm")
p_vals_adj

## Save null testing information
save(observed_vals,null_vals,p_vals,p_vals_nozero,p_vals_adj,
     file="df10_articledata_propdata_netdata.RData")

