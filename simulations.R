#######################################################################################################################################################
library(ManifoldDestinyWASMP)
library(ManifoldDestinyWASMD)
library(dplyr)
library(ggplot2)
library(htmltools)
library(plotly)
library(kableExtra)
library(huxtable)
library(gridExtra)
options(scipen=999)
set.seed(1)
source(paste0(rprojroot::find_rstudio_root_file(),'/R/wasmconverting.R'))
md <- ManifoldDestinyWASMD::metad
#########################################################################################################################################################
#########################################################################################################################################################
app_n_rep <- selreport(app0,md$app0)
#dfm <- (function(x){data.frame(P=seq(1,x),RV=as.integer(rnorm(x,1000,30)))})(10)
#probw <- c(m=0.51,s=0.10)
#probva <- c(vdm=0.7,mdm=0.4,vds=0.10,mds=0.10)
#probvb <- c(vdm=0.5,mdm=0.6,vds=0.10,mds=0.10)
#ztech <- c(0,1)	
#app_bal <- ballcastsim(dfm,probw,probva,probvb,ztech)
#SimVoterdatabase(app_bal)


