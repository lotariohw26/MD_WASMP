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
###### Normal 
######## R2 sim
dfm <- (function(x){data.frame(P=seq(1,x),RV=as.integer(rnorm(x,1000,30)))})(10)
probw <- c(m=0.51,s=0.10)
probva <- c(vdm=0.7,mdm=0.4,vds=0.10,mds=0.10)
probvb <- c(vdm=0.5,mdm=0.6,vds=0.10,mds=0.10)
ztech <- c(0,1)	
app_bal <- ballcastsim(dfm,probw,probva,probvb,ztech)
########## Normal form
app_n_rep <- selreport(app_bal,md$app0)
app_n_out <- seloutput(app_n_rep)
app_n_sim <- SimVoterdatabase(app_bal)
######## Rigged example 1: standard form
app_ex1_cou <- Countinggraphs(app_bal)
#print(app_ex1_cou$polyc[[1]][[1]])
app_ex1_cou$sortpre()
app_ex1_cou$mansys(sygen=list(frm=1,pre=c("alpha","x","y"),end=c("zeta","lamda"),me=c(plnr=1,rot=0)))
#app_ex1_cou$setres(NULL,0)
app_ex1_cou$setres(0.23,0)
app_ex1_cou$manimp(init_par=c(k0=0.0,k1=0.5,k2=0.5),TRUE,wn=c(0,0))
app_ex1_out <- seloutput(selreport(app_ex1_cou$rdfc,md$app0))
app_ex1_sim <- SimVoterdatabase(app_ex1_cou$rdfc)
######## Rigged example 2: hybrid form
app_ex2_cou <- Countinggraphs(app_bal)
pri_int_ex2 <- app_ex2_cou$polyc[[1]][[1]]
#print(app_ex2_cou$polyc[[1]][[1]])
app_ex2_cou$sortpre()
app_ex2_cou$mansys(sygen=list(frm=2,pre=c("alpha","g","h"),end=c("Gamma","Omega"),FALSE,me=c(plnr=1,rot=0)))
app_ex2_cou$setres(NULL,0)
app_ex2_cou$setres(0.23,0)
app_ex2_cou$manimp(init_par=c(k0=0.0,k1=0.5,k2=0.5),wn=c(0,0))
app_ex2_out <- seloutput(selreport(app_ex2_cou$rdfc,md$app0))
app_ex2_sim <- SimVoterdatabase(app_ex2_cou$rdfc)
###### Rigged example 3: Hybrid form
app_ex3_cou <- Countinggraphs(app_bal)
#pri_int_ex3 <- app_ex3_cou$polyc[[1]][[1]]
app_ex3_cou$sortpre()
app_ex3_cou$mansys(sygen=list(frm=1,pre=c("alpha","x","y"),end=c("zeta","lamda"),me=c(plnr=1,rot=0)))
app_ex3_cou$setres(NULL,0)
app_ex3_cou$setres(0.23,0)
app_ex3_cou$manimp(init_par=c(k0=0.0,k1=0.5,k2=0.5),wn=c(0,0))
app_ex3_out <- seloutput(selreport(app_ex3_cou$rdfc,md$app0))
app_ex3_sim <- SimVoterdatabase(app_ex3_cou$rdfc)
#####################################################################################################################################################################


