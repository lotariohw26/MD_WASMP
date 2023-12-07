#################################################################################################33
#library(ManifoldDestiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(randNames)
library(openxlsx)
library(patchwork)
library(writexl)
library(plotly)
library(ViewPipeSteps)
library(ggpubr)
library(htmltools)
library(gridExtra)
sapply(list.files(paste0(rprojroot::find_rstudio_root_file(),'/R'),full.names=T), source)
set.seed(1)
##################################################################################################
### R² Analysis 
##################################################################################################
prec <- 1:500
reg <- 500
probw <- 0.50
m1p <- m2p <- c(0.5,0.05)
probtb <- probta <- lapply(1:length(prec),function(x) {am1<-rnorm(1,m1p[1],m1p[2]);c(am1,1-am1,0)})
reg_form <- c('alpha~h+g','alpha~ g+ I(g^2)+ gh+ I(h^2)+ I(g^3)+ g2h+ I(h^3)')[1]
par_ele_1 <- data.frame(P=prec,R=reg,W=probw)
par_ele_1$Pa <- probta
par_ele_1$Pb <- probtb
sim_1 <- SimpleVoterdatabase(par_ele_1)
sim1_rep <- allreport(seldata=sim_1$listcbase,part=c(1,2,3),formulas=reg_form)
sim1_rep[[1]][[1]][[3]][[2]]
sim1_rep[[1]][[2]][[3]][[2]]
##################################################################################################
### Voterrollanalysis 
##################################################################################################
stat1_vg <- Voterdatabaseplots(type_nr=1,lsv=0)
stat1_vg$listvbase
dtc <- filter(stat1_vg$listcbase, cou_nr==1)
stat1_vg <- Countinggraphs(dtc)
#View(stat1_vg)
stat1_vg$sortpre(selvar=c('x','y','alpha','lambda','zeta'))
stat1_vg$plot2d()
stat1_vg$pl_2dsort
stat1_vg$plotxy()
stat1_vg$resplot()
stat1_vg$plotly3d(partition=2)
stat1_vg$gridarrange()
stat1_vg$all_pl_3dmani[[1]]
##################################################################################################
### Voterrollanalysis 
##################################################################################################
# Voterbase
#ohio_vb <- Voterdatabase(type_nr=2,lsv=1)
##ohio_vb$listvbase
#ohio_vb$regvbase()
#ohio_vb$scorecard()	
#ohio_vb$predictinput()
#ohio_vb$uploadvbase()
### Graphical
ohio_vg <-Voterdatabaseplots(type_nr=2)
ohio_vg$regvbase()
ohio_vg$scorecard()	
ohio_vg$predictinput()
ohio_vg$plot_predict()
ohio_vg$predictsc[[2]]
ohio_vg$lg_pred
ohio_vg$plot_keyrat()
ohio_vg$plot_histio()
ohio_vg$gridarrange()
ohio_vg$uploadvbase()
#ohio_vg$lg_pred
#ohio_vg$lg_keyr[[1]]
#ohio_vg$lg_hist[[1]]
## Report
ohio_vr <- Voterrollreport()
ohio_vr$regvbase()
ohio_vr$scorecard()	
ohio_vr$predictinput()
ohio_vr$htmlreport()
ohio_vr$uploadvbase()
#################################################################################################
### Election simulation 
##################################################################################################
state1_vb <- Voterdatabase(type_nr=1,lsv=0,probw=c(0.50,0.05),probv=list(c(0.60,0.30,0.10),c(0.30,0.60,0.10)),Ztech=c(0,1), cou_sim=list(state_sim='state1',cou_nr=1:2,cou_na=c("A","B"),nprect=c(20,20),tot_regis=c(0.80,0.80), 
						       agebrack=c(18,100,30)))
state2_vb <- Voterdatabase(type_nr=1,lsv=0,probw=c(0.50,0.05),probv=list(c(0.60,0.30,0.10),c(0.30,0.60,0.10)),Ztech=c(0,1), 
					  cou_sim=list(state_sim='state2',cou_nr=1:2,cou_na=c("A","B"),nprect=c(10,10),tot_regis=c(0.80,0.80), 
						       agebrack=c(18,100,30)))
state1_vb$regvbase()
state1_vb$scorecard()	
state1_vb$predictinput()
#################################################################################################
### Election simulation (Based on Dr. Frank)
##################################################################################################
### Part A: Fair election
### Part B: Rigged election
##################################################################################################
### Regression analysis (Based on Dr. Frank)
##################################################################################################
#### I: Voterdatabase ###
### Inititating
agebracketmax <- c(18,100,1000)
regf <- 0.8
numprec <- 200
nb <- 'defvotbase'
vrdf <- Voterdatabase(agebracketmax,numprec,regf,namebase=nb,newdraw=F)
###### Realization of DGP
probwset <- c(0.50,0.00)
probvset <- list(c(0.60,0.30,0.10),c(0.30,0.60,0.10))
Znr <- c(0,1)
vrdf$realizedgp(probv=probvset,probw=probwset,Ztech=Znr)
vrdf$regvbase()
#####
abc_vrg <- Voterrollgraphs(coudatafile='vtr_abc.rda')
abc_vrg$scorecard()
abc_vrg$predictinput()
View(abc_vrg$polypredi[[1]])
abc_vrg$plot_predict()
abc_vrg$lg_pred[3]
abc_vrg$plot_keyrat()
abc_vrg$lg_hist()
abc_vrg$lg_pred
abc_vrg$lg_keyr
abc_vrg$gridarrange()
# Report
abc_vrr <- Voterrollreport()
abc_vrr$htmlreport()
votr <- vrdf$listvbase[[2]]
#################################################################################################33
###### A) Fair ###
gsimf <- Countinggraphs(votr,selvar=c('R','C','a','b','c','d','probwd'))
gsimf$sortpre(selvar=c('x','y','alpha','lambda','zeta','probwd'))
gsimf$plot2d()
gsimf$plotxy()
gsimf$resplot()
gsimf$plotly3d()
gsimf$gridarrange()
vrdf$uploadvbase(gsimf$sdfc,gsimf$rdfc,gsimf$parameters$standard)
#################################################################################################33
##### B) Rigged ###
### Part A:
grigg <- Countinggraphs(votr)
grigg$sortpre()
grigg$manfolimp(pres1=grigg$quintile$x,
	       pres2=grigg$quintile$alpha-0.05,
	       pres3=c("x-alpha","(alpha*zeta + alpha - x)/zeta")[1])
grigg$riggsta()
gsimf$gridarrange()
vrdfg$uploadvbase(grig$sdfc,grig$rdfc,grig$parameters$standard)
#################################################################################################33
#################################################################################################33
#x <- rnorm(1000,0.5,0.075)
#y <- rnorm(100,0.5,0.075)
