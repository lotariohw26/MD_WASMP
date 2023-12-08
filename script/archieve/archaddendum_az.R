#prepurge <- c(555:577,773,777)
#######################################################################################################################################################
options(scipen=999)
set.seed(1)
library(ManifoldDestiny)
library(dplyr)
library(ggplot2)
library(htmltools)
library(gridExtra)
source(paste0(rprojroot::find_rstudio_root_file(),'/R/realregression.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/r2simulation.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
reticulate::source_python(paste0(rprojroot::find_rstudio_root_file(),'/inst/script/sympy/functions.py'))
#abc = estmpoly()
##########################################################################################################################################################
set_n <- c('k0+k1*y','k0+k1*x+k2*y','k0+k1*x+k2*y+k3*zeta')
set_h <- c('k0+k1*g','k0+k1*g+k2*h','k0+k1*g+k2*h+k3*zeta')
set_o <- c('k0+k1*n','k0+k1*n+k2*m','k0+k1*n+k2*m+k3*zeta')
########################################################################################################################################################
lsa <- ManifoldDestiny::lst_race_snap_all
old_l <- names(lsa[[3]])
new_l <- c("snap","txt","P","PN","R","T","S","V","U")
snapdf <- lsa[[1]] %>% dplyr::mutate(snap=cumsum(!duplicated(txt))) %>% data.table::setnames(old=old_l,new=new_l) %>% dplyr::filter(snap==12)
View(snapdf)
co <- Countinggraphs(snapdf)
co$purging(totv=50)
co$rdfc
alr <- allreport(co$rdfc)
smr <- sumreport(alr)
smr[[2]]




