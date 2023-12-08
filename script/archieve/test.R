#############################################################################################################################################################
library(magick)
options(scipen=999)
set.seed(1)
library(ManifoldDestiny)
library(dplyr)
library(ggplot2)
library(htmltools)
#library(gridextra)
# Application
source(paste0(rprojroot::find_rstudio_root_file(),'/R/realregression.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/r2simulation.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/snapshotanalysis.R'))
set_n <- c('k0+k1*y','k0+k1*x+k2*y','k0+k1*x+k2*y+k3*zeta')
set_h <- c('k0+k1*g','k0+k1*g+k2*h','k0+k1*g+k2*h+k3*zeta')
set_o <- c('k0+k1*n','k0+k1*n+k2*m','k0+k1*n+k2*m+k3*zeta')
#############################################################################################################################################################
sel_ma <- 'k0+k1*x+k2*y'
app_ma_sel <- list(alpha=sel_ma,solvf='y',form=1)
app_ma_eqs <- list(alpha=c(set_n,sel_ma))
#############################################################################################################################################################
#### Maricopa
##### "US Senate","MASTERS, BLAKE","KELLY, MARK"
canlet <- c("SNAP","RACE", "RACENR","TXT","P","PN","R","A1","B1","A2","B2")
df_m1 <- data.table::rbindlist(ManifoldDestiny::lst_race_snap_all_az_ma,F)  %>% data.table::setnames(new=canlet) %>% dplyr::mutate(S=A1,T=B1,U=A2,V=B2) %>% dplyr::filter(RACENR%in%c(1,2)) 
snap_mar <- Snapshotgraphs(df_m1) 

## Static plots
sp <- snap_mar$plot2d()

## Oneplot
ssp <- unique(snap_mar$sdfcs$SNAP)
lr1 <- lapply(ssp,function(s){
  lgp <- snap_mar$plot2d(racenr=1,snap=s)
  fn <- paste0(rprojroot::find_rstudio_root_file(),"/inst/addendum/ggplots/race1/race_1_snap",s,".png")
  ggplot2::ggsave(filename=fn)
  lgp
})
fdir <- paste0(rprojroot::find_rstudio_root_file(),"/inst/addendum/ggplots/race1")
imgs <- base::dir(fdir, full.names=TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 1)
image_write(image = img_animated, path = paste0(fdir,'/race1.gif'))

lr2 <- lapply(ssp,function(s){
  lgp <- snap_mar$plot2d(racenr=2,snap=s)
  fn <- paste0(rprojroot::find_rstudio_root_file(),"/inst/addendum/ggplots/race2/race_2_snap",s,".png")
  ggplot2::ggsave(filename=fn)
  lgp
})
fdir <- paste0(rprojroot::find_rstudio_root_file(),"/inst/addendum/ggplots/race2")
imgs <- base::dir(fdir, full.names=TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 1)
image_write(image = img_animated, path = paste0(fdir,'/race2.gif'))

## Dynamics plots
dp <- snap_mar$plot2ddynamic_fac()

## Static
statdf <- df_m1 %>% dplyr::filter(TXT%in%prTXT[1],RACENR==1) 
df_mc <- Countingprocess(statdf)
df_mc$purging()
app_ma_sen_arp <- allreport(df_mc$rdfc)
app_ma_sen_srp <- sumreport(app_ma_sen_arp)
##############################################################################################################################################################
### Cohise
#### "US Senate","MASTERS, BLAKE","KELLY, MARK"
canlet <- c("SNAP","RACE","P","PN","R",do.call(c,lapply(c(1:3),function(x)paste0(LETTERS[x],seq(1,3)))))
df_co_all <- data.table::rbindlist(ManifoldDestiny::lst_race_snap_all_az_co,F) %>%
  dplyr::select(1:5,12:20) %>%
  data.table::setnames(new=canlet) %>%
  dplyr::mutate(S=A1,T=B1,U=A2,V=B2) %>%
  dplyr::select(SNAP,RACE,P,PN,R,S,T,U,V) %>%
  dplyr::mutate(RACENR=cumsum(!duplicated(RACE)), .after=SNAP)

# Particular race
df_cm <- Countingprocess(dplyr::filter(df_co_all,RACENR==1))
df_cm$purging(totv=0)
app_co_sen_arp <- allreport(df_cm$rdfc)
app_co_sen_srp <- sumreport(app_co_sen_arp)
# All races
sna_sta_all <- Snapshotgraphs(df_co_all)
plo_sta_all <- sna_sta_all$plot2dstatic_fac()
##############################################################################################################################################################
set.seed(123) # for reproducibility

# Define the number of voters in each precinct
voters_election_day <- rep(500, 1000)
voters_mail_in <- rep(250, 1000)

# Total number of voters in each precinct
voters <- voters_election_day + voters_mail_in

# Define the probability of voting for each candidate in each precinct (randomly generated)
p_A_election_day <- runif(1000, 0.4, 0.7)
p_A_mail_in <- runif(1000, 0.3, 0.6)
p_A <- (p_A_election_day * voters_election_day + p_A_mail_in * voters_mail_in) / voters
p_B <- 1 - p_A

# Generate sample votes for each candidate in each precinct
votes_A_election_day <- sapply(voters_election_day, function(n) rbinom(1, size = n, prob = p_A_election_day))
votes_A_mail_in <- sapply(voters_mail_in, function(n) rbinom(1, size = n, prob = p_A_mail_in))
votes_A <- votes_A_election_day + votes_A_mail_in

votes_B_election_day <- sapply(voters_election_day, function(n) rbinom(1, size = n, prob = p_B_election_day))
votes_B_mail_in <- sapply(voters_mail_in, function(n) rbinom(1, size = n, prob = p_B_mail_in))
votes_B <- votes_B_election_day + votes_B_mail_in

# Combine the sample votes into a data frame
df <- data.frame(Precinct = 1:1000,
                 Candidate_A = votes_A,
                 Candidate_B = votes_B,
                 Total_Votes = voters,
                 Election_Day_Votes = voters_election_day,
                 Mail_In_Votes = voters_mail_in)
