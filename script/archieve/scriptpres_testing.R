options(scipen=999)
#library(ManifoldDestiny)
library(dplyr)
library(ggplot2)
library(htmltools)
library(gridExtra)
library(googlesheets4)
source(paste0(rprojroot::find_rstudio_root_file(),'/R/regressionanalysis.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/rsquare.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/olsncq.R'))
##########################################################################################################################################################
set_n <- c('k0+k1*y','k0+k1*x+k2*y','k0+k1*x+k2*y+k3*zeta')
set_h <- c('k0+k1*g','k0+k1*g+k2*h','k0+k1*g+k2*h+k3*zeta')
set_o <- c('k0+k1*n','k0+k1*n+k2*m','k0+k1*n+k2*m+k3*zeta')
##########################################################################################################################################################)
##########################################################################################################################################################
#s_1 <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1FxJg9hjU-M1MIeKl0koiHDVIp2dPAmm3nJpRzd5Ejdg/edit#gid=811418100',sheet='Bounded Tabulations', range='K5:N578')
bc <- 50
lm <- 0.05
um <- 0.95
mar_2022_bal <- ManifoldDestiny::mar_2022_sel %>% 
  dplyr::mutate(a=S) %>% 
  dplyr::mutate(b=T) %>% 
  dplyr::mutate(c=U) %>% 
  dplyr::mutate(d=V) %>%
  dplyr::mutate(R=as.numeric(Registered)) %>%
  dplyr::mutate(P=row_number()) %>%
  dplyr::arrange(desc(Registered)) %>%
  dplyr::filter(if_all(c(a,b,c,d), ~ . >= bc))

df1 <- Countinggraphs(mar_2022_bal)$sdfc %>%
	dplyr::filter(if_all(c(x,y,g,h,n,m,alpha,lamda,Omega), ~ . >= lm)) %>%
	dplyr::filter(if_all(c(x,y,g,h,n,m,alpha,lamda,Omega), ~ . <= um)) %>%
	dplyr::mutate(Psi_s=a/R) %>%
        dplyr::mutate(Psi_t=b/R) %>%
        dplyr::mutate(Psi_u=c/R) %>%
        dplyr::mutate(Psi_v=d/R) 

sel_3 <- 'k0+k1*x+k2*y'
app_3_sel <- list(alpha=sel_3,solvf='y',form=1)
app_3_eqs <- list(alpha=c(set_n,sel_3))
app_3_arp <- allreport(df1,app_3_sel,app_3_eqs)
app_3_srp <- sumreport(app_3_arp,1)
##################################3
# https://docs.google.com/document/d/1GfCZvMDNE6JnG5q5e5lewH9EiNQ7vzojcotiY7gdQOo/edit
sad <- s_1 %>%
  dplyr::mutate(P=row_number()) %>%
  dplyr::mutate(R=row_number()) %>%
  dplyr::mutate(a=S) %>% 
  dplyr::mutate(b=T) %>% 
  dplyr::mutate(c=U) %>% 
  dplyr::mutate(d=V) %>%
  Countinggraphs()

summary(lm(data=dplyr::select(sad$sdfc,alpha,x,y)))

View(sad)
a <- clipr::read_clip()
View(a)
summary(lm(data=dplyr::select(df1,alpha,x,y)))


fm <- 1
co <- Countinggraphs(df1)
co$sortpre(fm)
co$r2siminput(fm)
co$plot2d(fm)
co$plotxy(fm)
co$resplot(fm)
co$plotly3d(partition=fm)
co$gridarrange()

es <- Estimation(df1)
es$regression(regequ=list(alpha=set_n[2]))
es$regsum$mods
es$diagnostics()
es$resplots[[1]]





app_n_sel <- list(alpha=rev(set_n)[2],solvf='y',form=1)
app_n_eqs <- list(alpha=set_n)
app_n_arp <- allreport(df1,app_n_sel,app_n_eqs)
app_n_srp <- sumreport(app_n_arp,1)
app_n_srp[[5]]

mean(df1$alpha)

#library(googlesheets4)
#s_1 <- googlesheets4::read_sheet(xl_list[[1]], sheet=2)
#a <- clipr::read_clip()
#summary(lm(data=dplyr::select(co$sdfc,alpha,x,y)))


#ZX <- Xolsc(df1,z=c('alpha','g'),x=c('x','Psi'),y=c('y','Psi'))
#b <- beta_c(ZX[[1]],ZX[[2]])
#
#sel_n <- 'k0+k1*x+k2*y3'
#app_x_sel <- list(alpha='k0+k1*x+k2*y+k3*xy+k4*x**2+k5*y**2',solvf='y',form=1)
#app_x_eqs <- list(alpha=c(sel_n,app_x_sel[[1]]))
#app_x_cou <- Countinggraphs(df1)$sdfc %>% dplyr::mutate(xy=x*y)
#app_x_arp <- allreport(app_x_cou,app_x_sel,app_x_eqs)
#app_x_arp[[2]][[2]][[1]]$mods
#app_x_srp <- sumreport(app_x_arp,1)
#app_x_srp[[1]]
##https://docs.google.com/document/d/1TEt-ZqPkX0jcpFLoE6PAbCZh0s8kxFzcsKC_3f0dShk/edit#
##I: Database definitions
##########################################################################################################################################################)
##########################################################################################################################################################

was_2020_pre_sel <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/14Vl9VSmVqkSesg8qO7rgS7J4tkQEcszM4nq-N0f8h3g/edit#gid=1831440132', sheet=1, range='A2:J1023')
app_was_pre_2020 <- was_2020_pre_sel[,1:10]
names(app_was_pre_2020) <- c("P","PrecName","R","V","A","B","C","D","E","F")
#ManifoldDestiny::was_2020_pre_sel
#names(app_was_pre_2020) <- c("P","Precinct","R","B1","B2","B3","A1","A2","A3","Other.EDV","Other.Miv","Other.Ev")

app_was_pre_2020_bal <- app_was_pre_2020 %>%
	dplyr::mutate(a=A+E) %>% 
	dplyr::mutate(c=C) %>% 
	dplyr::mutate(b=B+F) %>% 
	dplyr::mutate(d=D) %>%
	dplyr::mutate(e=E) %>%
	dplyr::mutate(f=F) 

## II:Counting process
app_was_pre_2020_cou <- Countinggraphs(app_was_pre_2020_bal,selvar=c('P','V','R','a','b','c','d','e','f'))
app_was_pre_2020_cou$filtering(0)
mat <- app_was_pre_2020_cou$sdfc %>% dplyr::mutate(Psi_a_c_e=((a+c+e)/R),Psi_c=c/R,Psi_f=f/R) %>% dplyr::mutate(alpha2=alpha*alpha,h2=h*h,alphah=alpha*h)
View(mat)
es <- Estimation(mat)
es$regression(list(g='k1+k2*alpha+k3*h+k4*alpha2+k5*h2+k6*alphah'))
es$regsum$mods
  
  z <- dplyr::select(mat,g,Psi_a_c_e)
x <- dplyr::select(mat,g,Psi_a_c_e)
y <- dplyr::select(mat,g,Psi_a_c_e)


View(X)
## III: Reporting
lowbal <- 200
app_was_pre_2020_sel <- list(alpha=sel_o,solvf='y',form=2)
app_was_pre_2020_eqs <- list(alpha=c(reg_o,sel_o))
#app_was_pre_2020_eqs <- allreport(app_was_pre_2020_cou,app_was_pre_2020_sel,app_was_pre_2020_eqs,totv=lowbal)
#app_was_pre_2020_eqs[[2]][[3]][[2]]



print(beta.hat <- solve(Conj(t(X)) %*% X, Conj(t(X)) %*% z), digits=3)
print(beta.r <- coef(lm(Re(z) ~ Re(w) + Im(w))), digits=3)
print(beta.i <- coef(lm(Im(z) ~ Re(w) + Im(w))), digits=3)
#



app_was_pre_2020_srp <- sumreport(app_was_pre_2020_eqs,2)
app_was_pre_2020_srp[[4]][1]
##########################################################################################################################################################
# General procedure
#https://docs.google.com/document/d/13fVohcO0jdd4pgHbIzwbMTlPbZizuIrlVHtOjYopL4I/edit#heading=h.lty23tl914ct
#https://docs.google.com/spreadsheets/d/1T1U-VxgHLcCr6yfXk43YtpiQzvK4-o6jwk3MjqwsOGg/edit#gid=2074175427
## I:Database definitions
app_cla_2022 <- ManifoldDestiny::cla_2022_sel
names(app_cla_2022) <- c("C","P","R","A1","A2","A3","B1","B2","B3")
str(app_cla_2022)
app_cla_2022_bal <- app_cla_2022 %>% 
	dplyr::mutate(a=A1+A3) %>%
	dplyr::mutate(c=A2) %>%
	dplyr::mutate(b=B1+B3) %>%
	dplyr::mutate(d=B2)
## II:Counting process
app_cla_2022_cou <- Countinggraphs(app_cla_2022_bal)$sdfc %>% dplyr::mutate(gh=g*h,g2=g^2,h2=h^2,h3=h^3)
## III: Reporting
lowbal <- 0
app_cla_sel <- list(alpha='k0+k1*g+k2*h+k3*g**2+k4*h**2+k5*gh+k6h**3',solvf='y',form=2)
app_cla_eqs <- list(alpha=c(reg_o,app_cla_sel[[1]]))
app_cla_2022_arp <- allreport(app_cla_2022_cou,app_cla_sel,app_cla_eqs,totv=lowbal)
app_cla_2022_srp <- sumreport(app_cla_2022_arp,2)
app_cla_2022_srp[[5]]
##########################################################################################################################################################
##########################################################################################################################################################
###### Application 4: Trump vs. Biden
#sel_4 <- 'k0+k1*x+k2*y+k3*zeta'
#app_4_eqs <- list(alpha=c('k0+k1*y','k0+k1*x+k2*y','k0+k1*x+k2*y+k3*zeta',sel_3))
#app_4_sel <- list(alpha=sel_4,solvf='y',form=1)
#app_4_bal <- ManifoldDestiny::pres_clark_sel %>% dplyr::mutate(a=A2,b=B2,c=A3,d=B3)
#app_4_cou <- Countinggraphs(presdf)$sdfc %>% dplyr::mutate(gh=g*h,g2h=g^2*h)
#app_4_arp <- allreport(app_4_cou,app_4_sel,app_4_eqs)
#app_4_srp <- sumreport(app_4_arp)
###########################################################################################################################################################
############################################################################################################################################################
###### Application X1
##pre_2016_cla <- ManifoldDestiny::clarkp_2016_rec_sel %>% dplyr::mutate(a=E,b=F,c=A,d=B)
##pre_2016_cla_cou <- Countinggraphs(pre_2016_cla)$sdfc %>% dplyr::mutate(gh=g*h,g2h=g^2*h)
##pre_2016_cla_rep <- allreport(seldata=pre_2016_cla_cou,part=c(1,2,3),formulas=alstr_s[1:3])
###########################################################################################################################################################
###### Application 2X
##presdf_2008 <- ManifoldDestiny::nevadap_2008_rec_sel %>% dplyr::mutate(a=E,b=F,c=A,d=B)
##presdf_2008_co <- Countinggraphs(presdf_2008)$sdfc %>% dplyr::mutate(gh=g*h,g2h=g^2*h)
##rep_war <- allreport(seldata=presdf,part=c(1,2,3),eqreg=alstr_s[3],formulas=alstr_s[1:3])
###########################################################################################################################################################
###########################################################################################################################################################
###### I: Voterdatabase ###
##### Inititating
##ohio <-Voterdatabaseplots(type_nr=2,lsv=2)
##ohio$regvbase()
##ohio$scorecard()
##ohio$predictinput()
##ohio$plot_predict()
##ohio$plot_keyrat()
##ohio$plot_histio()
##ohio$gridarrange()
###cowplot::plot_grid(plotlist=ohio$lg_pred[[1]])
#### Report
##ohio <- Voterrollreport()
##ohio$regvbase()
##ohio$scorecard()
##ohio$predictinput()
########################################################################################################################################################
###library(ManifoldDestiny)
##library(dplyr)
##library(ggplot2)
##library(htmltools)
##library(gridExtra)
##source(paste0(rprojroot::find_rstudio_root_file(),'/R/regressionanalysis.R'))
##source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
##source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
##source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
##########################################################################################################################################################
##reg_form_abc <- c('alpha~h+g')
##reg_form_simr <- c('alpha~h+g','alpha ~ g+ I(g^2)+ gh+ I(h^2)+ I(g^3)+ g2h+ I(h^3)')
##reg_form_app1 <- c('alpha~h+g','alpha~h+g+Gamma','alpha ~ g+ I(g^2)+ gh+ I(h^2)+ I(g^3)+ g2h+ I(h^3)', 'alpha~y+x','alpha~h+g+Gamma')
##reg_form_app2 <- c('g~alpha',
##		   'g~alpha+Omega','g~Omega+Omega2+alphaOmega+Omega3+alphaOmega2+alpha2Omega+Omega3',
##		   'Psi~Lambda+Omega+betaLambda+OmegaLambda+Omega2','alpha~x,alpha~x+y','alpha~x+y+zeta')
##reg_form_app3 <- c('alpha~g','alpha~g+h','alpha~g+h+Gamma','alpha~x','alpha~x+y','alpha~x+y+xy+x2','alpha~x+y+zeta')
##reg_form_app5_ii <- c('alpha~y','alpha~y+x','alpha~y+x+zeta')
##reg_form_app4 <- c('alpha~g','alpha~g+h','alpha~g+h+Gamma')
##reg_form_app5_i <- c('alpha~rotv1xy','alpha~rotv1xy+rotv2xy')
##reg_form_app5_ii <- c('alpha~y','alpha~y+x','alpha~y+x+zeta')
##reg_form_sim_s <- c('alpha~x','alpha~y+x','alpha~y+x+zeta')
##########################################################################################################################################################
###reg_form_sim_o <- c('alpha~h','alpha~g+h','alpha~h+g+Gamma','alpha~g+I(g^2)+gh+I(h^2)+ I(g^3)+ g2h+ I(h^3)','alpha~y+x','alpha~h+g+Gamma')
##sq <- ManifoldDestiny::eqpar$meqs
###### Application 1: 
#### Stavros Miller
##mi_st <- ManifoldDestiny::clark_miller_stavros_sel
##mi_st_co <- Countinggraphs(mi_st)$sdfc %>% dplyr::mutate(gh=g*h,g2h=g^2*h)
##rep_stmi <- allreport(seldata=mi_st_co,eqreg='alpha~g+h',formulas=reg_form_app1,Pfr=0,Pfm=NULL,vfilter=list(a=0,b=0,c=0,d=0,V=0))
###cowplot::plot_grid(plotlist=rep_stmi[[1]][[2]][[5]],ncol=5)
##########################################################################################################################################################
##### Proto
##########################################################################################################################################################
###a <- "alpha~h+g"
###gsub("(~).*", "\\_hat", a)
##fulton_rec_sel <- ManifoldDestiny::fulton_rec_sel
##rep_fulton_i <-  fulton_rec_sel %>% dplyr::mutate(a=A,b=B,c=C,d=D,R=a+b+c+d)
##cou_foulton <- Countinggraphs(rep_fulton_i)
##cou_foulton$sortpre()
##cou_foulton$plot2d()
##cou_foulton$rdfc
##grad<- circular::rad(-45)
##seldata <- cou_foulton$rdfc %>% dplyr::mutate(rotv1xy=cos(grad)*x-sin(grad)*y,rotv2xy=sin(grad)*x+cos(grad)*y)
##es <- Estimation(seldata)
##es$regression(reg_form_app5_i[1])
##es$regsum[[2]]
##es$hat_predict()
##View(es$predict_df)
##av <- 0
##ful_rep <- allreport(seldata=cou_foulton,vfilter=list(a=av,b=av,c=av,d=av,V=av),part=c(1,2,3),formulas=reg_form_app4)
##ful_rep[[2]][[1]][[1]]$mods
###ful_rep[[2]][[2]][[1]]$mods
###ful_rep[[1]][[1]][[1]]
###ful_rep[[1]][[1]][[3]][[2]]
#### 2
##rep_fulton_ii <- fulton_rec_sel %>% dplyr::mutate(a=A+C,b=B+D,c=E,d=F,R=a+b+c+d)
##cou_foulton_ii <- Countinggraphs(rep_fulton_ii)$sdfc 
##ful_rep <- allreport(seldata=cou_foulton_ii,vfilter=list(a=av,b=av,c=av,d=av,V=av),part=c(1,2,3),formulas=reg_form_app4_ii)
##########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###### Application 4: Trump vs. Biden
##presdf <- ManifoldDestiny::pres_clark_sel %>% dplyr::mutate(a=A2,b=B2,c=A3,d=B3)
##abc <- Countinggraphs(presdf)$sdfc
###av <- 25
##wa <- allreport(seldata=abc,vfilter=list(a=av,b=av,c=av,d=av,V=0),part=c(1,2,3),formulas=reg_form_app4)
###wa[[1]][[2]][[1]]
##wa[[2]][[2]][[1]]$mods
##pres_clark_sel
###########################################################################################################################################################
###### Application 2: Gilbert, Sisolak, Lombardo, Clark and Washoe
##rec_gi_si_lo <- ManifoldDestiny::gov_sel 
##co_gi_si_lo  <- Countinggraphs(rec_gi_si_lo,selvar=names(rec_gi_si_lo))$rdfc %>% 
##	dplyr::mutate(Omega2=Omega*Omega) %>% 
##	dplyr::mutate(Omega3=Omega*Omega*Omega) %>% 
##	dplyr::mutate(alpha2=alpha*alpha) %>%
##	dplyr::mutate(alpha3=alpha*alpha*alpha) %>%
##	dplyr::mutate(alphaOmega=alpha*Omega) %>%
##	dplyr::mutate(alphaOmega2=alpha*Omega*Omega) %>%
##	dplyr::mutate(alpha2Omega=alpha*alpha*Omega) %>%
##	dplyr::mutate(Psi=(b+d)/R) %>%
##	dplyr::mutate(beta=c/R) %>%
##	dplyr::mutate(Lambda=d/R) %>%
##	dplyr::mutate(betaLambda=beta*Lambda) %>%
##	dplyr::mutate(OmegaLambda=Omega*Lambda) %>%
##	dplyr::mutate(gh=g*h,g2h=g^2*h) %>% dplyr::mutate(xy=x*y)
##es <- Estimation(co_gi_si_lo)
##es$regression(reg_form_abc[2])
##es$regsum$mods
##es$diagnostics()
##es$hat_predict()
##es$hatcompare()
###plot(x=es$compare$g,y=es$compare$g_hat)
##rep_gi_si_lo <- allreport(seldata=co_gi_si_lo,vfilter=list(a=0,b=0,c=0,d=0,V=0),part=c(1,2,3),formulas=reg_form_app1)
###########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##reg_form_abc <- c('g~Omega+Omega2+alpha*Omega+Omega3+alpha*Omega2+alpha2Omega+alpha3')
##ar_hiray <- ManifoldDestiny::arkansas_reynold_hill_sel %>% dplyr::mutate(P=row_number(Pre))
##ar_hiray_co <- Countinggraphs(ar_hiray)$sdfc %>% 
##	dplyr::mutate(Omega2=Omega*Omega) %>%
##	dplyr::mutate(alphaOmega=alpha*Omega) %>%
##	dplyr::mutate(Omega3=Omega*Omega*Omega) %>%
##	dplyr::mutate(alphaOmega2=alpha*Omega*Omega) %>%
##	dplyr::mutate(alpha2Omega=alpha*alpha*Omega) %>%
##	dplyr::mutate(alpha3=alpha*alpha*alpha) 
##re_ar_hiray_co <- allreport(seldata=ar_hiray_co,vfilter=list(a=0,b=0,c=0,d=0,V=30),part=c(1,2,3),formulas=reg_form_abc)
###re_ar_hiray_co[[1]][[2]][[1]]
##re_ar_hiray_co[[2]][[1]][[1]]$mods
### 
### Call:
### lm(formula = as.formula(regform), data = edfc)
### 
### Residuals:
###       Min        1Q    Median        3Q       Max 
### -0.046223 -0.006537  0.001722  0.007855  0.036840 
### 
### Coefficients:
###              Estimate Std. Error t value Pr(>|t|)    
### (Intercept)   0.28030    0.06163   4.548 9.09e-06 ***
### Omega        -1.40263    0.25381  -5.526 9.50e-08 ***
### Omega2        0.79550    0.41779   1.904   0.0582 .  
### alpha        -1.17440    0.20162  -5.825 2.09e-08 ***
### Omega3        0.54047    0.29845   1.811   0.0716 .  
### alpha2Omega  -5.36200    0.97669  -5.490 1.14e-07 ***
### alpha3        2.04844    0.37803   5.419 1.62e-07 ***
### Omega:alpha   8.22679    0.80033  10.279  < 2e-16 ***
### Omega2:alpha -3.21610    0.73041  -4.403 1.69e-05 ***
### ---
### Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### 
### Residual standard error: 0.01475 on 213 degrees of freedom
### Multiple R-squared:  0.9935,	Adjusted R-squared:  0.9933 
### F-statistic:  4071 on 8 and 213 DF,  p-value: < 2.2e-16
### 
###########################################################################################################################################################
##### Dr. Frank
##########################################################################################################################################################
#### Graphical
ohio_vrg <- Voterdatabaseplots()
ohio_vrg$listvbase[[1]]
ohio_vrg$scorecard()
ohio_vrg$predictinput()
ohio_vrg$plot_predict()
ohio_vrg$lg_pred[[3]][[1]]
ohio_vrg$plot_keyrat()
ohio_vrg$lg_keyr[[3]][[1]]
ohio_vrg$plot_histio()
ohio_vrg$lg_hist
ohio_vrg$gridarrange()
#### Report
ohio_vrr <- Voterrollreport()
ohio_vrr$htmlreport()
###
##########################################################################################################################################################
### Referances
##########################################################################################################################################################
#########################################################################################################################################################
#reg_form_norm <- c('alpha~x','alpha~y+x','alpha~y+x+zeta')
#reg_form_oppo <- c('alpha~h','alpha~h+g','alpha~h+g+Gamma')
#######################################################################################
