######################################################################################################################
#estmpoly(expr=10*x**3+3*x**2+k0*2*x**2+y,solvf=y)
#estmpoly(expr='10*x**3+3*x**2+k0*2*x**2+y',solvf='y')
#########################################################################################################################
# import json
# import sympy
# import numpy
# import sys
# from sympy import solve, Eq, symbols, latex, simplify, diff
# import pandas
# import os
# import rpy2
# import rpy2.robjects as robjects
# #abs_path = rroot.find_rstudio_root_file()[0]
# #csvfile=abs_path+'/inst/script/sympy/csv/parameters.csv'
# #readmodvar = pandas.read_csv(csvfile, sep=',') 
# sympy.var(readmodvar.iloc[:,0])
## Cleaning namespace
#for name in dir():
#  if not name.startswith('_'):
#      del globals()[name]
## Import packages
#from rpy2.robjects.packages import importr, data
#rroot=importr('rprojroot')
#abs_path = rroot.find_rstudio_root_file()[0]
#csvfile=abs_path+'/inst/script/sympy/csv/parameters.csv'
#readmodvar = pandas.read_csv(csvfile, sep=',') 
#sympy.var(readmodvar.iloc[:,0])
#beforems = set(dir())
#########################################################################################################################
##########################################################################################################################
#### Regression model
#alpha_f_s_p1= [Eq(alpha,k0+k1*y),Eq(alpha,k0+k1*x+k2*y),Eq(alpha,k0+k1*x+k2*y+k3*zeta)]
#alpha_f_s_p2= [Eq(alpha,k0+k1*x+k2*xy+k3*x**2+k4*y+k5*y**2)]
#alpha_f_s_p3= [Eq(alpha,k2*x**2+k1*x+k0)]
#alpha_f_s_p4= [Eq(alpha,k2*x**2+k1*x+k0)]
#alpha_f_h_p1= [Eq(alpha,k0+k1*g),Eq(alpha,k0+k1*g+k2*h),Eq(alpha,k0+k1*g+k2*h+k3*Gamma)]
#alpha_f_h_p3= [Eq(alpha,k0+k1*g+k2*g**2+k3*gh+k4*h**2+k5*g**3+k6*g2h+k7*h**3)]
#alpha_f_o_p1= [Eq(alpha,k0+k1*m),Eq(alpha,k0+k1*n+k2*m),Eq(alpha,k0+k1*n+k2*m+k3*xi)]
#########################################################################################################################
#### Solved for 
#y_sol_p1 = [Eq(y,solve(alpha_f_s_p1[0],y)[0]),Eq(y,solve(alpha_f_s_p1[1],y)[0]),Eq(y,solve(alpha_f_s_p1[2],y)[0])]
##g_sol_p1 = [Eq(g,solve(alpha_f_h_p1[0],y)[0])]
##n_sol_p1 = [Eq(n,solve(alpha_f_o_p1[1],y)[0])]
##########################################################################################################################
#### Polynomial
##from sympy import poly
##peq=Eq(alpha,k0+k1*x+k2*y+k3*zeta)
##peq2=Eq(alpha,k0+k1*g+k2*g**2+k3*gh+k4*h**2+k5*g**3+k6*g2h+k7*h**3)
##pol = poly(peq, x) 
##pol2 = poly(peq2, g) 
##y_sol_p1=pol.all_coeffs()
##g_sol_p1=pol2.all_coeffs()
###########################################################################################################################
#afterms = set(dir())
#modvarlist = list(afterms - beforems)
#modvarlist.remove('beforems')
#modvarlist
#modrqs = dict()
#modrql = dict()
#dfs = [] 
#dfl = [] 
#parv = 'empty'
#for i in range(0,len(modvarlist)):
#  modrqs[parv] = dfs
#  modrql[parv] = dfl
#  parv = modvarlist[i]
#  dfs = [] 
#  dfl = [] 
#  nid = len(eval(modvarlist[i])) 
#  for j in range(0,nid):
#    dfs.append(str(eval(modvarlist[i])[j].rhs))
#    dfl.append(latex(eval(modvarlist[i])[j].rhs))
#modrqs[parv] = dfs
#modrql[parv] = dfl
#modrqs.pop('empty')
#modrql.pop('empty')
############################################################################################################################
##reg_form_abc <- c('alpha~h+g')
##reg_form_simr <- c('alpha~h+g','alpha ~ g+ I(g^2)+ gh+ I(h^2)+ I(g^3)+ g2h+ I(h^3)')
##reg_form_app1 <- c('alpha~h+g','alpha~h+g+Gamma','alpha~ g+ I(g^2)+ gh+ I(h^2)+ I(g^3)+ g2h+ I(h^3)', 'alpha~y+x','alpha~h+g+Gamma')
##reg_form_app2 <- c('g~alpha','g~alpha+Omega','g~Omega+Omega2+alphaOmega+Omega3+alphaOmega2+alpha2Omega+Omega3','Psi~Lambda+Omega+betaLambda+OmegaLambda+Omega2','alpha~x,alpha~x+y','alpha~x+y+zeta')
##reg_form_app3 <- c('alpha~g','alpha~g+h','alpha~g+h+Gamma','alpha~x','alpha~x+y','alpha~x+y+xy+x2','alpha~x+y+zeta')
##reg_form_app5_ii <- c('alpha~y','alpha~y+x','alpha~y+x+zeta')
##reg_form_app4 <- c('alpha~g','alpha~g+h','alpha~g+h+Gamma')
##reg_form_app5_i <- c('alpha~rotv1xy','alpha~rotv1xy+rotv2xy')
##reg_form_app5_ii <- c('alpha~y','alpha~y+x','alpha~y+x+zeta')
##reg_form_sim_s <- c('alpha~x','alpha~y+x','alpha~y+x+zeta')
#v <- function(){
#    open_command <- switch(Sys.info()[['sysname']],
#                           Windows= 'open',
#                           Linux  = 'xdg-open',
#                           Darwin = 'open')
#
#    #temp_file <- paste0('tmp/abc', '.xlsx')
#    temp_file <- paste0(tempfile(), '.xlsx')
#    df <-  clipr::write_last_clip()
#    View(df)
#}
#
##' @export
#magic <- function(zeta=1,lambda=0.4,x=seq(0,1,by=0.01),select=c('t1','b1','rl','t2','b2','a2','ab')){
#
#  # evaluation
#  t1 <- eval(parse(text='x'),list(x=x)) # Predetermined
#  b1 <- eval(parse(text='1-x'),list(x=x))  # Predetermined
#  rl <- eval(parse(text='lambda'),c(list(lambda=lambda),list(x=x))) # Enforced form hybrid
#  t2 <- rl-(b1-rl)/zeta # Combination
#  b2 <- 1-t2
#  at <- (t1+t2*zeta)/(zeta+1)
#  ab <- (b1+b2*zeta)/(zeta+1)
#
#  # Plot
#  df <- data.frame(x,t1,b1,rl,t2,b2,at,ab)
#  dfs <- df %>% tidyr::pivot_longer(c(t1,b1,rl,t2,b2,at,ab)) %>% subset(name%in%select)
#  plot <- ggplot2::ggplot(data=dfs,ggplot2::aes(x=x,y=value,color=name)) + ggplot2::geom_point() + ggplot2::geom_line() + theme_classic()
#  ## output
#  list(df=df,dfv=df[],plot=plot)
#}
#
##' @export
#magic2 <- function(zeta=seq(0.2,2,0.01),lambda=0.3,alpha_b=0.55,selectv=NA){
#
#  # evaluation
#  gpn <- eval(parse(text='(lambda*(1+zeta)+zeta-alpha_b*(1+zeta))/(2*zeta)'),
#	      c(list(lambda=lambda,alpha_b=alpha_b),list(zeta=zeta)))
#  gp <- 1- gpn
#  fp <- lambda*(zeta+1) - zeta*gpn
#  fpn <- 1- fp
#  alpha_a <- 1-alpha_b
#  df <- data.frame(gpn,gp,fp,fpn,alpha_b,zeta,lambda,alpha_a)
#  dfs <- df %>% tidyr::pivot_longer(c(gpn,gp,fp,fpn,alpha_a,lambda,alpha_b))## %>% dplyr::select(ifelse(select==NA,everything(),select))
#  plot <- ggplot(data=dfs,aes(x=zeta,y=value,color=name)) + geom_point() + geom_line() + theme_classic()
#  list(df=df,dfv=df[],plot=plot)
#}
#
#' @export
ratio <- function(n=NULL,d=null){
	rat <- ifelse(d==0,0.5,n/d)
}
#
#aggordered <- function(dfc=NULL,polynr=6,arrangevar='x'){
#
#    names(dfc) <- c("pre","a","b","c","d","x","y","alpha","Lambda","zeta")
#    dgpsummed <- dfc %>% dplyr::arrange(arrangevar) %>%
#    dplyr::mutate(pre_ind=dplyr::row_number(alpha)/length(alpha)) %>%
#    dplyr::mutate(pred1=predict(lm(alpha ~ poly(pre_ind,polynr, raw=T), data=.))) %>%
#    dplyr::mutate(pred2=predict(lm(x ~ poly(pre_ind,polynr, raw=T), data=.))) %>%
#    dplyr::mutate(pred3=predict(lm(y ~ poly(pre_ind,polynr, raw=T), data=.))) %>%
#    #  Creating new variables
#    #dplyr::select(-model1,-model2,-model3) %>%
#    # Residuals
#    dplyr::mutate(res_x=x-pred1) %>%
#    dplyr::mutate(res_y=y-pred2) %>%
#    dplyr::mutate(res_alpha=alpha-pred3) %>%
#    dplyr::mutate(res_zeta=zeta-mean(zeta)) %>%
#    # Crossres
#    dplyr::mutate(crossres_x=res_zeta*res_x) %>%
#    dplyr::mutate(crossres_y=res_zeta*res_y) %>%
#    dplyr::mutate(crossres_alpha=res_zeta*res_alpha) %>%
#    tidyr::pivot_longer(c(x,y,alpha,pred1,pred2,pred3))
#}
#break1 = function(X) {
#        do.call(c, lapply(X, function(x) { c(Re(x), Im(x)) }))
#}
#break2 = function(X) {
#        do.call(c, lapply(X, function(x) { c(-Im(x), Re(x)) }))
#}
fit.complex = function(Y, X.List) {
        # Split into real variables
        YF = break1(Y)
        XF.List = do.call(c, lapply(X.List,
                function(x) { list(break1(x), break2(x)) } ))
        # Make the data.fram
        Data = data.frame(Y = YF)
        X.Names = paste('X', 1:length(XF.List), sep='')
        for (N in seq_along(XF.List)) {
                Data[[ X.Names[[N]] ]] = XF.List[[N]]
        }
        # Formula + Model
        Formula = paste("Y ~ ", paste(X.Names, collapse='+'), "-1")
        Model = lm(as.formula(Formula), data=Data)
        # Make them complex again
        Coeffs = sapply(seq_along(X.List),
                function(N) {
                        ( Model$coefficients[[ X.Names[[2*N-1]] ]]
                        + Model$coefficients[[ X.Names[[2*N]] ]]*1i )
                })
        names(Coeffs) = names(X.List)
        Model$coefficients.complex = Coeffs
        Model
}
#Beta0 = 1 + 3i
#Beta1 = 3 - 2i
##Re(Beta0)
##Im(Beta0)
#X = runif(3, 0, 10)
#Beta0 + Beta1*X
#Y = (Beta0 + Beta1*X + rnorm(length(X), 0, 0.7) * exp(1i*runif(length(X), 0, 2*pi)))
#Model = fit.complex(Y, list( const = 0*X+1,linear = X))
##Beta0.Est = Model$coefficients.complex[[1]]
##Beta1.Est = Model$coefficients.complex[[2]]
#source_lines <- function(file, lines){source(textConnection(readLines(file)[lines]))}

# Today
#######################################################################################
break1 = function(X) {
        do.call(c, lapply(X, function(x) { c(Re(x), Im(x)) }))
}
break2 = function(X) {
        do.call(c, lapply(X, function(x) { c(-Im(x), Re(x)) }))
}
fit.complex = function(Y, X.List) {
        # Split into real variables
        YF = break1(Y)
        XF.List = do.call(c, lapply(X.List,
                function(x) { list(break1(x), break2(x)) } ))
        # Make the data.fram
        Data = data.frame(Y = YF)
        X.Names = paste('X', 1:length(XF.List), sep='')
        for (N in seq_along(XF.List)) {
		# N <- 4
                Data[[ X.Names[[N]] ]] = XF.List[[N]]
		print(Data)
        }
        # Formula + Model
        Formula = paste("Y ~ ", paste(X.Names, collapse='+'), "-1")
	browser()
        Model = lm(as.formula(Formula), data=Data)
        # Make them complex again
        Coeffs = sapply(seq_along(X.List),
                function(N) {
			# N <- 2
                        ( Model$coefficients[[ X.Names[[2*N-1]] ]]
                        + Model$coefficients[[ X.Names[[2*N]] ]]*1i )
                })
        names(Coeffs) = names(X.List)
        Model$coefficients.complex = Coeffs
        Model
}
Beta0 = 1 + 3i
Beta1 = 3 - 2i
X = runif(15, 0, 10)
X
Y = (Beta0 + Beta1*X + rnorm(length(X), 0, 0.7) * exp(1i*runif(length(X), 0, 2*pi)))
Model = fit.complex(Y, list( const = 0*X+1,linear = X))
Beta0.Est = Model$coefficients.complex[[1]]
Beta1.Est = Model$coefficients.complex[[2]]

Model[[4]]


nevada_2020 <- 'https://docs.google.com/spreadsheets/d/14Vl9VSmVqkSesg8qO7rgS7J4tkQEcszM4nq-N0f8h3g/edit#gid=0'
#https://docs.google.com/spreadsheets/d/14Vl9VSmVqkSesg8qO7rgS7J4tkQEcszM4nq-N0f8h3g/edit#gid=0

nevada_2020_g <- googlesheets4::read_sheet(nevada_2020, sheet='Z Complex, All',range='A2:J1288')
names(nevada_2020_g) <- c("P","Pn","R","nmr","A1","B1","A2","B2","A3","B3")
nevada_2020_sel <- nevada_2020_g %>% dplyr::select(-2) %>%
	dplyr::mutate(a=A1+A3) %>%
	dplyr::mutate(b=B1+B3) %>%
	dplyr::mutate(c=A2) %>%
	dplyr::mutate(d=B2) %>%
        dplyr::mutate(Psi_ace=(A1+A2+A3)/R) %>%
        dplyr::mutate(Psi_c=A2/R) %>%
        dplyr::mutate(Psi_f=B3/R) %>%
	Countinggraphs(selvar=names(.)) 

sapply(dplyr::select(nevada_2020_sel$sdfc,Psi_ace,g,h,Psi_c,alpha,Psi_f), mean)
delv <- c(0.3509776817,0.4795525403,0.4115875626,0.1157344416,0.4538313291,0.03226082851)

nvs <- nevada_2020_sel$sdfc %>% 
        dplyr::mutate(zr=Psi_ace-delv[1]) %>%
        dplyr::mutate(zi=g-delv[2]) %>%
        dplyr::mutate(xr=h-delv[3]) %>%
	dplyr::mutate(xi=Psi_c-delv[4]) %>%
        dplyr::mutate(yr=alpha-delv[5]) %>%
        dplyr::mutate(yi=Psi_f-delv[6]) 

abc <- Cestimation(nvs)
abc$cregression()

olsce(dplyr::select(nvs,all_of(inpn)))




nevada_2020_g2 <- googlesheets4::read_sheet(nevada_2020, sheet='Z Complex, All',range='Q1:V1288')[-1,]
inpn <- c("zr","zi","xr","xi","yr","yi")
names(nevada_2020_g2) <- inpn
ce <- Cestimation(nevada_2020_g2)
c <- olsce(nevada_2020_g2)
########################################################################################################################################################
## Part
govdf <- ManifoldDestiny::gov_sel 
go <- Countinggraphs(govdf,selvar=names(govdf))
goext <- go$sdfc %>% dplyr::mutate(Psi=(a+b+c+d)/R, 
				   Psi_d=(A1+A2+C2+C3)/R, 
				   Psi_b1=b/R,
				   Psi_c1=c/R,
				   Psi_b2=Psi_b1*Psi_b1,
				   Psi_c2=Psi_c1*Psi_c1, 
				   Psi_b3=Psi_b1*Psi_b1*Psi_b1,
				   Psi_c3=Psi_c1*Psi_c1*Psi_c1, 
				   Psi_b4=Psi_b1*Psi_b1*Psi_b1*Psi_b1,
				   Psi_c4=Psi_c1*Psi_c1*Psi_c1*Psi_c1, 
				   alpha2=alpha*alpha, 
                                   alphaPsi_b1=alpha*Psi_b1, 
                                   alphaPsi_c1=alpha*Psi_c1)

X <- Xolsc(sdfce=goext,z=c('g','Psi_ci'),x=c('h','Psi_ci'),y=c('alpha','Psi_ci'),dg=3)
b <- beta_c(z=X[[1]],X=X[[2]])
# x0y0  0.0099445+0.1620291i
# yi    1.5351111-0.0995473i
# xi   -0.5528927-0.0966871i



print(beta.hat <- solve(Conj(t(X)) %*% X, Conj(t(X)) %*% z), digits=3)

## Part II: Complex part
pr <- dim(goext)[1]
zi <- complex(real=goext$g,imaginary=goext$Psi_d)
### Information
x0 <- complex(real=rep(1,pr),imaginary=rep(0,pr))
y0 <- complex(real=rep(1,pr),imaginary=rep(0,pr))
xi <- complex(real=goext$alpha,imaginary=goext$Psi_b1)
yi <- complex(real=goext$h,imaginary=goext$Psi_c1)
### Non-new information
x0y0 <- complex(real=Re(x0)*Re(y0),imaginary=Im(x0)*Im(y0))
x1y1 <- complex(real=Re(xi)*Re(yi)-Im(xi)*Im(yi),imaginary=Re(xi)*Im(yi)-Im(xi)*Re(yi))

x0y1 <- complex(real=Re(x0)*Re(yi),imaginary=Re(x0)*Im(yi))
x0y2 <- complex(real=Re(x0)*Re(yi^2),imaginary=Re(x0)*Im(yi^2))
x0y3 <- complex(real=Re(x0)*Re(yi^3),imaginary=Re(x0)*Im(yi^3))

x1y0 <- complex(real=Re(xi)*Re(y0),imaginary=Im(xi)*Re(y0))
x2y0 <- complex(real=Re(xi^2)*Re(y0),imaginary=Im(xi^2)*Re(y0))
x3y0 <- complex(real=Re(xi^3)*Re(y0),imaginary=Im(xi^3)*Re(y0))

x1y2 <- complex(real=Re(xi)*Re(yi^2)-Im(xi)*Im(yi^2),imaginary=Re(xi)*Im(yi^2)-Im(xi)*Re(yi^2))
x2y1 <- complex(real=Re(xi^2)*Re(yi)-Im(xi^2)*Im(yi),imaginary=Re(xi^2)*Im(yi)-Im(xi^2)*Re(yi))

X <- as.matrix(data.frame(x0y0,yi,xi,x0y2,x1y1,x2y0,x0y3,x1y2,x2y1))
X
#! last one
print(beta.hat <- solve(Conj(t(X)) %*% X, Conj(t(X)) %*% z), digits=3)
beta.hat
#######################################################################################
########################################################################################################################################################
## Nevada
https://docs.google.com/spreadsheets/d/14Vl9VSmVqkSesg8qO7rgS7J4tkQEcszM4nq-N0f8h3g/edit#gid=1442024031
https://docs.google.com/document/d/1TEt-ZqPkX0jcpFLoE6PAbCZh0s8kxFzcsKC_3f0dShk/edit
bal <- ManifoldDestiny::nevada_sel %>% dplyr::mutate(Psi_ace=(a+c+e)/R,Psi_c=c/R,Psi_f=f/R) 
cou <- Countinggraphs(bal,names(bal))$sdfc %>% dplyr::mutate(alpha2=alpha*alpha,h2=h*h,alphah=alpha*h)
deltav <- apply(dplyr::select(cou,Psi_ace,g,h,Psi_c,alpha,Psi_f),2,mean)
est <- Estimation(cou)
est$regression(list(g='k0+k1*alpha+k2*h'))
#est$regsum$mods
#
#
#
#
#
#
pso <- psi %>% dplyr::mutate(Psi_ace=(a+c+e)/R,Psi_c=c/R,Psi_f=f/R) %>% dplyr::select(P,Psi_ace,Psi_c,Psi_f)
#
#
#
#
#
#
########################################################################################################################################################
pso <- psi %>% dplyr::mutate(Psi_ace=(a+c+e)/R,Psi_c=c/R,Psi_f=f/R) %>% dplyr::select(P,Psi_ace,Psi_c,Psi_f)
ne_st <- psi %>% dplyr::mutate(a=a+e,b=b+f)
co <- Countinggraphs(ne_st)
View(co$sdfc)
names(co$sdfc)
pr1024 <- as.numeric(clipr::read_clip())
ne_st_ad <- co$sdfc %>% dplyr::mutate(alpha2=alpha*alpha,g2=g*g,alphag=alpha*g) %>% dplyr::left_join(pso,by='P') #   Psi_ace         g         h     Psi_c     alpha     Psi_f %>% dplyr::filter(P%in%pr1024)
#   Psi_ace         g         h     Psi_c     alpha     Psi_f 
deltav <- apply(dplyr::select(ne_st_ad,Psi_ace,g,h,Psi_c,alpha,Psi_f),2,mean)
#   Psi_ace         g         h     Psi_c     alpha     Psi_f 
# 0.3457351 0.4734162 0.4117906 0.1152805 0.4499783 0.0329626 
##deltavcb <- c(0.3509776817,0.4795525403,0.4115875626,0.1157344416,0.4538313291,0.03226082851)
zdataframe <- ne_st_ad %>% dplyr::select(Psi_f,Psi_ace,g,h,Psi_c,alpha) %>% dplyr::mutate(zor=Psi_ace-deltav[1],zoi=g-deltav[2],xir=h-deltav[3],xii=Psi_c-deltav[4],yir=alpha-deltav[5],yii=Psi_f-deltav[6]) 
pr <- dim(zdataframe)[1]
zo <- complex(real=zdataframe$zor,imaginary=zdataframe$zoi)
x0 <- complex(real=rep(1,pr),imaginary=rep(0,pr))
y0 <- complex(real=rep(1,pr),imaginary=rep(0,pr))
xi <- complex(real=zdataframe$xir,imaginary=zdataframe$xii)
yi <- complex(real=zdataframe$yir,imaginary=zdataframe$yii)
x0y0 <- complex(real=re(x0)*re(y0),imaginary=im(x0)*im(y0))
x0y1 <- complex(real=Re(x0)*Re(yi),imaginary=Re(x0)*Im(yi))
x1y0 <- complex(real=Re(xi)*Re(y0),imaginary=Im(xi)*Re(y0))
x0y2 <- complex(real=Re(x0)*Re(yi^2),imaginary=Re(x0)*Im(yi^2))
x1y1 <- complex(real=Re(xi)*Re(yi)-Im(xi)*Im(yi),imaginary=Re(xi)*Im(yi)+Im(xi)*Re(yi))
x2y0 <- complex(real=Re(xi^2)*Re(y0),imaginary=Im(xi^2)*Re(y0))
z <- zo
#x <- as.matrix(data.frame(x0y0,x0y1,x1y0,x0y2,x1y1,x2y0))
X <- as.matrix(data.frame(x0y0,yi,xi))
#X <- as.matrix(data.frame(x0y0,yi,xi,x0y2,x2y0,x1y1))
print(beta.hat <- solve(Conj(t(X)) %*% X, Conj(t(X)) %*% z), digits=3)
beta.hat
########################################################################################################################################################
# [1,] -0.00498579+0.000301922i
# [2,]  0.76207694+1.582677962i
# [3,] -0.09547085-0.517859951i
# [4,]  0.52971201-0.471202804i
# [5,]  0.00659995+0.124282804i
# [6,]  0.12218630+0.318259560i
#-0.005693794321159	0.000763439988202552	
#0.816255513568808	1.58764312942543	
#-0.140264148421178	-0.498357855155589	
#0.513978651269852	-0.582940600960614	
#0.0261057978154113	0.421921936572005	
#0.113403802790825	0.0164414984815799
## Alternative 2
#zv <- do.call(c, lapply(z, function(x) { c(Re(x), Im(x)) }))
#Data <- 
#  data.frame(ind=seq(1,length(z))) %>% 
#  dplyr::bind_cols(X) %>% 
#  dplyr::select(-1) %>%
#  sapply(rep.int,times=2) %>%
#  apply(2,function(x){
#  data.frame(r=rep(c(0,1),length(z)/2)*Re(x),i=rep(c(1,0),length(z)/2)*Im(x))
#  }) %>% as.data.frame() %>%
#  dplyr::mutate(z=zv) %>%
#  dplyr::relocate(z) 
#View(Data)
#X.Names <- paste('X', 1:(length(Data)-1), sep='')
#names(Data) <- c("Y",X.Names)
#Formula = paste("Y ~ ", paste(X.Names, collapse='+'), "-1")
#Model = lm(as.formula(Formula), data=Data)
########################################################################################
feq <- 'k0+k1*x+k2*y+k3*zeta'
app_1_eqs <- list(alpha=c('k0+k1*y','k0+k1*x+k2*y','k0+k1*x+k2*y+k3*zeta'))
app_1_sel <- list(alpha='k0+k1*x+k2*y',solvf='y',form=1)
az_st <- ManifoldDestiny::lake_sel
names(az_st) <- c("P","R","a","B","C","b","E","c","G","H","d","J")
co <- Countinggraphs(az_st)$sdfc %>% dplyr::mutate(x2=x*x,y2=y*y,xy=x*y)
alr <- allreport(arz_cou,app_1_sel,app_1_eqs,50)
srp  <- sumreport(alr)
##########################################################################################################################################################
co <- Countinggraphs(seldata_fil)
  co$sortpre(x)
  co$r2siminput(x)
  co$plot2d(x)
  co$plotxy(x)
  co$resplot(x)
  co$plotly3d(partition=x)
  co$gridarrange()
  co$all_pl_3dmani
#######################################################################################
# Libraries
library(googlesheets4)
library(ManifoldDestiny)
library(dplyr)
library(ggplot2)
library(htmltools)
library(broom)
library(huxtable)
library(dplyr)
library(htmltools)
library(gridExtra)
library("googlesheets4")
abs_p <- rprojroot::find_rstudio_root_file()
source(paste0(abs_p,'/R/voterrollanalysis.R'))
source(paste0(abs_p,'/R/countingprocess.R'))
source(paste0(abs_p,'/R/regressionanalysis.R'))
source(paste0(abs_p,'/R/misc.R'))
options(scipen=999)
set.seed(1)
#######################################################################################
### Formulas ###
reg_form_norm1 <- c('alpha~x','alpha~y+x','alpha~y+x+zeta')
reg_form_oppo1 <- c('alpha~h','alpha~h+g','alpha~h+g+Gamma')
reg_form_oppo2 <- c('alpha~h+g','alpha~ g+ I(g^2)+ gh+ I(h^2)+ I(g^3)+ g2h+ I(h^3)','alpha~y+x','alpha~h+g+Gamma')
reg_form_oppo3 <- c('g~h+alpha','g~Psi_b1+Psi_c1+alpha+Psi_b2+Psi_c2+alphaPsi_b1+alphaPsi_c1+Psi_b3+Psi_b4','Psi_d~Psi_b1+Psi_c1+alpha+alpha*Psi_c1+alpha2')
reg_form_oppo4 <- c('g~alpha','g~alpha+Omega','g~Omega+Omega2+alphaOmega+Omega3+alphaOmega2+alpha2Omega+Omega3',
		    'Psi~Lambda+Omega+betaLambda+OmegaLambda+Omega2')
#######################################################################################
#######################################################################################
## Arkanas
rec_re_hi <- ManifoldDestiny::arkansas_reynold_hill_sel %>% dplyr::mutate(P=row_number())  
co_re_hi <- Countinggraphs(rec_re_hi,vfilter=list(a=0,b=0,c=0,d=0,V=30))$sdfc %>% 
	dplyr::mutate(Omega2=Omega*Omega) %>% 
	dplyr::mutate(Omega3=Omega*Omega*Omega) %>% 
	dplyr::mutate(alpha2=alpha*alpha) %>%
	dplyr::mutate(alpha3=alpha*alpha*alpha) %>%
	dplyr::mutate(alphaOmega=alpha*Omega) %>%
	dplyr::mutate(alphaOmega2=alpha*Omega*Omega) %>%
	dplyr::mutate(alpha2Omega=alpha*alpha*Omega) %>%
	dplyr::mutate(Psi=(b+d)/R) %>%
	dplyr::mutate(beta=c/R) %>%
	dplyr::mutate(Lambda=d/R) %>%
	dplyr::mutate(betaLambda=beta*Lambda) %>%
	dplyr::mutate(OmegaLambda=Omega*Lambda)
dim(co_re_hi)
ar_re_hi <- allreport(seldata=co_re_hi,vfilter=list(a=0,b=0,c=0,d=0,V=30),part=c(1,2,3),formulas=reg_form_oppo4)
ar_re_hi[[1]][[2]][[1]]
ar_re_hi[[2]][[4]][[1]]$mods
#######################################################################################
## Stavros Miller
rec_mi_st <- ManifoldDestiny::clark_miller_stavros_sel
rep_stmi <- allreport(seldata=rec_mi_st,vfilter=list(a=0,b=0,c=0,d=0),part=c(1,2,3),formulas=reg_form_oppo2)






## Part
govdf <- ManifoldDestiny::gov_sel 
go <- Countinggraphs(govdf,selvar=names(govdf))
goext <- go$sdfc %>% dplyr::mutate(Psi=(a+b+c+d)/R, 
				   Psi_d=(A1+A2+C2+C3)/R, 
				   Psi_b1=b/R,
				   Psi_c1=c/R,
				   Psi_b2=Psi_b1*Psi_b1,
				   Psi_c2=Psi_c1*Psi_c1, 
				   Psi_b3=Psi_b1*Psi_b1*Psi_b1,
				   Psi_c3=Psi_c1*Psi_c1*Psi_c1, 
				   Psi_b4=Psi_b1*Psi_b1*Psi_b1*Psi_b1,
				   Psi_c4=Psi_c1*Psi_c1*Psi_c1*Psi_c1, 
				   alpha2=alpha*alpha, 
                                   alphaPsi_b1=alpha*Psi_b1, 
                                   alphaPsi_c1=alpha*Psi_c1)

#View(goext)
#v1 <- goext$alpha+goext$Gamma*(goext$alpha-goext$h)
#v2 <- goext$g
#View(data.frame(v1,v2))
govdf_rep <- allreport(seldata=goext,vfilter=list(a=0,b=0,c=0,d=0),part=c(1,2,3),formulas=reg_form_oppo2)
govdf_rep[['est']][[3]][[1]]$mods
govdf_rep[['cou']][[2]][[1]]
## Part II: Complex part
pr <- dim(goext)[1]
z <- complex(real=goext$g,imaginary=goext$Psi_d)
### Information
x0 <- complex(real=rep(1,pr),imaginary=rep(0,pr))
y0 <- complex(real=rep(1,pr),imaginary=rep(0,pr))
xi <- complex(real=goext$alpha,imaginary=goext$Psi_b1)
yi <- complex(real=goext$h,imaginary=goext$Psi_c1)
### Non-new information
x0y0 <- complex(real=Re(x0)*Re(y0),imaginary=Im(x0)*Im(y0))
x1y1 <- complex(real=Re(xi)*Re(yi)-Im(xi)*Im(yi),imaginary=Re(xi)*Im(yi)-Im(xi)*Re(yi))
x0y1 <- complex(real=Re(x0)*Re(yi),imaginary=Re(x0)*Im(yi))
x0y2 <- complex(real=Re(x0)*Re(yi^2),imaginary=Re(x0)*Im(yi^2))
x0y3 <- complex(real=Re(x0)*Re(yi^3),imaginary=Re(x0)*Im(yi^3))
x1y0 <- complex(real=Re(xi)*Re(y0),imaginary=Im(xi)*Re(y0))
x2y0 <- complex(real=Re(xi^2)*Re(y0),imaginary=Im(xi^2)*Re(y0))
x3y0 <- complex(real=Re(xi^3)*Re(y0),imaginary=Im(xi^3)*Re(y0))
x1y2 <- complex(real=Re(xi)*Re(yi^2)-Im(xi)*Im(yi^2),imaginary=Re(xi)*Im(yi^2)-Im(xi)*Re(yi^2))
x2y1 <- complex(real=Re(xi^2)*Re(yi)-Im(xi^2)*Im(yi),imaginary=Re(xi^2)*Im(yi)-Im(xi^2)*Re(yi))
X <- as.matrix(data.frame(x0y0,yi,xi,x0y2,x1y1,x2y0,x0y3,x1y2,x2y1))
X
#! last one
print(beta.hat <- solve(Conj(t(X)) %*% X, Conj(t(X)) %*% z), digits=3)
beta.hat
#######################################################################################
## Nevada
psi <- ManifoldDestiny::nevada_sel 
names(psi)
pso <- psi %>% dplyr::mutate(Psi_ace=(a+c+e)/R,Psi_c=c/R,Psi_f=f/R) %>% dplyr::select(P,Psi_ace,Psi_c,Psi_f)
ne_st <- psi %>% dplyr::mutate(a=a+e,b=b+f)
co <- Countinggraphs(ne_st)
names(co$sdfc)
pr1024 <- as.numeric(clipr::read_clip())
ne_st_ad <- co$sdfc %>% dplyr::mutate(alpha2=alpha*alpha,g2=g*g,alphag=alpha*g) %>% dplyr::left_join(pso,by='P') #   Psi_ace         g         h     Psi_c     alpha     Psi_f %>% dplyr::filter(P%in%pr1024)
#   Psi_ace         g         h     Psi_c     alpha     Psi_f 
deltav <- apply(dplyr::select(ne_st_ad,Psi_ace,g,h,Psi_c,alpha,Psi_f),2,mean)
#   Psi_ace         g         h     Psi_c     alpha     Psi_f 
# 0.3457351 0.4734162 0.4117906 0.1152805 0.4499783 0.0329626 
##deltavcb <- c(0.3509776817,0.4795525403,0.4115875626,0.1157344416,0.4538313291,0.03226082851)
zdataframe <- ne_st_ad %>% dplyr::select(Psi_f,Psi_ace,g,h,Psi_c,alpha) %>% dplyr::mutate(zor=Psi_ace-deltav[1],zoi=g-deltav[2],xir=h-deltav[3],xii=Psi_c-deltav[4],yir=alpha-deltav[5],yii=Psi_f-deltav[6]) 
pr <- dim(zdataframe)[1]
zo <- complex(real=zdataframe$zor,imaginary=zdataframe$zoi)
x0 <- complex(real=rep(1,pr),imaginary=rep(0,pr))
y0 <- complex(real=rep(1,pr),imaginary=rep(0,pr))
xi <- complex(real=zdataframe$xir,imaginary=zdataframe$xii)
yi <- complex(real=zdataframe$yir,imaginary=zdataframe$yii)
x0y0 <- complex(real=re(x0)*re(y0),imaginary=im(x0)*im(y0))
x0y1 <- complex(real=Re(x0)*Re(yi),imaginary=Re(x0)*Im(yi))
x1y0 <- complex(real=Re(xi)*Re(y0),imaginary=Im(xi)*Re(y0))
x0y2 <- complex(real=Re(x0)*Re(yi^2),imaginary=Re(x0)*Im(yi^2))
x1y1 <- complex(real=Re(xi)*Re(yi)-Im(xi)*Im(yi),imaginary=Re(xi)*Im(yi)+Im(xi)*Re(yi))
x2y0 <- complex(real=Re(xi^2)*Re(y0),imaginary=Im(xi^2)*Re(y0))
z <- zo
#x <- as.matrix(data.frame(x0y0,x0y1,x1y0,x0y2,x1y1,x2y0))
X <- as.matrix(data.frame(x0y0,yi,xi))
#X <- as.matrix(data.frame(x0y0,yi,xi,x0y2,x2y0,x1y1))
print(beta.hat <- solve(Conj(t(X)) %*% X, Conj(t(X)) %*% z), digits=3)
beta.hat
# [1,] -0.00498579+0.000301922i
# [2,]  0.76207694+1.582677962i
# [3,] -0.09547085-0.517859951i
# [4,]  0.52971201-0.471202804i
# [5,]  0.00659995+0.124282804i
# [6,]  0.12218630+0.318259560i
#-0.005693794321159	0.000763439988202552	
#0.816255513568808	1.58764312942543	
#-0.140264148421178	-0.498357855155589	
#0.513978651269852	-0.582940600960614	
#0.0261057978154113	0.421921936572005	
#0.113403802790825	0.0164414984815799
## Alternative 2
#zv <- do.call(c, lapply(z, function(x) { c(Re(x), Im(x)) }))
#Data <- 
#  data.frame(ind=seq(1,length(z))) %>% 
#  dplyr::bind_cols(X) %>% 
#  dplyr::select(-1) %>%
#  sapply(rep.int,times=2) %>%
#  apply(2,function(x){
#  data.frame(r=rep(c(0,1),length(z)/2)*Re(x),i=rep(c(1,0),length(z)/2)*Im(x))
#  }) %>% as.data.frame() %>%
#  dplyr::mutate(z=zv) %>%
#  dplyr::relocate(z) 
#View(Data)
#X.Names <- paste('X', 1:(length(Data)-1), sep='')
#names(Data) <- c("Y",X.Names)
#Formula = paste("Y ~ ", paste(X.Names, collapse='+'), "-1")
#Model = lm(as.formula(Formula), data=Data)
########################################################################################
## Simuluation 1
reg_form_sim_s <- reg_form_oppo
prec <- 1:10
reg <- 1000
probw <- c(0.5,0.00)
probt <- list(list(e=c(0.50,0.50,0.00),std=c(0.00,0.00)),list(e=c(0.50,0.50,0.00),std=c(0.00,0.00)))
Ztech <- c(0,1)	
par_ele_1 <- electiontechn(probw=probw,probt=probt,ztech=Ztech,nprect=length(prec)) %>% dplyr::mutate(R=reg)
names(par_ele_1)[1:2] <- c('P','W')
sim_1 <- SimpleVoterdatabase(par_ele_1)
cv <- Countinggraphs(sim_1$listcbase)
apply(cv$sdfc,2,mean)
sim1_rep <- allreport(seldata=sim_1$listcbase,vfilter=list(a=0,b=0,c=0,d=0),part=c(1,2,3),formulas=reg_form_sim_s)
sim1_rep[[1]][[1]][[1]]
sim1_rep[[1]][[2]][[1]]
#sim1_rep[[1]][[3]][[1]]
#sort(sim1_rep[[3]]$lambda)
#######################################################################################
## Simulation 2
d <- ManifoldDestiny::clark_miller_stavros_sel; prec <- 1
length(d$P)
reg <- d$R;
probw <- c(0.5,0.10)
probt <- list(list(e=c(0.60,0.30,0.10),std=c(0.05,0.05)),list(e=c(0.60,0.30,0.10),std=c(0.05,0.05)))
Ztech <- c(0,1)	
par_ele_2 <- electiontechn(probw=probw,probt=probt,ztech=Ztech,nprect=length(d$P)) %>% dplyr::mutate(R=reg)
names(par_ele_2)[1:2] <- c('P','W')
sim_2 <- SimpleVoterdatabase(par_ele_2)
sim2_rep <- allreport(seldata=sim_2$listcbase,vfilter=list(a=0,b=0,c=0,d=0),part=c(1,2,3),formulas=reg_form_sim_s)
#sim2_rep[[1]][[2]][[1]]
#sim2_rep[[1]][[3]][[1]]
########################################################################################
# Maricopa
reg_form_mar <- c('alpha~y+x','alpha~y+xy+x')
mar_sel <- get(load(paste0(abs_path(),'/data/maricopa_sel.rda')))
ad <- 15
mar_rep <- allreport(seldata=mar_sel,vfilter=list(a=ad,b=ad,c=ad,d=ad),part=c(1,2,3),formulas=reg_form_mar)
mar_rep[[1]][[1]][[1]]
mar_rep[[1]][[2]][[1]]
mar_rep[[1]][[3]][[1]]
mar_rep[[2]][[2]][[1]]$mods
'2'2
#stuv_rep_org <- allreport(seldata=stuv_df_org$rdfc,vfilter=list(a=0,b=0,c=0,d=0),part=c(1,2,3),formulas=reg_form)
#######################################################################################
#break1 = function(X) {
#        do.call(c, lapply(X, function(x) { c(Re(x), Im(x)) }))
#}
#break3 = function(X) {
#        do.call(c, lapply(X, function(x) { c(Re(x), Im(x)) }))
#}
#fit.complex = function(Y, X.List) {
#	browser() #
#	abc <- do.call(data.frame,lapply(X.List, function(x){
#		      list(c(Re(x),rep(0,length(x))),c(rep(0,length(x)),Im(x))) 
#	}))
#
#	names(abc) <- c("x0","x0i","x1","x1i","x2","x2i")
#		     
#	Data <- data.frame(ind=seq(1,30),part=rep(c('r','i'),15),YF=break1(Y)) %>%
#		dplyr::arrange(desc(part),ind) %>%
#		dplyr::bind_cols(abc) %>%
#		dplyr::arrange(ind)
#        
#	Model = lm(as.formula('YF~x0+x0i+x1+x1i+x2+x2i-1'), data=Data)
#
#
#	#XC1 <- rep(c(0,1),length(YF))
#	#XC2 <- rep(c(1,0),length(YF))
#        #Data = data.frame(Y = YF, XC1,XC2)
#	#XF.List = do.call(c, lapply(X.List,
#        #        function(x) { list(break1(x), break2(x)) } ))
#        ## Make the data.fram
#        #Data = data.frame(Y = YF)
#        #X.Names = paste('X', 1:length(XF.List), sep='')
#        #for (N in seq_along(XF.List)) {
#	#	# N <- 4
#        #        Data[[ X.Names[[N]] ]] = XF.List[[N]]
#	#	print(Data)
#        #}
#        ## Formula + Model
#        #Formula = paste("Y ~ ", paste(X.Names, collapse='+'), "-1")
#        #Model = lm(as.formula(Formula), data=Data)
#        ## Make them complex again
#        Coeffs = sapply(seq_along(X.List),
#                function(N) {
#			# N <- 2
#                        ( Model$coefficients[[ X.Names[[2*N-1]] ]]
#                        + Model$coefficients[[ X.Names[[2*N]] ]]*1i )
#                })
#        names(Coeffs) = names(X.List)
#        Model$coefficients.complex = Coeffs
#        Model
#}
### Complex
#gs_complex <- 'https://docs.google.com/spreadsheets/d/1x1w2hv8JdAA5Zq5C-SbmsSBTA1fhO1aUuDyFRgEeHK0/edit#gid=263283852'
#df_complex <- googlesheets4::read_sheet(gs_complex, sheet=1, range="A4:F1234")
#names(df_complex) <- c('Psi_apc','zeta','h','Psi_c','alpha','Psi_b')
#npr <- dim(df_complex)[1]
#Y <- (df_complex$zeta+1i*df_complex$Psi_apc)
#X <- list(X0=rep(c(1+1i*1),npr),X1=df_complex$h+1i*df_complex$zeta,X2=df_complex$alpha+1i*df_complex$Psi_b) 
#Model = fit.complex(Y=Y,X.List=X)
#Y
#Beta1 = Model$coefficients.complex[[1]]
#Beta2 = Model$coefficients.complex[[2]]
#########################################################################################
# STUV
#reg_form <- c('alpha~h','alpha~g+h','alpha~h+g+Gamma','alpha~ g+ I(g^2)+ gh+ I(h^2)+ I(g^3)+ g2h+ I(h^3)','alpha~y+x','alpha~h+g+Gamma')
#df_proto_org <- googlesheets4::read_sheet(gs_proto_stuv, sheet='stuv', range="A2:F22")
#df_proto_alt <- googlesheets4::read_sheet(gs_proto_stuv, sheet='stuv', range="H2:M22")
#stuv_df_org <- Countinggraphs(df_proto_org)
#stuv_df_alt <- Countinggraphs(df_proto_alt)
#stuv_rep_org <- allreport(seldata=stuv_df_org$rdfc,vfilter=list(a=0,b=0,c=0,d=0),part=c(1,2,3),formulas=reg_form)
#stuv_rep_alt <- allreport(seldata=stuv_df_alt$rdfc,vfilter=list(a=0,b=0,c=0,d=0),part=c(1,2,3),formulas=reg_form)
#stuv_rep_org[[1]][[1]][[3]][[2]]
#stuv_rep_org[[2]][[3]][[1]]$mods
#stuv_rep_alt[[1]][[2]][[3]][[2]]
#stuv_rep_alt[[2]][[3]][[1]]$mods
#######################################################################################
# Google spreadsheet
#vtype <- 2
#gs_proto <- 'https://docs.google.com/spreadsheets/d/1s4sk_d9JCF4L2yMVn6N8cX34gPfZXCNRasSjioFVL3Y/edit?usp=sharing'
#df_proto <- googlesheets4::read_sheet(gs_proto, sheet='normal', range="A11:G14")
#co <- Countinggraphs(df_proto)
#co$plotly3d(partition=vtype)
#co$gridarrange()
#co$all_pl_3dmani[[vtype]]
#######################################################################################
#gs_proto_1 <- 'https://docs.google.com/spreadsheets/d/1u9l2mVobdHOG4fmxde0FwfnUMH9y3-xG4ALoQhccehE/edit#gid=0'
#r2_df <- googlesheets4::read_sheet(gs_proto_1, range="C4:T1004")
#co <- Countinggraphs(df_proto)
#co$plotly3d(partition=vtype)
#co$gridarrange()
#co$all_pl_3dmani[[vtype]]
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
## Gobernational Election: Lake vs. Robson ##
### Reading standardized data from Rda format
az_st <- ManifoldDestiny::lake_sel
reg_form_az <- c('alpha~g','alpha~x','alpha~x+y','alpha~x+y+zeta')
names(az_st) <- c("P","R","a","B","C","b","E","c","G","H","d","J")
rep_gne_1 <- allreport(seldata=az_st,vfilter=list(a=0,b=0,c=0,d=0),part=c(1,2,3),formulas=reg_form_az)
abcd <- 50
rep_gne_2 <- allreport(seldata=az_st,vfilter=list(a=abcd,b=abcd,c=abcd,d=abcd),part=c(1,2,3),formulas=reg_form_az)
#rep_gne_1[[1]][[1]][[3]][[2]]
#rep_gne_2[[1]][[1]][[3]][[2]]
rep_gne_1[[2]][[3]][[1]]$mods
rep_gne_2[[2]][[4]][[1]]$mods
reg_form <- c('alpha~h','alpha~h+g','alpha~h+g+Gamma')
#names(mi_st) <- c( "P","R","SEV","MEV","c","d","a","b")
#rep_stmi_ii <- allreport(seldata=mi_st,part=c(1,2,3),formulas=reg_form)
#rep_stmi_ii[['cou']][[2]][[3]][[2]]
#rep_stmi_ii[['est']][[1]][[1]]$mods
#rep_stmi_ii[['est']][[2]][[1]]$mods
#rep_stmi_ii[['est']][[3]][[1]]$mods
##########################################################################################################################################
#######################################################################################
### Estimation ###
reg_form <- c('alpha~h+g','alpha~ g+ I(g^2)+ gh+ I(h^2)+ I(g^3)+ g2h+ I(h^3)','alpha~y+x','alpha~h+g+Gamma')
modlat <- ManifoldDestiny::eqpar$meql
## Stavros Miller
mi_st <- ManifoldDestiny::clark_miller_stavros_sel
abcd <- 50
rep_stmi <- allreport(seldata=mi_st,vfilter=list(a=abcd,b=abcd,c=abcd,d=abcd),part=c(1,2,3),formulas=reg_form)
rep_stmi[['cou']][[2]][[3]][[2]]
##### Cubic plot ###
#### Estimere
p_path <- paste0(rprojroot::find_rstudio_root_file(),'/inst/script/roots')
kvec <- rep_stmi[['est']][[2]][[1]][[3]]$estimate
fqs <- reticulate::import_from_path("fqs", path = p_path)
ndft <- Countinggraphs(mi_st)$rdfc %>% dplyr::mutate(gh=g*h,g2h=g^2*h)
rep_stmi[['est']][[2]][[1]][[3]]$estimate
#0.03669834+0.76505270*x+ -0.82951971*x^2+ 0.99386391*x*y+ 0.30746607*y^2+ 0.92813499*x^3+ -1.04633695*x^2*y+ -0.17986266*y^3
cubicres <- ndft %>% dplyr::select(P,g,h,alpha) %>%
	dplyr::arrange(P) %>%
	dplyr::mutate(A=kvec[6]) %>%
	dplyr::mutate(B=kvec[3]+h*kvec[7]) %>%
	dplyr::mutate(C=kvec[2]+h*kvec[4]) %>%
	dplyr::mutate(D=kvec[1]+kvec[5]*h^2+kvec[8]*h^3-alpha) %>%
	dplyr::group_by(P) %>%
	dplyr::mutate(cubic=fqs$cubic_roots(c(A,B,C,D))) %>%
	dplyr::mutate(g_exp=Re(cubic)[1])
cpl <- ggplot2::ggplot(data=cubicres,aes(x=g_exp,y=g)) + ggplot2::geom_point() + ggplot2::theme_classic() 
#######################################################################################
# Washoe 
wa_st <- ManifoldDestiny::washoe_sel
rep_pel <- allreport(seldata=wa_st,part=c(1,2,3),formulas=reg_form)
#######################################################################################
# Oakland 
oa_st <- ManifoldDestiny::oakland_sel
#View(oa_st)
#######################################################################################
app_n_cou <- Countinggraphs(app_bal$listcbase)$sdfc %>% dplyr::mutate(RV=S+T+U+V) %>% dplyr::mutate(gh=g*h,g2h=g^2*h) %>% dplyr::mutate(xy=x*y) 
app_n_sel <- list(alpha=rev(set_n)[1],solvf='y',form=2)
app_n_eqs <- list(alpha=set_n)
abc <- selreport(app_n_cou)
#abc[[1]]$pl_3dmani[[1]]
#mar_2022 <- paste0(abs_path(),'/data-raw/xlsx/Maricopa, General, 2022, Tabulation Database.xlsx')
#mar_2022_rec <- openxlsx::read.xlsx(mar_2022, sheet=1)[-c(1:5),-c(1:6)][,-c(3,6)][,1:6]
#s_1 <- googlesheets4::read_sheet(gs_ap1, sheet='Governor, Mail-in Manifold')
#filename <- paste0(abs_path(),'/data-raw/xlsx/Miller vs Stavros.xlsx')
#clark_miller_stavros_rec <- openxlsx::read.xlsx(filename, sheet=2) %>% dplyr::select(1:8) 
coc_2022 <- paste0(abs_path(),'/data-raw/xlsx/cochise_az_nov_2022_general_official2.xlsx')
coc_2022_rec <- openxlsx::read.xlsx(coc_2022, sheet=1)
names(coc_2022_rec) <- c("Precinct name","R","V","X4","X5","A1","A2","A3","B1","B2","B3")
coc_2022_rec <- coc_2022_rec[c(-1,-dim(coc_2022_rec)[1]),]
coc_2022_sel <- coc_2022_rec %>% dplyr::mutate_at(names(coc_2022_rec)[-1],as.numeric) 
bc <- 50
lm <- 0.05
um <- 0.95

dfc <- coc_2022_sel %>%
  dplyr::mutate(a=A1) %>%
  dplyr::mutate(b=B1) %>%
  dplyr::mutate(c=A2+A3) %>%
  dplyr::mutate(d=B2+B3)%>%
  dplyr::mutate(P=row_number("Precinct name")) %>%
  dplyr::mutate(alpha_t=(a+c)/(a+b+c+d)) %>%
  dplyr::mutate(zeta_t=(c+d)/(a+b)) %>%
  dplyr::mutate(x_t=a/(a+b)) %>%
  dplyr::mutate(y_t=c/(c+d)) %>%
  dplyr::filter(if_all(c(a,b,c,d), ~ . >= bc))

select(dfc,alpha,x,y)
summary(lm(data=select(dfc,alpha_t,x_t)))
summary(lm(data=select(dfc,alpha_t,y_t)))
summary(lm(data=select(dfc,alpha_t,x_t,y_t)))
summary(lm(data=select(dfc,alpha_t,x_t,y_t,zeta_t)))
usethis::use_data(coc_2022_sel,overwrite = T)



View(dfc)
l()
View(dfc)
set_n <- c('k0+k1*y','k0+k1*x+k2*y','k0+k1*x+k2*y+k3*zeta')
set_h <- c('k0+k1*g','k0+k1*g+k2*h','k0+k1*g+k2*h+k3*zeta')
set_o <- c('k0+k1*n','k0+k1*n+k2*m','k0+k1*n+k2*m+k3*zeta')

View(dfc)
app_n_sel <- list(alpha=rev(set_n)[1],solvf='y',form=1)
app_n_eqs <- list(alpha=set_n)
app_n_cou <- Countinggraphs(dfc)$sdfc %>% dplyr::mutate(V=a+b+c+d) %>% dplyr::mutate(gh=g*h,g2h=g^2*h) %>% dplyr::mutate(xy=x*y) 
app_n_arp <- allreport(app_n_cou,app_n_sel,app_n_eqs)
app_n_srp <- sumreport(app_n_arp)
app_n_srp[2]

dfa <- Countinggraphs(dfc)

x <- 1
co <- Countinggraphs(dfa$sdfc) 
co$sortpre(x)
co$r2siminput(x)
co$plot2d(x)
co$plotxy(x)
co$resplot(x)
co$plotly3d(partition=x)
co$gridarrange()
co$all_pl_3dmani[[4]][[1]]







select(dfc,alpha,x,y)
summary(lm(data=select(dfc,alpha,x)))
summary(lm(data=select(dfc,alpha,y)))
summary(lm(data=select(dfc,alpha,x,y)))
summary(lm(data=select(dfc,alpha,x,y,zeta)))
usethis::use_data(coc_2022_sel,overwrite = T)
################################################################################################################










mar_2022 <- paste0(abs_path(),'/data-raw/xlsx/Maricopa, General, 2022, Tabulation Database.xlsx')
mar_2022_rec <- openxlsx::read.xlsx(mar_2022, sheet=1)
mar_2022_fil <- mar_2022_rec[c(-1,-2,-3),-c(1:7)][,1:11]
names(mar_2022_fil) <- mar_2022_fil[1,]
mar_2022_fil <- mar_2022_fil[-1,]
names(mar_2022_fil) <- make.unique(names(mar_2022_fil))
mar_2022_sel <- mar_2022_fil %>% dplyr::mutate_at(names(mar_2022_fil),as.numeric) 
usethis::use_data(mar_2022_sel,overwrite = T)
################################################################################################################
cla_2022 <- paste0(abs_path(),'/data-raw/xlsx/Clark County, NV, 2022, Quaterionic Result.xlsx')
cla_2022_rec <- openxlsx::read.xlsx(cla_2022, sheet=4)
cla_2022_fil <- cla_2022_rec[c(-1,-2,-3),1:26][,c(-4,-5,-6)][,c(1,2,3,9:14)]
names(cla_2022_fil) <- cla_2022_fil[1,]
cla_2022_fil <- cla_2022_fil[-1,]
names(cla_2022_fil) <- make.unique(names(cla_2022_fil))
cla_2022_sel <- cla_2022_fil %>% dplyr::mutate_at(names(cla_2022_fil)[3:9],as.numeric) 
usethis::use_data(cla_2022_sel,overwrite = T)
################################################################################################################
## Washow
filename <- paste0(abs_path(),'/data-raw/xlsx/Patriot Database, Nevada, Washoe.xlsx')
was_2020_pre_rec <- openxlsx::read.xlsx(filename) %>% dplyr::select(-1)	
was_2020_pre_sel <- was_2020_pre_rec %>% dplyr::mutate_at(names(was_2020_pre_rec)[c(1,3:12)],as.numeric) 
usethis::use_data(was_2020_pre_sel, overwrite = TRUE)
################################################################################################################
filename <- paste0(abs_path(),'/data-raw/xlsx/ArizonaExportByPrecinct_08022022_08052022.txt')
lake <- data.table::fread(filename, sep = "\t")
lake_sel <- lake %>% dplyr::filter(ContestOrder==10) %>% 
	dplyr::filter(CandidateId%in%(30:34)) %>% 
	dplyr::select(8,11:39) %>% rename_with(tolower) %>%
        mutate(across(.cols=where(is.integer), .fns=as.numeric)) %>%
        dplyr::select(1,2,18,22,26,4,9,13,30) %>%
	dplyr::select(1,6,7,8,4,5) %>%
        tidyr::pivot_wider(names_from=candidatename, values_from=c('votes_early vote','votes_election day')) 
lake_sel
usethis::use_data(lake_sel, overwrite = TRUE)
################################################################################################################







#clkon <- paste0(abs_path(),'/data-raw/xlsx/Clark County, NV, 2022 General Election Tabulation Database.xlsx')
#alphaxy_rec <- openxlsx::read.xlsx(clkon, sheet='Mothersheet')
#alphaxy_flt <- alphaxy_rec[c(-1,-2,-3),c(-4,-5)]
#names(alphaxy_flt) <- alphaxy_flt[1,]
#alphaxy_flt <- alphaxy_flt[-1,]
#names(alphaxy_flt)
#
#
##abc <- alphaxy_rec
##View(abc)
##### Alphaxy
##filename <- paste0(abs_path(),'/data-raw/xlsx/XYAlpha Rig.xlsx')
##alphaxy_rec <- openxlsx::read.xlsx(filename, sheet=1)
##names(nevadap_2008_rec)[c(1,3,4:12)] <- c("R","P","A","B","C","D","E","F","G","H","I")
##nevadap_2008_rec_sel <- nevadap_2008_rec[,c(1,3,4:12)]
##usethis::use_data(nevadap_2008_rec_sel,overwrite = T)
##View(nevadap_2008_rec_sel)
##
##?stringr::str_match
#
#

##############################################
xl_list <- list('https://docs.google.com/spreadsheets/d/1xyMQovYh81Wptz-5fyVuLpL4uHrNE78NCDKOVQZ8MyE/edit?usp=sharing', 'https://docs.google.com/spreadsheets/d/1v9-bAI9INnjgfInEJBHNBp4D7N3-Opf4nDKJbzxdBrk/edit?usp=sharing', 'https://docs.google.com/spreadsheets/d/1pzpaOrABQ_9oJK5juubeG_gj4InJpkFzhi5yzxwqf8k/edit?usp=sharing', 'https://docs.google.com/spreadsheets/d/1Gs2Z3eo-ZNO7FZRGXZpgh6J2dJeyryCaMLSbUg3tQig/edit?usp=sharing', 'https://docs.google.com/spreadsheets/d/1DC5zZjoklVgTNExXRTxAIXYcq8_TpVn_WAK03pi6Fpw/edit?usp=sharing', 'https://docs.google.com/spreadsheets/d/1tWgYZcv53teugdQi4WYmNEw0XlIBktrXqka8G6I7TJk/edit?usp=sharing', 'https://docs.google.com/spreadsheets/d/1tAB6sjG7P-0y6PzxyYDvKMbYDK833MgQhfLti5Ip6Gg/edit?usp=sharing', 'https://docs.google.com/spreadsheets/d/1xk8IW9v03i0omZfUo9c75_7wuw9RvE1w8UqUyo1WzAw/edit?usp=sharing')
#Load the required library 
library(googlesheets4)
s_1 <- googlesheets4::read_sheet(xl_list[[1]], sheet=2)
s_2 <- googlesheets4::read_sheet(xl_list[[2]], sheet=2)
s_3 <- googlesheets4::read_sheet(xl_list[[3]], sheet=2)
s_4 <- googlesheets4::read_sheet(xl_list[[4]], sheet=2)
s_5 <- googlesheets4::read_sheet(xl_list[[1]], sheet=2)
s_6 <- googlesheets4::read_sheet(xl_list[[1]], sheet=2)
s_7 <- googlesheets4::read_sheet(xl_list[[1]], sheet=2)
s_8 <- googlesheets4::read_sheet(xl_list[[1]], sheet=2)
##############################################

### Abc
filename <- paste0(abs_path(),'/data-raw/xlsx/Fair 2008 Obama vs Mccain, Clark County, Nevada.xlsx')
nevadap_2008_rec <- openxlsx::read.xlsx(filename, sheet=2)
names(nevadap_2008_rec)[c(1,3,4:12)] <- c("R","P","A","B","C","D","E","F","G","H","I")
nevadap_2008_rec_sel <- nevadap_2008_rec[,c(1,3,4:12)]
usethis::use_data(nevadap_2008_rec_sel,overwrite = T)
View(nevadap_2008_rec_sel)

### Def
filename <- paste0(abs_path(),'/data-raw/xlsx/Fair 2016 Hillary vs Trump, Clark County, Nevada.xlsx')
clarkp_2016_rec <- openxlsx::read.xlsx(filename, sheet=2)
names(clarkp_2016_rec)[c(1,3,4:9)] <- c("R","P","A","B","C","D","E","F")
clarkp_2016_rec_sel <- clarkp_2016_rec[,c(1,3,4:9)]
usethis::use_data(clarkp_2016_rec_sel,overwrite = T)

# Fulton
filename <- paste0(abs_path(),'/data-raw/xlsx/Fulton County.xlsx')
fulton_rec <- openxlsx::read.xlsx(filename, sheet=3)[,c(1:7)]
fulton_rec_sel <- fulton_rec %>% `colnames<-` (c("P","A","B","C","D","E","F"))
#names(fulton_rec_sel)
## [1] "Precinct"  "Biden.Edv" "TrumpEdv"  "Biden.Adv" "Trump.Adv" "Biden.Abs" "Trump.Abs"
usethis::use_data(fulton_rec_sel,overwrite = T)
# Abc
filename <- paste0(abs_path(),'/data-raw/xlsx/Rotation Restoration Demonstration.xlsx')
rotation_rec <- openxlsx::read.xlsx(filename, sheet=1)[c(-1,-2,-3),c(3,6,11,12,13,14)]
names(rotation_rec) <-c("P","R","a","b","c","d")
usethis::use_data(rotation_rec,overwrite = T)

View(rotation_rec)
# presidental
filename <- paste0(abs_path(),'/data-raw/xlsx/Clark County, NV.xlsx')
pres_clark_rec <- openxlsx::read.xlsx(filename, sheet='Clark County Recorder')[-1]
cnames <-  c("P","R","A1","A2","A3","B1","B2","B3","C1","C2","C3")
names(pres_clark_rec) <- cnames
pres_clark_sel <- pres_clark_rec %>% dplyr::mutate_at(cn,as.numeric) 
usethis::use_data(pres_clark_sel,overwrite = true)
ManifoldDestiny::pres_clark_sel

# Senate
filename <- paste0(abs_path(),'/data-raw/xlsx/Manifolds In Action; Clark 2022 Primaries.xlsx')
sen_rec <- openxlsx::read.xlsx(filename, sheet=1)[-1,c(2:3,14:22)]
cn <- c("P","R","A1","A2","A3","B1","B2","B3","C1","C2","C3")
names(sen_rec) <- cn
sen_rec_sel <- sen_rec %>% dplyr::mutate_at(cn,as.numeric) 
usethis::use_data(sen_rec_sel,overwrite = TRUE)
bm()
dfa <- ManifoldDestiny::sen_rec_sel %>% dplyr::mutate(a=A1,b=B1+B3,c=A3,d=C1+C3)
View(dfa)


bm()	
View(sen_rec)


ctype <- c("pre","a","b","c","d","e","f")
dallas_sel <- openxlsx::read.xlsx(filename, sheet="County Recorder Data") %>% dplyr::select(1:7) %>%
	`colnames<-` (c("Precinct","e","f","b","a","d","c")) %>% filter(!row_number() %in% c(1170)) %>%
	dplyr::mutate(pre=gsub("-","",Precinct)) %>% dplyr::mutate_at(ctype,as.numeric) 

# Hill
View(arkansas_reynold_hill_sel)
filename <- paste0(abs_path(),'/data-raw/xlsx/Arkansas, 2022 Reynold vs Hill.xlsx')
arkansas_reynold_hill_sel <- openxlsx::read.xlsx(filename, sheet=2)[,c(1:7)] %>% `colnames<-` (c('C','Pre','R','a','b','c','d')) %>% dplyr::mutate(P=row_number(Pre)) %>% dplyr::relocate(P)
usethis::use_data(arkansas_reynold_hill_sel,overwrite = TRUE)
bm()
filename <- paste0(abs_path(),'/data-raw/xlsx/ArizonaExportByPrecinct_08022022_08052022.txt')
lake <- data.table::fread(filename, sep = "\t")
lake_sel <- lake %>% dplyr::filter(ContestOrder==10) %>% 
	dplyr::filter(CandidateId%in%(30:34)) %>% 
	dplyr::select(8,11:39) %>% rename_with(tolower) %>%
        mutate(across(.cols=where(is.integer), .fns=as.numeric)) %>%
        dplyr::select(1,2,18,22,26,4,9,13,30) %>%
	dplyr::select(1,6,7,8,4,5) %>%
        tidyr::pivot_wider(names_from=candidatename, values_from=c('votes_early vote','votes_election day')) %>%
	`colnames<-`  (c('Pid','P','R','A','B','C','D','E','F','G','H','I','J')) %>%
	tidyr::unnest(c('Pid','P','R','A','B','C','D','E','F','G','H','I','J')) %>%
	dplyr::arrange(Pid)
usethis::use_data(lake_sel, overwrite = TRUE)
## To candidates
# Dallas
View(clark_miller_stavros_sel)

filename <- paste0(abs_path(),'/data-raw/xlsx/Dallas Texas, Completed.xlsx')
ctype <- c("pre","a","b","c","d","e","f")
dallas_sel <- openxlsx::read.xlsx(filename, sheet="County Recorder Data") %>% dplyr::select(1:7) %>%
	`colnames<-` (c("Precinct","e","f","b","a","d","c")) %>% filter(!row_number() %in% c(1170)) %>%
	dplyr::mutate(pre=gsub("-","",Precinct)) %>% dplyr::mutate_at(ctype,as.numeric) 
usethis::use_data(dallas_sel, overwrite = TRUE)


filename <- paste0(abs_path(),'/data-raw/xlsx/Maricopa Plane, Hybrid.xlsx')
maricopa_sel <- openxlsx::read.xlsx(filename) %>% `colnames<-` (c("P","name","R","b","a","d","c"))
usethis::use_data(maricopa_sel, overwrite = TRUE)
View(maricopa_sel)
## Oakland
filename <- paste0(abs_path(),'/data-raw/xlsx/OaklandCountyRecorder.xlsx')
oakland_sel <- openxlsx::read.xlsx(filename)
names(oakland_sel)  <- c("pre","Town","Prec","BidenEdvspt","TrumpEdvspt","OtherEDVspt","BidenAVspt","TrumpAVspt","OtherAVspt","BidenEdvInd","TrumpEdvInd","OtherEDVInd","BidenAVInd","TrumpAVInd","OtherAVInd")
usethis::use_data(oakland_sel, overwrite = TRUE)

# Tarrant
filename <- paste0(abs_path(),'/data-raw/xlsx/Tarrant County.xlsx')
washoe_sel <- openxlsx::read.xlsx(filename)
names(washoe_sel)

# Clark
reg_form <- c('alpha~g','alpha~g+h','alpha~g+h+Gamma')
ctype <- c("a","b","c","d")
filename <- paste0(abs_path(),'/data-raw/xlsx/Clark County, NV.xlsx')
clark_sel <- openxlsx::read.xlsx(filename,sheet='Clark County Recorder') %>% `colnames<-` (c("pind","P","R","biden.edv","d","b","trump,edv","c","a","other.edv","other.miv","other.adv")) %>%
dplyr::mutate_at(ctype,as.numeric) %>%
dplyr::mutate_at(ctype, ~replace(., is.na(.), 0))
usethis::use_data(clark_sel, overwrite = TRUE)
#View(clark_sel)
#ca <- allreport(seldata=clark_sel,part=c(1,2,3),formulas=reg_form)
#ca[['cou']][[2]][[1]]
#ca[['cou']][[2]][[3]][[2]]
#ca[['est']][[2]][[1]]$mods
#ca <- allreport(seldata=clark_sel,part=c(1,2,3),formulas=reg_form)
#wa[['cou']][[2]][[1]]
#wa[['est']][[3]][[1]]$mods
#bm()
# Nevada
filename <- paste0(abs_path(),'/data-raw/xlsx/PrefaceNevada.xlsx')
ctype <- c("P","R","tot","a","b","c","d","e","f")
nevada_sel <- openxlsx::read.xlsx(filename,sheet=5) %>% select(c(1,3:10)) %>%   filter(!row_number() %in% c(1)) %>% 
	`colnames<-` (c("P","R","tot","a","b","c","d","e","f")) %>% dplyr::mutate_at(ctype,as.numeric) %>% dplyr::mutate_at(ctype, ~replace(., is.na(.), 0))
usethis::use_data(nevada_sel, overwrite = TRUE)
names(ManifoldDestiny::nevada_sel)
## Three candidates
filename <- paste0(abs_path(),'/data-raw/xlsx/Manifolds In Action; County Recorder Data.xlsx')
clark <- openxlsx::read.xlsx(filename,sheet=1) 
stn <- c("P","R","A1","A2","A3","B1","B2","B3","C1","C2","C3")
she <- clark[c(-1),c(-1,-4)] %>% dplyr::select(1,2,3:11)  %>% `colnames<-` (stn) %>% dplyr::mutate_at(stn,as.numeric) 
gov <- clark[c(-1),c(-1,-4)] %>% dplyr::select(1,2,12:20) %>% `colnames<-` (stn) %>% dplyr::mutate_at(stn,as.numeric) 
sen <- clark[c(-1),c(-1,-4)] %>% dplyr::select(1,2,21:29) %>% `colnames<-` (stn) %>% dplyr::mutate_at(stn,as.numeric) 
clark_sgs_sel <- list(sheriff=she,governor=gov,senate=sen)
usethis::use_data(clark_sgs_sel,overwrite = TRUE)
## II: Standardizing the data before counting
#  [1] "U.S..Senator"                            "U.S..Representative.in.Congress.Dist..6" "U.S..Representative.in.Congress.Dist..7"
#  [4] "Governor"                                "State.Senator.Dist..19"                  "State.Senator.Dist..21"
#  [7] "State.Representative.Dist..19"           "State.Representative.Dist..21"           "Secretary.of.State"
# [10] "Attorney.General"                        "State.Treasurer"                         "Superintendent.of.Public.Instruction"
# [13] "State.Mine.Inspector"                    "Corporation.Commissioner"
### Cochise county ###
race <- 1 
df_coh <- cochise_az_2022[[race]]
cou <- df_coh %>%
  dplyr::rename(P=1) %>%
  dplyr::rename(R=3) %>%
  dplyr::mutate(R=as.numeric(R)) %>%
  dplyr::mutate(S=A1+A3) %>%
  dplyr::mutate(T=B1+B3) %>%
  dplyr::mutate(U=A2) %>%
  dplyr::mutate(V=B2)
co <- Countinggraphs(cou)
co$purging()
cor(co$rdfc$g,co$rdfc$h)
slr <- selreport(co$rdfc,eqregpar=list(alpha=set_n[3],1))
# 1
slr[[1]]$desms
slr[[1]]$pl_2dsort
slr[[1]]$pl_corrxy[1]
slr[[1]]$all_pl_3dmani[1]

# 2
# 3
# 4


## I::
gs_ap3 <- 'https://docs.google.com/spreadsheets/d/1FxJg9hjU-M1MIeKl0koiHDVIp2dPAmm3nJpRzd5Ejdg/edit#gid=301195549'
mar_2022_sel <- googlesheets4::read_sheet(gs_ap3, sheet=1, range='G5:O940')[,-c(2)][,-c(4,7)]
ncn3 <- c("P","R","A1","B1","A2","B2")

