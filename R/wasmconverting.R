#' @export ballcastsim
ballcastsim <- function(
  dfm=(function(x){data.frame(P=seq(1,x),RV=as.integer(rnorm(x,1000,30)))})(10),
  probw=c(0.5,0.02),
  probva=c(0.7,0.2,0.03,0.00),
  probvb=c(0.7,0.2,0.03,0.00),
  ztech=c(0,0)){

  probvrnd <<- dfm |>
    dplyr::mutate(ZV=rnorm(dplyr::n(),probw[1],probw[2])) |>
    dplyr::mutate(N=runif(dplyr::n(),ztech[1],ztech[2])) |>
    dplyr::mutate(p3=(1-ztech)*(1-pmax(0, pmin(1,rnorm(dplyr::n(),probva[1],probva[3]))))) |>
    dplyr::mutate(p6=(1-ztech)*(1-pmax(0, pmin(1,rnorm(dplyr::n(),probvb[1],probvb[3]))))) |>
    dplyr::mutate(p2=(1-p3)*pmax(0, pmin(1,rnorm(dplyr::n(),probva[2],probva[4])))) |>
    dplyr::mutate(p5=(1-p6)*pmax(0, pmin(1,rnorm(dplyr::n(),probvb[2],probvb[4])))) |>
    dplyr::mutate(p1=1-p2-p3) |>
    dplyr::mutate(p4=1-p5-p6) |>
    dplyr::select(P,RV,ZV,N,p1,p2,p3,p4,p5,p6) |>
    dplyr::group_split(P)
  voticp <- lapply(seq(1,length(probvrnd)),function(x){
    inpv <- probvrnd[[x]]
    dfa <- data.frame(P=rep(inpv$P,inpv$RV),R=inpv$RV,C=stats::rbinom(inpv$RV,1,inpv$ZV)) |>
    dplyr::mutate(Id=dplyr::row_number()) |>
    dplyr::group_by(Id) |>
    dplyr::mutate(V=ifelse(C==1,sample(1:3,1,prob=c(inpv$p1,inpv$p2,inpv$p3)),sample(4:6,1,prob=c(inpv$p4,inpv$p5,inpv$p6)))) |>
    dplyr::mutate(S=ifelse(V==1,1,0)) |>
    dplyr::mutate(T=ifelse(V==4,1,0)) |>
    dplyr::mutate(U=ifelse(V==2,1,0)) |>
    dplyr::mutate(V=ifelse(V==5,1,0))
})
  votpc <- do.call("rbind", voticp) |>
    dplyr::arrange(P) |>  dplyr::group_by(P)  |>
    dplyr::select(c('P','R','S','T','U','V')) |>
    dplyr::mutate(S=sum(S),T=sum(T),U=sum(U),V=sum(V)) |>
    dplyr::distinct() |>
    dplyr::mutate(Z=sum(S+T+U+V)) |>
    dplyr::ungroup()
}
## Simulate prob
#' @export r2simn
r2simn <- function(nprec=300,
		   regs=c(3.15,0.25),
		   minmax=c(400,4000),
		   turn=c(0.5,0.10),
		   Invper=c(0.5,0.10),
		   u=c(0.6,0.10),
		   dv=c(-0.2,0.08),
		   form=1)
		   {
    BL <- list(fm1=c('S','T','U','V'),fm2=c('S','V','T','U'),fm3=c('S','U','T','V'))[[form]]
    form <- list(std=c('alpha~x+y','alpha~x+y+zeta'),
                 std=c('alpha~g+h','alpha~g+h+gamma'),
                 std=c('alpha~n+m','alpha~n+m+xi'))[[form]]
    # Box
    dfb <- data.frame(P=seq(1,nprec)) %>%
    dplyr::mutate(genreg=10^qnorm(runif(n()),regs[1],regs[2])) %>%
    dplyr::mutate(R=round(ifelse(genreg>minmax[2]|genreg<minmax[1],runif(1,minmax[1],minmax[2]),genreg))) %>%
    dplyr::mutate(Z=round(R*qnorm(runif(n()),turn[1],turn[2]))) %>%
    # Form
    dplyr::mutate(!!paste0('OpT'):=round(Z*rnorm(n(),Invper[1],Invper[2]))) %>%
    dplyr::mutate(!!paste0('TpF'):=Z-OpT) %>% dplyr::mutate(gen_u=rnorm(n(),u[1],u[2])) %>%
    dplyr::mutate(gen_v=rnorm(n(),dv[1],dv[2])+gen_u) %>%
    # Ballots
    dplyr::mutate(!!paste0(BL[1]):=round(gen_u*OpT)) %>%
    dplyr::mutate(!!paste0(BL[2]):=round(OpT-S)) %>%
    dplyr::mutate(!!paste0(BL[3]):=round(gen_v*TpF)) %>%
    dplyr::mutate(!!paste0(BL[4]):=round(TpF-U))

    dfb2 <- Countingprocess(dfb)$sdfc

    # Prediction
    pe_1 <- stats::predict(lm(as.formula(form[1]),data=dfb2))
    pe_2 <- stats::predict(lm(as.formula(form[2]),data=dfb2))
    # Comparison
    dfc <- dfb2 %>%
	#1 Alpha
	dplyr::mutate(alpha_hat_1=pe_1) %>%
	dplyr::mutate(alpha_hat_2=pe_2) %>%
  	dplyr::mutate(TSS_1=(alpha-mean(alpha))^2) %>%
  	dplyr::mutate(RSS_1=(alpha-alpha_hat_1)^2) %>%
  	dplyr::mutate(sTSS_1=sum(TSS_1)) %>%
  	dplyr::mutate(sRSS_1=sum(RSS_1)) %>%
  	dplyr::mutate(R2_1=1-sRSS_1/sTSS_1) %>%
	# Ballots
  	dplyr::mutate(TSS_2=(alpha-mean(alpha))^2) %>%
  	dplyr::mutate(RSS_2=(alpha-alpha_hat_2)^2) %>%
  	dplyr::mutate(sTSS_2=sum(TSS_2)) %>%
  	dplyr::mutate(sRSS_2=sum(RSS_2)) %>%
	# Output
  	dplyr::mutate(R2_2=1-sRSS_2/sTSS_2)

    list(r2reg=unique(dfc$R2_1),receil=unique(dfc$R2_2),dfcopy=dfc)
}

#' @export SimVoterdatabase
SimVoterdatabase <- setRefClass("SimVoterdatabase",fields=
				   list(
					ballcous='data.frame',
					r2dflook='list',
					htmlr2='data.frame',
					ggplr2='list'
				   )
)

SimVoterdatabase$methods(initialize=function(initdf=NULL){
  ballcous <<- Countingprocess(initdf)$sdfc
})
SimVoterdatabase$methods(r2sim=function(rept=10,form=1)
{
    #$standard
    #[1] "x"     "y"     "zeta"  "alpha" "lamda"
    #$hybrid
    #[1] "g"     "h"     "Gamma" "alpha" "Omega"
    #$opposition
    #[1] "m"     "n"     "xi"    "lamda" "Omega"
    # Fixed
    srs <- list(st=c('Omega','x','y'),hy=c('lamda','h','g'),op=c('Omega','m','n'))[[form]]
    v_nprec <- length(ballcous$P)
    v_regs <- c(mean(ballcous$R)/1000,sd(ballcous$R)/1000) #c(3.15,0.25)
    v_minmax <- range(ballcous$R)
    v_turn <- c(mean(ballcous$R/ballcous$Z),sd(ballcous$R/ballcous$Z)) #c(0.5,0.01)
    # Form dependent
    v_Invper <- c(mean(ballcous[[srs[1]]]),sd(ballcous[[srs[1]]]))
    v_u <- c(mean(ballcous[[srs[2]]]),sd(ballcous[[srs[2]]]))
    v_dv <- c(mean(ballcous[[srs[[3]]]]-ballcous[[srs[[2]]]]),sd(ballcous[[srs[[3]]]]-ballcous[[srs[[2]]]]))
    # R2 calculations
    tf <- replicate(rept, r2simn(nprec=v_nprec,regs=v_regs,minmax=v_minmax,turn=v_turn,Invper=v_Invper,u=v_u,dv=v_dv))
    dfgp <- data.frame(r2a=unlist(tf[seq(1,length(tf),3)]),r2b=unlist(tf[seq(2,length(tf),3)])) %>% mutate(perc = ntile(r2a, 100))
    # Input DF2
    percentiles <- c(90, 95, 99)
    nstd <- c(1,2,5)
    std <- mean(dfgp$r2a)+nstd*sd(dfgp$r2a)
    perc1 <- quantile(dfgp$r2a,probs = percentiles / 100)
    perc2 <- quantile(dfgp$r2b,probs = percentiles / 100)
    percdf <- data.frame(perc1,perc2,nstd,std) %>% data.table::setnames(c("Perc r2a","Perc r2b","Nstd","Vstd"))
    r2dflook <<- list(dfgp,percdf)
})
SimVoterdatabase$methods(htmltable=function(){

  htmlr2 <<- r2dflook[[1]] #kableExtra::kbl() #%>% kableExtra::kable_paper(full_width = F)

})
SimVoterdatabase$methods(gghist=function(){
  dfgp <- r2dflook[[1]] %>% tidyr::pivot_longer(cols=c("r2a","r2b")) %>% dplyr::arrange(name,perc)
  percd <- r2dflook[[2]]
    ggplot(dfgp,aes(x=value, fill=name)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = "histogram of values by category", x = "value", y = "count") +
    #geom_vline(xintercept = as.numeric(percd[1,1]), linetype = "dashed", color = "blue") +
    #geom_vline(xintercept = as.numeric(percd[2,1]), linetype = "dashed", color = "blue") +
    #geom_vline(xintercept = as.numeric(percd[3,1]), linetype = "dashed", color = "blue") +
    #geom_vline(xintercept = as.numeric(percd[3,4]), linetype = "solid", color = "red") +
    #geom_label(y=0,x=as.numeric(percd[1,1]),label="*",geom="label") +
    #geom_label(y=0,x=as.numeric(percd[2,1]),label="**",geom="label") +
    #geom_label(y=0,x=as.numeric(percd[3,1]),label="***",geom="label") +
    theme_minimal() +
    scale_fill_manual(values = c("#0072b2", "#e69f00"))  # set fill colors
})
#########################################################################################################################################
#########################################################################################################################################
##' @export Rall
Rall <- function(sel=c(1,2,3)){
  Rxy <- function(rad) {
    matrix(c(cos(rad), sin(rad), 0,
             -sin(rad), cos(rad), 0,
             0, 0, 1), ncol = 3)
}
Rxz <- function(rad) {
    matrix(c(cos(rad), 0, sin(rad),
             0, 1, 0,
            -sin(rad), 0, cos(rad)), ncol = 3)
}
Ryx <- function(rad) {
    matrix(c(cos(rad), sin(rad), 0,
             -sin(rad), cos(rad), 0,
             0, 0, 1), ncol = 3)
}
Ryz <- function(rad) {
    matrix(c(1, 0, 0,
             0, cos(rad), sin(rad),
             0, -sin(rad), cos(rad)), ncol = 3)
}
Rzx <- function(rad) {
   matrix(c(cos(rad), 0, sin(rad),
             0, 1, 0,
            -sin(rad), 0, cos(rad)), ncol = 3)
}
Rzy <- function(rad) {
    matrix(c(1, 0, 0,
             0, cos(rad), sin(rad),
             0, -sin(rad), cos(rad)), ncol = 3)
  }
    allrot <- list(Rxy,Rxz,Ryx,Ryz,Rzx,Rzy)[sel]
}

#' @export erotation
erotation <-function(
		     dfe=NULL,
		     selvar=NULL,
                     rpar=c(theta=0.2509451852,phi=0.8685213889,rho=0.2020759661),
		     rs=c(1,4,2),
                     mvec=NULL,
		     slice=20
		     ){

  if (is.null(mvec)) 'ho' else 'abc'
  Ralv <- Rall(sel=rs)
  rofc <<- dfe %>%
    dplyr::select(P,all_of(selvar)) %>%
    dplyr::arrange(P) %>%
    # Standardize variable names
    dplyr::mutate(ui=.[[2]]) %>%
    dplyr::mutate(vi=.[[3]]) %>%
    dplyr::mutate(wi=.[[4]]) %>%
    dplyr::mutate(m1=cos(rpar[1]),m2=cos(rpar[2]),m3=cos(rpar[3])) %>%
    dplyr::mutate(n1=sin(rpar[1]),n2=sin(rpar[2]),n3=sin(rpar[3])) %>%
    # Abc
    dplyr::mutate(st1=rs[1]) %>%
    dplyr::mutate(st2=rs[2]) %>%
    dplyr::mutate(st3=rs[3]) %>%
    # Euler-rotation
    dplyr::mutate(mu=if (is.null(mvec)) mean(ui) else mvec[1]) %>%
    dplyr::mutate(mv=if (is.null(mvec)) mean(vi) else mvec[2]) %>%
    dplyr::mutate(mw=if (is.null(mvec)) mean(wi) else mvec[3]) %>%
    dplyr::mutate(u0=ui-mu) %>%
    dplyr::mutate(v0=vi-mv) %>%
    dplyr::mutate(w0=wi-mw) %>%
    ##
    dplyr::mutate(u1=Ralv[[1]](rpar[1])[1,1]*u0+Ralv[[1]](rpar[1])[1,2]*v0+Ralv[[1]](rpar[2])[1,3]*w0) %>%
    dplyr::mutate(v1=Ralv[[1]](rpar[1])[2,1]*u0+Ralv[[1]](rpar[1])[2,2]*v0+Ralv[[1]](rpar[2])[2,3]*w0) %>%
    dplyr::mutate(w1=Ralv[[1]](rpar[1])[3,1]*u0+Ralv[[1]](rpar[1])[3,2]*v0+Ralv[[1]](rpar[2])[3,3]*w0) %>%
    ##
    dplyr::mutate(u2=Ralv[[2]](rpar[2])[1,1]*u1+Ralv[[2]](rpar[2])[1,2]*v1+Ralv[[2]](rpar[2])[1,3]*w1) %>%
    dplyr::mutate(v2=Ralv[[2]](rpar[2])[2,1]*u1+Ralv[[2]](rpar[2])[2,2]*v1+Ralv[[2]](rpar[2])[2,3]*w1) %>%
    dplyr::mutate(w2=Ralv[[2]](rpar[2])[3,1]*u1+Ralv[[2]](rpar[2])[3,2]*v1+Ralv[[2]](rpar[2])[3,3]*w1) %>%
    ##
    dplyr::mutate(x=Ralv[[3]](rpar[3])[1,1]*u2+Ralv[[3]](rpar[3])[1,2]*v2+Ralv[[3]](rpar[3])[1,3]*w2) %>%
    dplyr::mutate(y=Ralv[[3]](rpar[3])[2,1]*u2+Ralv[[3]](rpar[3])[2,2]*v2+Ralv[[3]](rpar[3])[2,3]*w2) %>%
    dplyr::mutate(z=Ralv[[3]](rpar[3])[3,1]*u2+Ralv[[3]](rpar[3])[3,2]*v2+Ralv[[3]](rpar[3])[3,3]*w2) %>%
    dplyr::mutate(slide=floor(z*50))
}

#' @export ballcount
ballcount <- function(ballotsdf=NULL,se=se){
  # Assigning model equations
  sdfc <<- ballotsdf %>%
    #dplyr::select(P,all_of(selvar))
    dplyr::mutate(Z=S+T+U+V) %>%
    dplyr::mutate(O=R-Z) %>%
    dplyr::mutate(x=pareq(se[['x_s']][1],as.list(.[,]))) %>%
    dplyr::mutate(y=pareq(se[['y_s']][1],as.list(.[,]))) %>%
    dplyr::mutate(g=pareq(se[['g_h']][1],as.list(.[,]))) %>%
    dplyr::mutate(h=pareq(se[['h_h']][1],as.list(.[,]))) %>%
    dplyr::mutate(m=pareq(se[['m_o']][1],as.list(.[,]))) %>%
    dplyr::mutate(n=pareq(se[['n_o']][1],as.list(.[,]))) %>%
    dplyr::mutate(alpha=pareq(se[['alpha_s']][1],as.list(.[,]))) %>%
    dplyr::mutate(zeta=pareq(se[['zeta_s']][1],as.list(.[,]))) %>%
    dplyr::mutate(lamda=pareq(se[['lamda_s']][1],as.list(.[,]))) %>%
    dplyr::mutate(Omega=pareq(se[['Omega_h']][1],as.list(.[,]))) %>%
    dplyr::mutate(Gamma=pareq(se[['Gamma_h']][1],as.list(.[,]))) %>%
    dplyr::mutate(xi=pareq(se[['xi_o']][1],as.list(.[,])))
    #!%>% na.omit()
}
#' @export pareq
pareq <- function(ste='(x + y*zeta)/(zeta + 1)',lv=list(x=0.75,y=0.25,zeta=1)){
	eval(parse(text=ste),lv)
}

############################################################################################################################################################
#' @export Countingprocess
Countingprocess <- setRefClass("Countingprocess",
			       fields=list(sdfc='data.frame',
					   rdfci='data.frame',
					   rdfc='data.frame',
					   rofc='data.frame',
                                           rdfce='data.frame',
					   quintile='data.frame',
					   desms='data.frame',
					   r2list='list',
					   predet='list',
					   sumreg='vector',
					   psel='vector',
					   polyc='list',
					   radpar='vector',
					   parameters='list',
					   preend='list',
					   parampre='data.frame',
                                           rotplotly='list',
					   se='list',
					   lx='list',
					   pl_2dsort='list',
					   pl_corrxy='list',
					   pl_rescro='list',
					   pl_3d_mani='list',
					   all_pl_3d_mani='list',
					   enf='list',
					   mansysl='list',
					   gensysl='list',
					   exnrs='vector',
					   allvar='list',
					   loss_df='data.frame'
					   ))
Countingprocess$methods(initialize=function(sdfinp=NULL,
					   selvar=c('P','R','S','T','U','V'),
					   polyn=9,
					   sortby=alpha
					   ){
  parameters <<- stickers[['parameters']]
  se <<- eqpar$meqs
  lx <<- eqpar$meql
  ils <- c('S','T','U','V')
  sdfc <<- ballcount(dplyr::select(sdfinp,all_of(selvar)),se=se)
  rdfci <<- rdfc <<- sdfc %>%
    dplyr::arrange(alpha) %>%
    dplyr::mutate(pri=dplyr::row_number()/length(P)) %>%
    dplyr::relocate(pri,.before=P) %>%
    dplyr::relocate(Z,.after=O)
    #dplyr::arrange(P)

  ## Polynom
  pnset <- min(length(rdfci$pri)-1,polyn)
  ### Init values standard form
  polyc[[1]] <<- stats::lm(rdfci$alpha ~ poly(rdfci$pri, pnset, raw=TRUE))
  ### Init valuesstats:: hybrid form
  polyc[[2]] <<- stats::lm(rdfci$alpha ~ poly(rdfci$pri, pnset, raw=TRUE))
  ##### Init valustats::es opposition form
  polyc[[3]] <<- stats::lm(rdfci$lamda ~ poly(rdfci$pri, pnset, raw=TRUE))

})
Countingprocess$methods(r2siminput=function(form=1,latest=0)
{
  rdf <- list(rdfci,rdfc)[[ifelse(latest==0,1,2)]]
  pm <- parameters[[form]]
  regs <- c(mean(rdf$R),sd(rdf$R))
  turn <- c(mean(rdf$V/rdf$R),sd(rdf$V/rdf$R))
  minmax <- c(min(rdf$R),max(rdf$R))
  sv <- c(mean(rdf[[pm[[1]]]]),sd(rdf[[pm[[1]]]]))
  dsv <- c(mean(rdf[[pm[[2]]]])-mean(rdf[[pm[[1]]]]),sd(rdf[[pm[[2]]]]-rdf[[pm[[1]]]]))
  Perc <- list(s=c(mean(rdf$Omega),sd(rdf$Omega)),h=c(mean(rdf$Omega),sd(rdf$Omega)),o=c(mean(rdf$xi),sd(rdf$xi)))
  nprec <- length(rdf$P)
  r2list <<- list(form=form,turn=turn,regs=regs,minmax=minmax,s=sv,ds=dsv,Perc=Perc[[form]],nprec=nprec)
})
Countingprocess$methods(descriptive=function(form=1){
  flp <- c(unname(unlist(ManifoldDestiny::stickers[['parameters']])))
  co <- c('S','T','U','V','R','Z')
  sdv <- as.data.frame(sapply(dplyr::select(rdfc,dplyr::all_of(co)),mean))
  mdv <- as.data.frame(sapply(dplyr::select(rdfc,dplyr::all_of(flp)),mean))
  sta <- as.data.frame(sapply(dplyr::select(rdfc,dplyr::all_of(c(co,flp))),sd))
  desms <<- data.frame(variable=rownames(sta),mean=c(sdv[,1],mdv[,1]),std=sta[,1])
})

Countingprocess$methods(rotation=function(selvar=c('x','y','alpha'),
				     rpar=c(theta=0,phi=0,rho=0),
				     rs=c(0,0,0),
				     mmeanv=NULL,
			             sli=NULL)
				     {
  dfe <- erotation(dfe=rdfc,selvar=selvar,rpar=rpar,rs=rs,mvec=mmeanv,slice=20)
  rdfc <<- dfe

})



Countingprocess$methods(plext=function(){
  #! automized
  rdfce <<- rdfc %>%
    dplyr::mutate(alpha2=alpha*alpha,alpha3=alpha*alpha*alpha) %>%
    dplyr::mutate(alphah=alpha*h,alphah2=alpha*h*h,alpha2h=alpha*alpha*h) %>%
    dplyr::mutate(h2=h*h,gh=g*h,g2h=g*g*h,g2=g*g,g3=g*g*g,h3=h*h*h,h2g=h*h*g,hg2=h*g*g) %>%
    dplyr::mutate(gh2=g*h*h) %>%
    dplyr::mutate(g4=g*g*g*g) %>%
    dplyr::mutate(g3h=g*g*g*h) %>%
    dplyr::mutate(g2h2=g*g*h*h) %>%
    dplyr::mutate(g2h2=g*g*h*h) %>%
    dplyr::mutate(gh3=g*h*h*h) %>%
    dplyr::mutate(g4h=g*g*g*g*h) %>%
    dplyr::mutate(h4=h*h*h*h) %>%
    dplyr::mutate(xy=x*y)
})

Countingprocess$methods(purging=function(mdprg=NULL,pri=0){
  rdfv <- rdfci %>%
    dplyr::arrange(P) %>%
    # Filter
    ## Number of ballots
    dplyr::filter(S>=mdprg$prg$stuv[1]) %>%
    dplyr::filter(T>=mdprg$prg$stuv[2]) %>%
    dplyr::filter(U>=mdprg$prg$stuv[3]) %>%
    dplyr::filter(V>=mdprg$prg$stuv[4]) %>%
    ## Percentages
    dplyr::filter(if_all(c(alpha,x,y,g,h,m,n),~.>mdprg$prg$blup[1]&.<mdprg$prg$blup[2]))
    # Fit filter
    erdfv <- Estimation(rdfv)
    erdfv$regression(mdprg$sgs$eq)
    rdfc <<- erdfv$predict_df %>%
            dplyr::mutate(pre_rnk=row_number(desc(deva))) %>%
            dplyr::arrange(pre_rnk) %>%
            #dplyr::filter(pre_rnk>regr[[2]]) %>% dplyr::filter(!P%in%pref) %>%
            dplyr::mutate(pri=row_number()/length(P)) %>%
            dplyr::arrange(P)
    #!Discarded
  if (pri==1) {print(dim(rdfci)); print(dim(rdfv)); print(dim(rdfc))}
})
#})
Countingprocess$methods(sortpre=function(form=1,
					 polyn=6,
					 sortby='alpha'
					 ){
  frmsel <- list(c(1,2,4,10,3),c(6,7,9,5,8),c(11,12,14,4,13),c(1,2,4,11,12,15))[[form]]
  selvar <- unname(unlist(parameters))[frmsel]
  psel <<- selvar[1:ifelse(form %in% 1:3,5,6)]
  proppar <- rev(selvar)[1]
  srdfc <- rdfc %>%
    dplyr::select(P,all_of(selvar)) %>%
    dplyr::arrange(alpha) %>%
    dplyr::mutate(pri=row_number()/length(P)) %>%
    dplyr::mutate(!!paste0(proppar,'_m'):=mean(!!rlang::sym(proppar))) %>%
    dplyr::mutate(!!paste0(proppar,'_mr'):=!!rlang::sym(proppar)-!!rlang::sym(paste0(proppar,'_m')))
    psel %>% purrr::map(function(x,df=srdfc,p=polyn){
        pred <- c(predict(lm(df[[x]] ~ stats::poly(df$pri,p, raw=TRUE))))
        res <- pred - df[[x]]
        data.frame(pred,res) %>% `colnames<-` (c(paste0(x,'_pred'),paste0(x,'_res')))
    }) %>% as.data.frame(.) -> predictor
  quintile <<- dplyr::bind_cols(srdfc, predictor)

  ## Comments needed
  #plso <- round(polynom::polynomial(unname(coef(polyc[[form]]))),3)
  #pintv <- polynom::integral(polynom::polynomial(plso),c(0,1))
  #plr2 <- round(cor(quintile[[paste0(sortby,'_pred')]],quintile[[sortby]])^2,4)
  #sumreg <<- list(poleq=paste0(plso),polint=pintv,R2=paste0(plr2))
})
Countingprocess$methods(mansys=function(sygen=NULL){
  mansysl <<- sygen
  sho <- c("_s","_h","_o")[[mansysl$frm]]
  allvar <<- list(pre=mansysl$pre,end=mansysl$end)
  exnrs <<- gsub('v',mansysl$pre[2], gsub('u',mansysl$pre[3],ManifoldDestiny::formpolv[mansysl$me[['plnr']]]))
  enf[[1]] <<- unname(stats::predict(polyc[[mansysl$frm]]))
  enf[[2]] <<- eqpar$meqs[[paste0(mansysl$pre[2],sho)]]
  enf[[3]] <<- py_genpolycoeff(exnrs[[1]],mansysl$pre[[1]],mansysl$pre[[3]])[[1]]
})
Countingprocess$methods(setres=function(czset=NULL,prnt=0){
  frp <- mansysl$frm
  if (!is.null(czset)) polyc[[frp]][[1]][[1]] <<- czset
  if (prnt==1) {
    vec <- unname(polyc[[frp]][[1]])
    print(polynom::integral(polynom::polynomial(vec),c(0,1)))
  }
})
Countingprocess$methods(manimp=function(init_par=NULL,man=TRUE,wn=c(0,0)){
  ## Variables
  allvec <- c(unlist(allvar$pre),unlist(allvar$end))
  sho <- c("_s","_h","_o")[[mansysl$frm]]
  altvec <- paste0(as.vector(unlist(allvar)),sho)
  endp <- paste0(allvec,sho)[c(4,5)]
  pre1 <- enf[[1]]
  pre2 <- enf[[2]]
  pre3 <- enf[[3]]
  end1 <- se[[endp[1]]][2]
  end2 <- se[[endp[2]]][2]
  lstr   <- paste0("(",allvec[4],"-",altvec[4],")^2")
  lof <- function(kvec=NULL){
    loss_df <<- rdfci %>%
      dplyr::select(P,S,T,U,V,R,Z,all_of(allvec)) %>%
      #dplyr::select(pri,P,S,T,U,V,R,Z,all_of(allvec)) %>%
      data.table::setnames(allvec,altvec) %>%
      ### Parameters
      dplyr::mutate(!!!kvec) %>%
      ### Presetting the first variables
      dplyr::mutate(!!allvec[1]:=enf[[1]]) %>%
      ### Presetting second variable
      dplyr::mutate(!!allvec[2]:=pareq(pre2,c(as.list(.[,])))) %>%
      ### Presetting the Manifold object
      dplyr::mutate(!!allvec[3]:=manobj(enfl=pre3,.[,],allvec[3])) %>%
      ### Adding some noise
      dplyr::mutate(!!allvec[3]:=!!rlang::sym(allvec[3])*(1+rnorm(n(),wn[1],wn[2]))) %>%
      ### Backsolving for the two remaining parameter
      dplyr::mutate(!!allvec[4]:=pareq(end1,c(as.list(.[,])))) %>%
      dplyr::mutate(!!allvec[5]:=pareq(end2,c(as.list(.[,])))) %>%
      dplyr::mutate(LSV:=pareq(lstr,c(as.list(.[,])))) %>%
      #### Backsolving for ballots
      data.table::setnames(c("S","T","U","V"),c("S_o","T_o","U_o","V_o")) %>%
      dplyr::mutate(S=floor(pareq(se[[paste0('S',sho)]][2],as.list(.[]))))  %>%
      dplyr::mutate(T=floor(pareq(se[[paste0('T',sho)]][2],as.list(.[]))))  %>%
      dplyr::mutate(U=floor(pareq(se[[paste0('U',sho)]][2],as.list(.[]))))  %>%
      dplyr::mutate(V=floor(pareq(se[[paste0('V',sho)]][2],as.list(.[]))))  %>%
      dplyr::rename(Z_o=Z) %>%
      dplyr::mutate(Z=S+T+U+V)
      ## Loss value
  }
  lv <- function(param=NULL){
    lofdf <- lof(kvec=param)
    nrv <- sum(dplyr::select(lofdf, S, T, U, V) < 0)
    clvl <- sum(lofdf$LSV)+ifelse(nrv>0,nrv*sum(loss_df$LSV),0)
  }
  if (man) {
    man_lores <- lv(param=init_par)
  } else {
    opt_lores <- optim(par = init_par, fn = lv, method='L-BFGS-B',lower=c(k0=0,k1=0,k2=0),upper=c(k0=0,k1=0,k2=0))
  }
  rdfc <<- dplyr::select(loss_df,P,R,S,T,U,V) %>% ballcount(se=se)
})

