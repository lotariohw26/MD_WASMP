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


