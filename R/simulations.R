#' @export ballcastsim
ballcastsim <- function(
	dfm=dfm,
	probw=c(0.5,0), 	
	probva=c(0.7,0.2,0.03,0.00), 				
	probvb=c(0.7,0.2,0.03,0.00), 
	ztech=c(0,0)
			){

  # Simulate prob
  probvrnd <<- dfm %>%
    dplyr::mutate(ZV=rnorm(n(),probw[1],probw[2])) %>%
    dplyr::mutate(N=runif(n(),ztech[1],ztech[2])) %>%
    dplyr::mutate(p3=(1-ztech)*(1-pmax(0, pmin(1,rnorm(n(),probva[1],probva[3]))))) %>%
    dplyr::mutate(p6=(1-ztech)*(1-pmax(0, pmin(1,rnorm(n(),probvb[1],probvb[3]))))) %>%
    dplyr::mutate(p2=(1-p3)*pmax(0, pmin(1,rnorm(n(),probva[2],probva[4])))) %>%
    dplyr::mutate(p5=(1-p6)*pmax(0, pmin(1,rnorm(n(),probvb[2],probvb[4])))) %>%
    dplyr::mutate(p1=1-p2-p3) %>%
    dplyr::mutate(p4=1-p5-p6) %>%
    dplyr::select(P,ZV,N,p1,p2,p3,p4,p5,p6)

  ballcodf <- dfm %>% dplyr::left_join(probvrnd,by='P') %>% base::split(.$P) %>%
       purrr::map(function(x){
  # Assigning voters in each precinct
  cp <- stats::rbinom(x$R,1,x$ZV) 
  ## Setting up vector frame
  dc <- data.frame(P=x$P,ZV=x$ZV,R=x$R,C=cp,p1=x$p1,p2=x$p2,p3=x$p3,p4=x$p4,p5=x$p5,p6=x$p6) %>%
    dplyr::mutate(Id=row_number()) %>% 
    dplyr::relocate(Id,.before=P)  %>%
    dplyr::group_by(Id) %>% 
    dplyr::mutate(V=ifelse(C==1,sample(1:3,1,prob=c(p1,p2,p3)),sample(4:6,1,prob=c(p4,p5,p6)))) %>%
    dplyr::ungroup() 
  })  %>%
  dplyr::bind_rows(.) %>%
  dplyr::mutate(Id=row_number(P)) %>% 
  dplyr::relocate(Id,.before=P) %>% 
  dplyr::mutate(S=ifelse(V==1,1,0)) %>% 
  dplyr::mutate(T=ifelse(V==4,1,0)) %>%  
  dplyr::mutate(U=ifelse(V==2,1,0)) %>%
  dplyr::mutate(V=ifelse(V==5,1,0)) %>%
  dplyr::arrange(P) %>%  dplyr::group_by(P)  %>% 
  dplyr::select(c('P','R','S','T','U','V')) %>%
  dplyr::mutate(S=sum(S),T=sum(T),U=sum(U),V=sum(V)) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(Z=sum(S+T+U+V)) %>%
  dplyr::ungroup() 
}


