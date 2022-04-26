#' Performs a one sample hotelling t^2-test for given hypothesis
#'
#' @param data Matrix of vectors (variables) you want to test means of
#' @param mu.0 Hypothesized values of means
#'
#' @return F-stat and p-value for hotelling t^2 test
#'
#' @examples
#' one.sample.hotelling(iris[,1:4], c(5,3,4,1))
#'
#' @export

one.sample.hotelling <- function(Data,mu.0){
  #### One Sample Hotelling’s T squared test
  #### Data: matrix of the 2 vectors you want tested
  #### mu.0:vector of length p, with hypothesized values of mu
  Data=Data
  n=nrow(Data)
  p=ncol(Data)
  S <- cov(Data)
  y.bar <- apply(Data,2,mean)
  ybar.minus.mu <- y.bar - mu.0
  T.square <- n*t(ybar.minus.mu)%*%solve(S)%*%ybar.minus.mu
  F.stat <- round(((n-p)/(p*(n-1)))*T.square,3)
  P.value <- round(1-pf(F.stat,p,n-p),digits=5)
  Output <- paste("F=",F.stat,"P-Value=", P.value,sep=" ")
  return(Output)
}
