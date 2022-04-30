#' Performs profile anaylsis on multivsriate data with k different groups
#'
#' @param Data Dataframe with first column as category and the rest as mesured varaibles
#' @param Test The type of profile analysis you want to conduct (P = parallelism, C = coincidence, F = flatness)\
#' @param K Number of categories in data
#'
#' @return MANOVA summary for desired profile test
#'
#' @examples
#' one.sample.hotelling(data, Test = "p", K = 3)
#'
#' @export

k.sample.profile = function(Data,Test="P",K=1)
{
  ### Data: first column categorical variable, next columns, sequential measurements
  ### Test: which type of test, p=parallelism,c=coincidence,f=flatness
  ### K: number of groups, default is 1
  nrow=nrow(Data)
  ncol=ncol(Data)
  if(K==1) {
    Data <- as.matrix(Data)
    C=matrix(0,ncol-1,ncol)
    for(itor in 1:ncol-1){
      for(itorat in 1:ncol){
        if(itorat-itor==0){
          C[itor,itorat]=1
        }
        if(itorat-itor==1){
          C[itor,itorat]=-1
        }}}
    trans.data <- Data%*%t(C)
    one.samp.profile <- manova(trans.data~1)
    output=(summary(one.samp.profile,intercept=T,test="Hotelling"))
  }
  if((Test=="P" | Test=="p") & K!= 1){
    crow=ncol-2
    ccol=ncol-1
    C=matrix(0,crow,ccol)
    for(itor in 1:crow){
      for(itorat in 1:ccol){
        if(itorat-itor==0){
          C[itor,itorat]=1
        }
        if(itorat-itor==1){
          C[itor,itorat]=-1
        }}}
    Y <- as.matrix(Data[,2:ncol])
    trans.data <- Y%*%t(C)
    two.samp.profile <- manova(trans.data~as.factor(Data[,1]))
    output=summary(two.samp.profile,test="Wilks")
  }
  if((Test=="C" | Test=="c") & K!= 1){
    j <- rep(1,times=ncol-1)
    Y <- as.matrix(Data[,2:ncol])
    trans.data <- Y%*%j
    coin.test <- aov(trans.data~as.factor(Data[,1]))
    output=summary(coin.test,test=F)
  }
  if((Test=="F" | Test=="f") & K!= 1){
    crow=ncol-2
    ccol=ncol-1
    C=matrix(0,crow,ccol)
    for(itor in 1:crow){
      for(itorat in 1:ccol){
        if(itorat-itor==0){
          C[itor,itorat]=1
        }
        if(itorat-itor==1){
          C[itor,itorat]=-1
        }}}
    Y <- as.matrix(Data[,2:ncol])
    trans.data <- Y%*%t(C)
    test.flat <- manova(trans.data~1+as.factor(Data[,1]))
    output=summary(test.flat,intercept=T,test="Hotelling")
  }
  return(output)
}
