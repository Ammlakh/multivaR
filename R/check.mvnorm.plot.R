#' Checks for multivariate normality in data
#'
#' @param data A dataframe with multiple quantitative variables
#'
#' @return A qq-plot for multivariate normality
#'
#' @examples
#' check.mvnorm.plot(iris[,1:4])
#'
#' @export


check.mvnorm.plot <- function(data) {
  data <- as.matrix(data)
  d2 <- 1:nrow(data)
  for (i in 1:nrow(data)) {
    d2[i] <- t(as.matrix(data[i,] -
                           apply(data,2,mean)))%*%solve(cov(data))%*%(as.matrix(data[i,]-apply(data,2,mean)))
  }
  p <- ((1:nrow(data))-.5)/nrow(data)
  plot(qchisq(p,ncol(data)),sort(d2),ylab="D^2 Values",xlab="Theoretical Chi-Square Quantiles",
       main="Chi-Square Probability Plot")
  abline(0,1)
}
