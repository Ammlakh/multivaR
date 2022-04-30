#' Tests if a given sample covariance matrix is significantly different than a null
#'
#' @param S Sample covariance matrix from multivariate responses
#' @param sig.0 Hypothesized covariance matrix
#' @param N Sample size
#' @param K Number of groups in multivariate data
#'
#' @return Test statistics, p-values and df for covariance matrix test
#'
#' @examples
#' test.covmat(cov(iris[,1:4]), matrix(rep(0,16), ncol = 4), 150, 3)
#'
#' @export


covmat.test <- function(S, sig.0, N, k=1){
  # S is the sample covariance matrix or the pooled covariance matrix, S.pl
  # sig.0 is the hypothesized covariance matrix
  # N is the sample size of either the one group, or the total
  # sample size of all of the groups combined, in the case of S.pl
  # k is the number of groups, and is by default 1.
  p <- ncol(S)
  nu <- N - k
  evals <- eigen(S %*% solve(sig.0))$values
  u <- nu * (sum(evals - log(evals)) - p)
  u.prime <- (1-(1/(6*nu -1))*(2*p + 1 - 2/(p+1))) * u
  chi.df <- .5*p*(p+1)
  p.val.u <- 1 - pchisq(u, chi.df)
  p.val.u.prime <- 1 - pchisq(u.prime, chi.df)
  return(list(u = u, u.prime = u.prime, p.value.u = p.val.u,
              p.value.u.prime = p.val.u.prime, df = chi.df))
}
