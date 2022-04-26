#' Adds two to any number
#'
#' @param x A number
#'
#' @return A number
#' @examples
#' add_two(3)
#'
#' @export
powergraph <- function(n, d, alpha, alt){
  d = ((-d*1000):(d*1000))/1000
  power = pwr::pwr.t.test(n,d,alpha,type="one.sample",alternative = alt)$power
  df = data.frame(d, power)
  ggplot2::ggplot(df) + geom_line(aes(x=d, y=power)) +
    labs(x = "Effect Size", y = "power", title = "Power Function Graph") + theme_classic()
  }
