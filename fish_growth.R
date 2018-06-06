#' Fish Growth Rate
#'
#' This function computes growth rate of fish in environment with various temperatures measured in degrees Celsius. The third order polynomial function is from Bjoornsson et al., 2007
#' @param a is derived from Table 2, Bjoornsson et al., 2007
#' @param b is derived from Table 2, Bjoornsson et al., 2007
#' @param c is derived from Table 2, Bjoornsson et al., 2007
#' @param d is derived from Table 2, Bjoornsson et al., 2007
#' @author Frank Wang
#' @return Fish growth rate G(%/day)
#' @export

library(tidyverse)
fish_growth <- function(Temp){
  G = a + b*Temp + c*Temp^2 + d*Temp^3
  return(G)
}
