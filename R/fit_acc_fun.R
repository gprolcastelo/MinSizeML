#' Inverse function of the fitted accuracy or kappa
#' 
#' Used to calculate the sample size, given the parameters and a metric value.
#' 
#' @param a,b,c Coefficient from fitted formula
#' @param x Metric value (between 0 and 1)
#' @return Corresponding sample size given the metric and parameters, solving
#' for the fitted formula.
#' 
fit_acc_fun <- function(a,b,c,x){
  return(abs(((1-a-x)/b)^(1/c)))
}