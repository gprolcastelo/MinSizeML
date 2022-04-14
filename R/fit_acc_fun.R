#' Inverse function of the fitted accuracy or kappa
#' 
#' Used to calculate the corresponding size, given the parameters and an accuracy
#' or Kappa value.
#' 
fit_acc_fun <- function(a,b,c,x){
  return(abs(((1-a-x)/b)^(1/c)))
}