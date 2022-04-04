fit_acc_fun <- function(a,b,c,x){
  return(abs(((1-a-x)/b)^(1/c)))
}