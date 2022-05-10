#' CI for minimum sample size
#' 
#' Find the confidence interval for minimum sample size, given a fitted accuracy/Kappa curve
#' 
#' @import stats
#' @param tr_set_size Training set size
#' @param prediction.ci Confidence Interval for the fitted curve, made up of three variables.
#' @param thr_acc Threshold of the chosen metric.
#' @param min_sam_size Estimated minimum sample size.
#' @param fit_accuracy Fitted object from nls.
#' @return CI based on fitted upper and lower lines
#' 
CI_MinimumSampleSize_fun <- function(X,
                                     prediction.ci,
                                     formula_rhs,
                                     start_parameters,
                                     thr_acc,
                                     min_sam_size,
                                     fit_accuracy){
  
  
  # Unpack prediction.ci: 
  predictY<-prediction.ci[,1];
  predictY.lw<-prediction.ci[,2];
  predictY.up<-prediction.ci[,3];
  
  # Coefficients: 
  a_fit <- summary(fit_accuracy)$coefficients[,1][[1]]
  b_fit <- summary(fit_accuracy)$coefficients[,1][[2]]
  c_fit <- summary(fit_accuracy)$coefficients[,1][[3]]
  
  
  # d) fitting upper and lower curves:
  
  # d).1. LB:
  fit_up <- stats::nls(as.formula(paste("predictY.up~",formula_rhs,sep = "")),
                start = start_parameters,
                control = nls.control(maxiter = 100, tol = 1e-8),
                algorithm = "port"
  )
  
  a_up <- summary(fit_up)$coefficients[,1][[1]]
  b_up <- summary(fit_up)$coefficients[,1][[2]]
  c_up <- summary(fit_up)$coefficients[,1][[3]]
  
  LB_d <- fit_acc_fun(a_up,b_up,c_up,thr_acc)
  
  # d).2. UB:
  
  fit_lw <- nls(as.formula(paste("predictY.lw~",formula_rhs,sep = "")),
                start = start_parameters,
                control = nls.control(maxiter = 100, tol = 1e-8),
                algorithm = "port"
  )
  
  a_lw <- summary(fit_lw)$coefficients[,1][[1]]
  b_lw <- summary(fit_lw)$coefficients[,1][[2]]
  c_lw <- summary(fit_lw)$coefficients[,1][[3]]
  
  UB_d <- fit_acc_fun(a_lw,b_lw,c_lw,thr_acc)
  
  # d).3. CI: vector of lower and upper bounds:
  CI_min_sam_size_d <- c(LB_d,UB_d)
  
  #return_CI <- list("a" = CI_min_sam_size_a, "b" = NULL, "c" = CI_min_sam_size_c, "d" = CI_min_sam_size_d)
  
  return(CI_min_sam_size_d)
  
}