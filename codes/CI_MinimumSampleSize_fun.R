CI_MinimumSampleSize_fun <- function(tr_set_size,prediction.ci,thr_acc,min_sam_size,fit_accuracy,w){
  
  # Unpack prediction.ci: 
  predictY<-prediction.ci[,1];
  predictY.lw<-prediction.ci[,2];
  predictY.up<-prediction.ci[,3];
  
  # Coefficients: 
  a_fit <- summary(fit_accuracy)$coefficients[,1][[1]]
  b_fit <- summary(fit_accuracy)$coefficients[,1][[2]]
  c_fit <- summary(fit_accuracy)$coefficients[,1][[3]]
  
  # a) Finding the same accuracy on the other two curves: 
  # CI_min_sam_size_a <- tr_set_size[c(which.min(abs(predictY.up-thr_acc)),
  #                                    which.min(abs(predictY.lw-thr_acc))
  # )]
  
  # # b) Geometry: find min_sam_size, then corresponding acc for the upper and lower fit,
  # # then infer from same formula used for min_sam_size
  # 
  # # b).1. Upper accuracy: corresponds to the value of predictY.up closest to min_sam_size
  # upper_acc <- predictY.up[which.min(abs(tr_set_size-min_sam_size))]
  # # If the upper accuracy is lower than given thr_acc, then get the next index:
  # if (upper_acc<thr_acc) {
  #   upper_acc <- predictY.up[which.min(abs(tr_set_size-min_sam_size))+1]
  # }
  # # Fit minimum sample size corresponding to upper accuracy (upper bound)
  # UB <- fit_acc_fun(a_fit,b_fit,c_fit,upper_acc)
  # 
  # # b).2. Lower accuracy: corresponds to the value of predictY.lw closest to min_sam_size
  # lower_acc <- predictY.lw[which.min(abs(tr_set_size-min_sam_size))]
  # # If the lower accuracy is higher than given thr_acc, then get the previous index:
  # if (lower_acc>thr_acc) {
  #   lower_acc <- predictY.lw[which.min(abs(tr_set_size-min_sam_size))-1]
  # }
  # # Fit minimum sample size corresponding to lower accuracy (lower bound)
  # LB <- fit_acc_fun(a_fit,b_fit,c_fit,lower_acc)
  # 
  # # b).3. CI: vector of lower and upper bounds:
  # CI_min_sam_size_b <- c(LB,UB)
  # 
  # c) Defining a w:
  # thr_acc+c(-1,1)*0.005
  # CI_min_sam_size_c <- fit_acc_fun(a_fit,b_fit,c_fit,
  #                                  thr_acc+c(-1,1)*w)
  
  # d) fitting upper and lower curves:
  
  # d).1. LB:
  fit_up <- nls(predictY.up~(1-a)-b*tr_set_size^c,
                start = list(a=0,b=1,c=-0.5),
                control = nls.control(maxiter = 100, tol = 1e-8),
                algorithm = "port"
  )
  
  a_up <- summary(fit_up)$coefficients[,1][[1]]
  b_up <- summary(fit_up)$coefficients[,1][[2]]
  c_up <- summary(fit_up)$coefficients[,1][[3]]
  
  LB_d <- fit_acc_fun(a_up,b_up,c_up,thr_acc)
  
  # d).2. UB:
  
  fit_lw <- nls(predictY.lw~(1-a)-b*tr_set_size^c,
                start = list(a=0,b=1,c=-0.5),
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