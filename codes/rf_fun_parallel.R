########################
# Random Forest #
########################

minimum_sample_rf_parallel <- function(X,Y,p_vec,thr_acc,n.cores){
  
  # For paralelization with foreach:
  n.cores <- parallel::detectCores() - 2

  #create the cluster
  my.cluster <- parallel::makeCluster(
    n.cores,
    type = "PSOCK"
  )
  
  #register it to be used by %dopar%
  doParallel::registerDoParallel(cl = my.cluster)
  
  #check if it is registered (optional)
  # foreach::getDoParRegistered()
  
  #how many workers are available? (optional)
  # foreach::getDoParWorkers()
  
  
  
  # Control to get Y as a vector rather than a list:
  if (typeof(Y)=="list") {
    Y <- unlist(Y)
  }
  
  # Parameter tuning:
  # Define parameters to use cross-validation in the train() function:
  train_control <- trainControl(## 5-fold CV
    method = "repeatedcv",
    number = 5,
    ## repeated 1 times
    repeats = 1)
  
  # Loop through the data sizes given by p_vec
  
  x_foreach <- foreach(
    i = p_vec,
    .combine = 'rbind'
  ) %dopar% {
    library(caret)
    source("./codes/cohen_kappa_fun.R")
    
    # Split data for training set (keep a portion p=i):
    trainIndex <- createDataPartition(Y, p = i, 
                                      list = FALSE, 
                                      times = 1)
    
    # Input data split:
    trainX <- X[trainIndex,]
    testX <- X[-trainIndex,]
    trainY <- Y[trainIndex]
    testY <- Y[-trainIndex]
    
    # Create grid for the train() function
    rfGrid <- expand.grid(mtry=round(c(2, sqrt(ncol(trainX))/2,
                                       seq(sqrt(ncol(trainX)), 
                                           ncol(trainX), 
                                           length.out = 4))))
    
    # Training data:
    rfFit <- train(x = trainX, y = trainY,
                   method = "rf", 
                   trControl = train_control,
                   metric = "Kappa",
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = TRUE, 
                   ## Now specify the exact models 
                   ## to evaluate:
                   tuneGrid = rfGrid)
    
    # Predictions:
    predict_rf <- predict(rfFit, newdata = testX)
    
    # Return when using paralelization with foreach:
    return(c(length(trainIndex),  # save size of training set
             mean(predict_rf == testY),  # save accuracy
             cohen_kappa(predict_rf,testY)  # save Cohen's kappa
             )
           )
    
  }
  
  # Create dataframe with saved vectors:
  df_acc_cohen <- data.frame(x_foreach, row.names = NULL)
  names(df_acc_cohen) <- c("training_set_size","acc_vec","cohen_vec")
  
  # Fit non-linear regression to get the accuracy fit.
  # Formula given by Figueroa et al 2012
  fit_accuracy <- nls(acc_vec~(1-a)-b*training_set_size^c,
                      data = df_acc_cohen, 
                      start = list(a=0.5,b=0.5,c=-0.5),
                      control = nls.control(maxiter = 100, tol = 1e-8),
                      algorithm = "port"
  )  
  # Coefficients: 
  a_fit <- summary(fit_accuracy)$coefficients[,1][[1]]
  b_fit <- summary(fit_accuracy)$coefficients[,1][[2]]
  c_fit <- summary(fit_accuracy)$coefficients[,1][[3]]
  
  # Confidence intervals for fitted curve (from Figueroa 2012 appendix):
  se.fit <- sqrt(apply(fit_accuracy$m$gradient(),
                       1,
                       function(x) sum(vcov(fit_accuracy)*outer(x,x)))
  );
  prediction.ci <- predict(fit_accuracy,x=df_acc_cohen$training_set_size) + outer(se.fit,qnorm(c(.5, .025,.975)))
  
  predictY<-prediction.ci[,1];
  predictY.lw<-prediction.ci[,2];
  predictY.up<-prediction.ci[,3];
  
  # Plot accuracy vs size of training set.
  # Circles = calculated values of accuracy given a sample size.
  # Lines = fitted data.
  
  # # Calculated accuracy vs sample size
  plot(df_acc_cohen$training_set_size,
       df_acc_cohen$acc_vec,
       xlab = "Training set size", ylab = "Accuracy of prediction")
  # # Middle line:
  lines(df_acc_cohen$training_set_size,
        predict(fit_accuracy,df_acc_cohen$training_set_size))
  
  # # Upper line:
  lines(df_acc_cohen$training_set_size,
        predictY.up, col = "blue")
  
  # # Lower line:
  lines(df_acc_cohen$training_set_size,
        predictY.lw, col = "red")
  
  # Minimum sample size calculation:
  # Simply with the formula, solving for new_data:
  min_sam_size <- fit_acc_fun(a_fit,b_fit,c_fit,thr_acc)
  
  # Confidence interval for minimum sample size:
  CI_vec <- CI_MinimumSampleSize_fun(df_acc_cohen$training_set_size,
                                     prediction.ci,
                                     thr_acc,
                                     min_sam_size,
                                     fit_accuracy,
                                     w=0.005)
  
  # Print results.
  print("For minimum accuracy:")
  print(thr_acc)
  print("Minimum sample size:")
  print(min_sam_size)
  
  
  print("CI for minimum sample size: a)")
  print(CI_vec$a)
  print("CI for minimum sample size: b)")
  print(CI_vec$b)
  print("CI for minimum sample size: c)")
  print(CI_vec$c)
  print("CI for minimum sample size: d)")
  print(CI_vec$d)
  
}