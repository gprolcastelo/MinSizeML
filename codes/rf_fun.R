########################
# Random Forest #
########################

minimum_sample_rf <- function(X,Y,p_vec,thr_acc){
  
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
  
  # Initialize vectors to save properties:
  acc_vec <- c()
  cohen_vec <- c()
  training_set_size <- c()
  
  # Loop through the data sizes given by p_vec
  for (i in p_vec) {
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
    
    # Save accuracy: proportion of correctly classified samples
    acc_vec <- c(acc_vec,mean(predict_rf == testY))
    
    # Save Cohen's kappa:
    cohen_vec <- c(cohen_vec,cohen_kappa(predict_rf,testY))
    
    # Save size of training set:
    training_set_size <- c(training_set_size,length(trainIndex))
    
  }
  
  # Create dataframe with saved vectors:
  df_acc_cohen <- data.frame(training_set_size,acc_vec,cohen_vec)
  
  # Fit non-linear regression to get the accuracy fit.
  # Formula given by Figueroa et al 2012
  fit_accuracy <- nls(acc_vec~(1-a)-b*training_set_size^c,
                      data = df_acc_cohen, 
                      start = list(a=0.5,b=0.5,c=-0.5),
                      control = nls.control(maxiter = 100, tol = 1e-8),
                      algorithm = "port"
                      )  
  
  
  # Plot accuracy vs size of training set.
  # Circles = calculated values of accuracy given a sample size.
  # Lines = fitted data.
  plot(df_acc_cohen$training_set_size,
       df_acc_cohen$acc_vec,
       xlab = "Training set size", ylab = "Accuracy of prediction")
  lines(df_acc_cohen$training_set_size,
        predict(fit_accuracy,df_acc_cohen$training_set_size))
  
  # Coefficients: 
  a_fit <- summary(fit_accuracy)$coefficients[,1][[1]]
  b_fit <- summary(fit_accuracy)$coefficients[,1][[2]]
  c_fit <- summary(fit_accuracy)$coefficients[,1][[3]]
  
  # Calculate minimum sample size for given threshold accuracy:
  min_sam_size <- abs(((1-a_fit-thr_acc)/b_fit)^(1/c_fit))
  
  print("For minimum accuracy:")
  print(thr_acc)
  print("Minimum sample size:")
  print(min_sam_size)
  
}