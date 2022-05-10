#' Algorithm for minimum sample size estimation in ML
#' 
#' This algorithm determines the minimum sample size to use with a specified algorithm,
#' given a minimum value for the metric ("Accuracy" or "Kappa").
#' It may be used for binary or multiple-feature classification.
#' 
#' @import caret foreach doParallel stats
#' @param X Dataset of variable/s to use for prediction.
#' @param Y Vector with the predictor variable, i.e., single variable to classify.
#' @param algorithm Choose from "knn" (k Nearest Neighbors), "glm" (logistic regression), "nb" (Naive Bayes), or "rf" (Random Forest).
#' For binary classification any of the algorithms may be used. For multiple feature classification, only "knn" or "rf" may be used.
#' @param metric "Accuracy" or "Kappa". Classification metric to use to determine minimum sample size.
#' @param thr_metric Threshold on the metric to calculate the corresponding minimum sample size.
#' @param p_vec Vector of ratios to divide training data into. 
#' Code loops through the different ratios to get a sample size and calculate the corresponding metric. 
#' Default is 1:99/100
#' @param n.cores Number of logical CPU threads to use. Default is 1.
#' @return List with minimum sample size, corresponding CI, dataframe with sample size, 
#' corresponding obtained metrics, and fit parameters of the metric.
#' @export
MinSizeClassification <- function(X,Y,algorithm,
                                  metric,thr_metric,
                                  formula_rhs="(1-a)-b*X^c",start_parameters,
                                  p_vec=1:99/100,
                                  cv_number=5, show_plot = T,
                                  n.cores=1){
  
  # For paralelization with foreach:
  # Create the cluster
  my.cluster <- parallel::makeCluster(
    n.cores,
    type = "PSOCK"
  )
  
  #register it to be used by %dopar%
  doParallel::registerDoParallel(cl = my.cluster)
  
  # Control to get Y as a vector rather than a list:
  if (typeof(Y)=="list") {
    Y <- unlist(Y)
  }
  
  # Parameter tuning:
  # Define parameters to use cross-validation in the train() function:
  train_control <- trainControl(## n-fold CV
    method = "repeatedcv",
    number = cv_number,
    ## repeated 1 times
    repeats = 1)
  
  i_here<-NULL
  # Loop through the data sizes given by p_vec
  
  x_foreach <- foreach(
    i_here = p_vec,
    .combine = 'rbind'
  ) %dopar% {
    
    # Split data for training set (keep a portion p=i_here):
    trainIndex <- createDataPartition(Y, p = i_here, 
                                      list = FALSE, 
                                      times = 1)
    
    # Input data split:
    trainX <- X[trainIndex,]
    testX <- X[-trainIndex,]
    trainY <- Y[trainIndex]
    testY <- Y[-trainIndex]
    
    # Select algorithm an fit:
    if (algorithm == "rf") {
      
    # Create grid for the train() function
    rfGrid <- expand.grid(mtry=round(c(2, sqrt(ncol(trainX))/2,
                                       seq(sqrt(ncol(trainX)), 
                                           ncol(trainX), 
                                           length.out = 4))))
    
    # Training data:
    trainFit <- train(x = trainX, y = trainY,
                   method = algorithm, 
                   trControl = train_control,
                   metric = metric,
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = TRUE, 
                   ## Now specify the exact models 
                   ## to evaluate:
                   tuneGrid = rfGrid)
    
    } else {
      # Training data:
      trainFit <- train(x = trainX, y = trainY,
                        method = algorithm, 
                        metric = metric, 
                        trControl = train_control)
    }
    
    # Predictions:
    Prediction <- predict(trainFit, newdata = testX)
    
    # Return when using paralelization with foreach:
    return(c(length(trainIndex),  # save size of training set
             mean(Prediction == testY),  # save accuracy
             cohen_kappa(Prediction,testY)  # save Cohen's kappa
    )
    )
    
  }
  
  # Create dataframe with saved vectors:
  df_acc_cohen <- data.frame(x_foreach, row.names = NULL)
  names(df_acc_cohen) <- c("X","Yacc","Ykap")
  
  # Plot accuracy vs size of training set.
  # Circles = calculated values of accuracy given a sample size.
  # Lines = fitted data.
  
  if (show_plot) {
    
  
  if (metric =="Accuracy") {
    # # Calculated accuracy vs sample size:
    plot(df_acc_cohen$X,
         df_acc_cohen$Yacc,
         xlab = "Training set size", ylab = "Accuracy of prediction")
    
    # plot_metric <- ggplot(data = df_acc_cohen) + 
    #   geom_point(mapping = aes(x=training_set_size,y=acc_vec),
    #              shape=21,color="black",fill="black",alpha=0.9) + 
    #   labs(x="Training set size",y="Accuracy of prediction")
    # 
    # print(plot_metric)
    
  } else {
    # # Calculated Kappa vs sample size:
    plot(df_acc_cohen$X,
         df_acc_cohen$Ykap,
         xlab = "Training set size", ylab = "Kappa of prediction")
    
    # plot_metric <- ggplot(data = df_acc_cohen) + 
    #   geom_point(mapping = aes(x=training_set_size,y=cohen_vec),
    #              shape=21,color="black",fill="black",alpha=0.9) + 
    #   labs(x="Training set size",y="Kappa of prediction")
    # 
    # print(plot_metric)
  }
  
  }
  
  # Fit non-linear regression to get the accuracy fit.
  # Formula given by Figueroa et al 2012
  if (metric =="Accuracy") {
    
    text_formula <- paste("Yacc~",formula_rhs,sep = "")
    
    fit_accuracy <- nls(formula = as.formula(text_formula),
                        data = df_acc_cohen, 
                        start = start_parameters,
                        control = nls.control(maxiter = 100, tol = 1e-8),
                        algorithm = "port"
    ) 
  } else {
    
    text_formula <- paste("Ykap~",formula_rhs,sep = "")
    
    fit_accuracy <- nls(formula = as.formula(text_formula),
                        data = df_acc_cohen, 
                        start = start_parameters,
                        control = nls.control(maxiter = 100, tol = 1e-8),
                        algorithm = "port"
    )  
  }
  
  # Coefficients: 
  a_fit <- summary(fit_accuracy)$coefficients[,1][[1]]
  b_fit <- summary(fit_accuracy)$coefficients[,1][[2]]
  c_fit <- summary(fit_accuracy)$coefficients[,1][[3]]
  
  # Confidence intervals for fitted curve (from Figueroa 2012 appendix):
  se.fit <- sqrt(apply(fit_accuracy$m$gradient(),
                       1,
                       function(x) sum(vcov(fit_accuracy)*outer(x,x)))
  );
  prediction.ci <- predict(fit_accuracy,x=df_acc_cohen$X) + outer(se.fit,qnorm(c(.5, .025,.975)))
  
  predictY<-prediction.ci[,1];
  predictY.lw<-prediction.ci[,2];
  predictY.up<-prediction.ci[,3];
  
  
  if (show_plot) {
    # df_predictions <- data.frame(df_acc_cohen$X,
    #                              predictY, predictY.up, predictY.lw,
    #                              row.names = NULL)
    # names(df_predictions) <- c("X",
    #                            "predictY","predictY.up","predictY.down")
    
  # # Middle line:
  graphics::lines(df_acc_cohen$X,
                  predictY, col = "black")
    # plot_metric <- plot_metric +  geom_line(aes(y = predictY), size = 1)
  
  # # Upper line:
  graphics::lines(df_acc_cohen$X,
        predictY.up, col = "blue")
    # plot_metric <- plot_metric +  geom_line(aes(y = predictY.up), size = 1)
  
  # # Lower line:
  graphics::lines(df_acc_cohen$X,
        predictY.lw, col = "red")
    
    # plot_metric <- plot_metric +  
    #   geom_line(data = df_predictions, size = 1) 
    
    # print(plot_metric)
  }
  
  # Minimum sample size calculation:
  # Simply with the formula, solving for new_data:
  min_sam_size <- fit_acc_fun(a_fit,b_fit,c_fit,thr_metric)
  
  # Print results.
  print(c("Chosen algorithm: ", algorithm),quote=F)
  if (metric=="Accuracy") {
    print("For minimum accuracy:",quote=F)
  } else {
    print("For minimum kappa:",quote=F)
  }
  print(thr_metric)
  print("Minimum sample size:",quote=F)
  print(min_sam_size)
  
  print("RMSE of fit:")
  RMSE_fit <- RMSE(pred = predictY, obs = df_acc_cohen$Yacc,na.rm = T)
  print(RMSE_fit)
  print("MAE of fit:")
  MAE_fit <- MAE(pred = predictY, obs = df_acc_cohen$Yacc,na.rm = T)
  print(MAE_fit)
  print("R^2 of fit:")
  R2_fit <- R2(pred = predictY, obs = df_acc_cohen$Yacc, na.rm = T)
  print(R2_fit)
  
  # Confidence interval for minimum sample size:
  CI_vec <- CI_MinimumSampleSize_fun(df_acc_cohen$X,
                                     prediction.ci,
                                     formula_rhs,
                                     start_parameters,
                                     thr_metric,
                                     min_sam_size,
                                     fit_accuracy)
  
  print("CI for minimum sample size: d)",quote=F)
  print(CI_vec)
  
  return_info <- list("Nmin" = min_sam_size, 
                      "CI" = CI_vec, 
                      "df"= df_acc_cohen, 
                      "coeffs" = c(a_fit,b_fit,c_fit),
                      "RMSE" = RMSE_fit, "MAE" = MAE_fit, "R2" = R2_fit)
  return(return_info)
  
}