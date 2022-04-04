########################
# Logistics Regression #
########################

# Imports:
library(caret)
library(nlstools)
# Data:
require(ISLR)
attach(Smarket) # attach allows you to look up in R variables within a dataframe
X <- Smarket[-ncol(Smarket)]
Y <- Smarket[ncol(Smarket)]
Y <- unlist(Y)

# setwd("C:/Users/gpcas/Documents/Repos/master-thesis/")
source("./codes/cohen_kappa_fun.R")

# Threshold accuracy, or minimum accuracy to determine corresponding
# sample size, which is returned as minimum sample size.
thr_acc <- .95

# Vector to get data for training as a % of the total input data:
p_vec = 1:99/100

#######################
# Train and test:
#######################

# N-fold cross validation 
# http://datascience.recursos.uoc.edu/es/n-fold-cross-validation/
# Define parameters to use cross-validation in the train() function:
fitControl <- trainControl(## 5-fold CV
  method = "cv",
  number = 5,
  ## repeated 1 times
  repeats = 1)

# Initialize vectors to save properties:
acc_vec <- c()
cohen_vec <- c()
training_set_size <- c()
# Loop through the data sizes given by p_vec
for (i in p_vec) {
# set.seed(998)
# Split data for training set (keep a portion p=i):
trainIndex <- createDataPartition(Y, p = i, 
                             list = FALSE, 
                             times = 1)


# Input data split:
trainX <- X[trainIndex,]
testX <- X[-trainIndex,]
trainY <- Y[trainIndex]
testY <- Y[-trainIndex]

# Fit the training data
# glmFit <- glm(trainY ~ .,
#                data = trainX,
#                family = binomial,
#                control = glm.control(epsilon = 1e-8, maxit = 100))

# This is supposed to be the same but gives all sorts of problems:
glmFit <- train(x = trainX, y = trainY,
                method = "glm",
                family = binomial,
                trControl = fitControl
)
# Predictions:
glmPred <- predict(glmFit,
                     newdata = testX)

# Assign prediction probability to yes/no values:
# glm.pred <- ifelse(test = glm.probs > 0.5, yes = "Up", no = "Down")

# Save accuracy: proportion of correctly classified samples
acc_vec <- c(acc_vec,mean(glmPred == testY))

# Save Cohen's kappa:
cohen_vec <- c(cohen_vec,cohen_kappa(glmPred,testY))

# Save size of training set:
training_set_size <- c(training_set_size,length(trainIndex))
}

# Create dataframe with saved vectors:
df_acc_cohen <- data.frame(training_set_size,acc_vec,cohen_vec)

# Fit non-linear regression to get the accuracy fit.
# Formula given by Figueroa et al 2012
fit_accuracy <- nls(acc_vec~(1-a)-b*training_set_size^c,
                    data = df_acc_cohen, 
                    start = list(a=0,b=1,c=-0.5),
                    control = nls.control(maxiter = 100, tol = 1e-8),
                    algorithm = "port"
                    )  

# Figueroa say they use nl2sol, which corresponds to algorithm="port"
# port uses the nl2sol algorithm from the Port library
# However, this is from the nls documentation:
# "The algorithm = "port" code appears unfinished, 
# and does not even check that the starting value is within the bounds. 
# Use with caution, especially where bounds are supplied."
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/nls

#confidence intervals (from Figueroa 2012 appendix)
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
# polygon(c(rev(df_acc_cohen), df_acc_cohen), 
#         c(rev(predictY.up), predictY.lw),
#         col = 'grey', border = NA)

plot(df_acc_cohen$training_set_size,
     df_acc_cohen$acc_vec,
     xlab = "Training set size", ylab = "Accuracy of prediction")
# Middle line:
lines(df_acc_cohen$training_set_size,
      predict(fit_accuracy,df_acc_cohen$training_set_size))

# # Upper line:
lines(df_acc_cohen$training_set_size,
      predictY.up, col = "blue")

# # Lower line:
lines(df_acc_cohen$training_set_size,
      predictY.lw, col = "red")
# Predictions: get a finer set of accuracies from fitted data.

# Coefficients: 
a_fit <- summary(fit_accuracy)$coefficients[,1][[1]]
b_fit <- summary(fit_accuracy)$coefficients[,1][[2]]
c_fit <- summary(fit_accuracy)$coefficients[,1][[3]]

# New values of sample size:
# new_data <- 1:2000
# New, fitted values of accuracy:
# new_acc <- (1-a_fit)-b_fit*new_data^c_fit

# Minimum sample size, based on accuracy:
# min_sam_size <- new_data[which(new_acc==new_acc[new_acc >=0.95][1])[[1]]]

# Simply with the formula, solving for new_data:
min_sam_size <- abs(((1-a_fit-thr_acc)/b_fit)^(1/c_fit))

print("For minimum accuracy:")
print(thr_acc)
print("Minimum sample size:")
print(min_sam_size)

# Logistic regression steps from:
# https://www.datacamp.com/community/tutorials/logistic-regression-R

## Next steps:
# - Get a ROC from the previous point -> I think I need numerical predictions
# - Turn into a function, with inputs: dataset,column to predict; output: predictions
# - Goodness of fit measures: MAE and RMSE (see Figueroa 2012)