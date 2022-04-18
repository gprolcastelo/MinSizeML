#' Cohen's Kappa
#' 
#' Custom-made function to calculate the Cohen's Kappa, which compares two vectors:
#' the result from predict() and the actual values, i.e., the test.
#' 
#' @param predict_result A vector resulting from the predict function
#' @param test A vector with the real values.
#' 
cohen_kappa <- function(predict_result,test){
# Cohen's kappa:
# Contingency table
xtab <- table(predict_result,test)
# Descriptive statistics
diagonal.counts <- diag(xtab)
N <- sum(xtab)
row.marginal.props <- rowSums(xtab)/N
col.marginal.props <- colSums(xtab)/N
# Compute kappa (k)
Po <- sum(diagonal.counts)/N
Pe <- sum(row.marginal.props*col.marginal.props)
k <- (Po - Pe)/(1 - Pe)
return(k)
}