evaluate_model <- function(observed, predicted) {
  mean_observed <- mean(observed)
  se <- (observed - predicted)^2
  ae <- abs(observed - predicted)
  sem <- (observed - mean_observed)^2
  aem <- abs(observed - mean_observed)
  mae <- mean(ae)
  rmse <- sqrt(mean(se))
  rae <- sum(ae) / sum(aem)
  rse <- sum(se) / sum(sem)
  rsq <- 1 - rse
  metrics <- c("Mean Absolute Error" = mae,
               "Root Mean Squared Error" = rmse,
               "Relative Absolute Error" = rae,
               "Relative Squared Error" = rse,
               "Coefficient of Determination" = rsq)
  return(metrics)
}
