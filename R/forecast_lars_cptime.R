#' Forecast Lars Computation Time
#'
#' This function predicts the computation time for the lars model based on input data.
#' It also suggests whether to use glmnet or lars based on the predicted computation time.
#'
#' @param X A numeric matrix or vector representing the input data.
#' @param T_hope A numeric value representing the desired computation time threshold. Default is 20.
#' @param message A logical value. If `TRUE`, messages will be displayed. Default is `TRUE`.
#'
#' @details The function takes an input dataset and processes it through a neural network model defined by specific weights and biases loaded from files. The network is designed to predict the computation time for the Lars model.
#'
#' @return A numeric value representing the predicted computation time for the Lars model.
#'
#'
#' @import RcppArmadillo
#' @import Rcpp
#' @export
forecast_lars_cptime <- function(X, T_hope = 20, message = TRUE) {
  x_input <- make_input(X)
  x_input <- as.numeric(x_input)
  x_input <- c(x_input[1:(length(x_input) - 5)], rev(tail(x_input, 5)))# Temporary fix: due to inconsistent model input
  result <- myNN_lars_cpp(x_input)
  pred_time <- result[1]#time
  if (message) {
    cat(sprintf(
      "Predicted lars time: %.2f (T_hope: %.2f)\n",
      pred_time, T_hope
    ))
    if (pred_time > T_hope) {
      cat("→ The predicted time exceeds T_hope; it might be better to use 'glmnet'.\n")
    } else {
      cat("→ The predicted time is within T_hope; it might be better to use 'lars'.\n")
    }
  }

  return(pred_time)
}
