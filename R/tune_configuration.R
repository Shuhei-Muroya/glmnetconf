#' Automatically select configuration for glmnet
#'
#' This function uses a neural network model to find optimal `nlambda` and `thresh` parameters based on input data.
#'
#' @param X Input matrix.
#' @param size Integer, number of samples to generate pareto front for optimization.
#' @param T_hope A numeric value representing the desired computation time threshold. Default is 20.
#' @param message A logical value. If `TRUE`, messages will be displayed. Default is `TRUE`.
#' @param seed An integer specifying the random seed for reproducibility. Default is 1.
#' @param line Logical. If `TRUE`, the Pareto-front points in the plot are connected with a polyline (frontier line). If `FALSE`, only the points are shown. Default is `FALSE`.
#'
#' @return A list containing optimal `nlambda`, `thresh`, pareto front, and pareto data.
#'
#' @import Rcpp
#' @importFrom Rcpp sourceCpp
#' @importFrom readr read_csv
#' @export
tune_configuration<- function(X, size = 1000, T_hope = 20, seed=1,message = TRUE,line=FALSE) {
  x_input <- make_input(X)
  x_input <- as.numeric(x_input)
  x_input_matrix <- matrix(rep(x_input, size), ncol = length(x_input), byrow = TRUE)
  p<-ncol(X)

  # random sampling of configuration for glmnet
  set.seed(seed)
  nlambda <- runif(size, 100, 2*p)
  log_thresh <- runif(size, log(1e-9), log(1e-7))
  thresh <- exp(log_thresh)
  x_input_matrix <- cbind(x_input_matrix, nlambda, thresh)
  x_input_matrix <- unname(as.matrix(x_input_matrix))

  results <- apply(x_input_matrix, 1, function(x) {
    myNN_glmnet_cpp(as.numeric(x))
  })

  results <- t(results)
  results <- cbind(results, nlambda, thresh)

  # Solve Pareto optimal solution and plot Pareto front
  result_return <- cp_pareto_front(results,T_hope,line)

  if (message) {
    cat("params_nlambda:", result_return$best_configuration$nlambda, "\n")
    cat("params_thresh:",result_return$best_configuration$thresh, "\n")
  }

  result_list <- list(best_configuration = result_return$best_configuration,
                      Pareto_front = result_return$Pareto_front,
                      Pareto_front_data = result_return$data)
  return(result_list)
}
