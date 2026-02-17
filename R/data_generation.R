#' Generate Synthetic Data for lasso Regression
#'
#' Generates simulation datasets with a compound symmetry covariance structure.
#'
#' @param N_train Integer; the number of observations for the training set.
#' @param N_test Integer; the number of observations for the test set.
#' @param p Integer; the total number of predictors (features).
#' @param rho Numeric; the constant correlation coefficient between predictors (0 to 1).
#' @param sparse_rate Numeric; the proportion of true coefficients set to zero (0.0 to 1.0).
#' @param sigma Numeric; the standard deviation of the Gaussian noise (error term).
#' @param seed Integer; the random seed for reproducibility.
#'
#' @details
#' The predictors are generated from a multivariate normal distribution \eqn{N(0, \Sigma)},
#' where \eqn{\Sigma} is a covariance matrix with 1 on the diagonal and \eqn{\rho}
#' on all off-diagonal elements. The true coefficient vector \eqn{\beta} consists
#' of ones for the first \eqn{p \times (1 - sparse\_rate)} elements and zeros thereafter.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{X_train}: A matrix of training predictors.
#'   \item \code{y_train}: A numeric vector of training responses.
#'   \item \code{X_test}: A matrix of test predictors.
#'   \item \code{y_test}: A numeric vector of test responses.
#'   \item \code{beta}: The true coefficient vector used in the data generation.
#' }
#' @importFrom MASS mvrnorm
#' @importFrom stats rnorm
data_generation <- function(N_train = 1500, N_test = 100, p = 800,
                            rho = 0.5, sparse_rate = 0.5, sigma = 1,seed=1) {
  set.seed(seed)
  N_total <- N_train + N_test

  Sigma <- matrix(rho, nrow = p, ncol = p)
  diag(Sigma) <- 1

  X <- mvrnorm(n = N_total, mu = rep(0, p), Sigma = Sigma)

  num_zeros <- round(p * sparse_rate)
  num_ones <- p - num_zeros
  beta_true <- c(rep(1, num_ones), rep(0, num_zeros))

  epsilon <- rnorm(N_total, mean = 0, sd = sigma)
  y <- X %*% beta_true + epsilon

  train_idx <- 1:N_train

  return(list(
    X_train = X[train_idx, , drop = FALSE],
    y_train = y[train_idx],
    X_test  = X[-train_idx, , drop = FALSE],
    y_test  = y[-train_idx],
    beta    = beta_true
  ))
}
