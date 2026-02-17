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
