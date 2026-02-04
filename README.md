
# glmnetconf

<!-- badges: start -->
<!-- badges: end -->

The glmnetconf package automatically provides appropriate hyperparameters for glmnet function.
It selects the appropriate hyperparameters for glmnet based on the given data.
Additionally, it can compare lars and glmnet in terms of computation time and choose the better package for the task.

## Installation

You can install the development version of glmnetconf from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("Shuhei-Muroya/glmnetconf")
```
or
``` r
remotes::install_github("Shuhei-Muroya/glmnetconf")
```

## Example

This is a basic example :

``` r
library(glmnetconf)
library(MASS)
library(glmnet)

# Prepare data (Generate dummy data)
set.seed(1)
N_train <- 1500
N_test  <- 100
N_total <- N_train + N_test
p <- 800
rho <- 0.5

# Prepare Covariance Matrix
Sigma <- matrix(rho, nrow = p, ncol = p)
diag(Sigma) <- 1

# Generate full dataset
X <- mvrnorm(n = N_total, mu = rep(0, p), Sigma = Sigma)
beta_true <- c(rep(1, p/2), rep(0, p/2))
y <- X %*% beta_true + rnorm(N_total)

# Split into Training and Testing sets
train_ind <- 1:N_train
X_train <- X[train_ind, ]
y_train <- y[train_ind]
X_test  <- X[-train_ind, ]
y_test  <- y[-train_ind]


# Automatically select the hyperparameters and compute the lasso, predict for the test data.
result<-auto_lasso(X_train, y_train, new_x=X_test,T_hope=20)

# Check the estimated coefficients
print(result$coefficients)

# Check the Pareto front and the tuned configuration (if glmnet was used)
print(result$Pareto_front)
print(result$hyperparameters)

# Test error
mse_glmnetconf <-mean((y_test - result$prediction)^2)


# Compare the glmnet (default)
cv_glmnet <- cv.glmnet(X_train, y_train, alpha = 1)
pred_glmnet <- predict(cv_glmnet, newx = X_test, s = "lambda.min")
mse_default  <- mean((y_test - pred_glmnet)^2)


cat("Test MSE (glmnetconf):  ", mse_glmnetconf, "\n")
cat("Test MSE (glmnet default):", mse_default, "\n")

```

