
# glmnetconf

<!-- badges: start -->
<!-- badges: end -->

The glmnetconf package automatically provides appropriate configuration for glmnet function.
It selects the appropriate configuration for glmnet based on the given data.
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
data<-data_generation(N_train = 1500, N_test = 100, p = 800, rho = 0.5, sparse_rate = 0.5, sigma = 1)
X_train <- data$X_train
y_train<-data$y_train
X_test  <-  data$X_test
y_test  <- data$y_test


# Automatically select the configuration and compute the lasso, predict for the test data.
result<-auto_lasso(X_train, y_train, new_x=X_test,T_hope=20)

# Check the estimated coefficients
print(result$coefficients)

# Check the Pareto front and the tuned configuration (if glmnet was used)
print(result$Pareto_front)
print(result$configuration)

# Test error
mse_glmnetconf <-mean((y_test - result$prediction)^2)


# Compare the glmnet (default)
cv_glmnet <- cv.glmnet(X_train, y_train, alpha = 1)
pred_glmnet <- predict(cv_glmnet, newx = X_test, s = "lambda.min")
mse_default  <- mean((y_test - pred_glmnet)^2)


cat("Test MSE (glmnetconf):  ", mse_glmnetconf, "\n")
cat("Test MSE (glmnet default):", mse_default, "\n")

```

