#' Perform Lasso Regression Using lars and glmnet
#'
#' This function performs Lasso regression using both lars and glmnet, with default
#' and custom configurations. It compares computation times and returns the models,
#' cross-validation results, and coefficients.You can check the difference of three ways.
#'
#' @param X A matrix or data frame of predictor variables.
#' @param y A numeric vector of response variables.
#' @param nlambda The number of lambda values for the custom glmnet configuration. Default is 100.
#' @param thresh A numeric value specifying the convergence threshold for glmnet. Default is `1e-7`.
#' @param seed An integer seed for reproducibility. Default is 1.
#'
#' @return A list containing:
#' \item{lars}{A list with lars cross-validation results, the lars model, and coefficients for the optimal model.}
#' \item{glmnet_default}{A list with default glmnet cross-validation results, the glmnet model, and coefficients for the optimal model.}
#' \item{glmnet_set}{A list with custom glmnet cross-validation results, the glmnet model, and coefficients for the optimal model.}
#' \item{time}{A list with computation times for lars, default glmnet, and custom glmnet configurations.}
#'
#' @details
#' This function compares the performance of Lasso regression using lars and glmnet
#' with both default and custom configurations. For glmnet, the function allows customization
#' of the number of lambda values and the convergence threshold.
#'
#' The function calculates the coefficients for the optimal model based on cross-validation results.
#' Additionally, it measures the computation time for each method.
#'
#' @examples
#' \dontrun{
#' # Example data
#' x <- matrix(rnorm(100 * 10), 100, 10)
#' y <- rnorm(100)
#'
#' # Perform Lasso regression
#' result <- all_lasso(X = x, y = y)
#' print(result)
#' }
#'
#' @import glmnet
#' @import lars
#' @export
# all_lasso<-function(X,y,nlambda=100,thresh=1e-7,seed=1){
#   set.seed(seed)
#   X<-as.matrix(X)
#   y<-as.numeric(y)
#   p<-length(y)
#
#   #lars
#   a<-Sys.time()
#   cvlars_p<-cv.lars(X,y,plot.it = FALSE,type = "lasso" ,max.step=p,use.Gram=FALSE)
#   lars.model_p<-lars(X,y,use.Gram=FALSE,type = "lasso",trace = FALSE)
#   b<-Sys.time()
#   time_lars<-b-a
#   coef_p<-coef(lars.model_p,mode="lambda",s=cvlars_p$index[which.min(cvlars_p$cv)])
#   lars_list<-list(cv_lars=cvlars_p,lars_model=lars.model_p,lars_coef.cvmin=coef_p)
#
#   #glmnet default
#   set.seed(seed)
#   a<-Sys.time()
#   cv_def<-cv.glmnet(X,y,family = "gaussian",alpha = 1)
#   model_def<-glmnet(X,y,family = "gaussian",alpha = 1)
#   b<-Sys.time()
#   time_def<-b-a
#   coef_def<-coef(model_def,s=cv_def$lambda.min)
#   def_list<-list(def_cv=cv_def,def_model=model_def,def_coef.cvmin=coef_def)
#
#   #glmnet automatilcally select
#   lam<-numeric()
#   pl<-nlambda
#   t<-thresh
#   minn<-min(model_def$lambda)
#   len_model1_lambda<-length(model_def$lambda)
#   model1_lambda<-model_def$lambda
#   if(pl==0){
#     lam<-model1_lambda
#   }else if(pl<len_model1_lambda){
#     add<-seq(minn,0,by=-(minn/(100-len_model1_lambda)))[-1]
#     add<-seq(minn,0,length.out=pl)#[-1]
#     lam<-add
#   }else{
#     add<-seq(minn,0,by=-(minn/(pl-len_model1_lambda)))[-1]
#     lam<-c(model1_lambda,add)
#   }
#
#   pl_l<-length(lam)
#   st<-Sys.time()
#   cv_set<-cv.glmnet(X,y,family = "gaussian",alpha = 1,thresh=t,lambda=lam)
#   model_set<-glmnet(X,y,family = "gaussian",alpha = 1,thresh=t,lambda=lam)
#   gt<-Sys.time()
#   time_set<-gt-st
#   coef_set<-coef(model_set,s=cv_set$lambda.min)
#   set_list<-list(set_cv=cv_set,set_model=model_set,set_coef.cvmin=coef_set)
#
#   time_list<-list(time_lars=time_lars,
#                   time_def=time_def,
#                   time_set=time_set)
#   result<-list(lars=lars_list,
#                glmnet_default=def_list,
#                glmnet_set=set_list,
#                time=time_list)
#   return(result)
#
# }
all_lasso <- function(X, y, nlambda = 100, thresh = 1e-7, seed = 1, new_x = NULL) {
  set.seed(seed)
  X <- as.matrix(X)
  y <- as.numeric(y)
  p <- ncol(X)

  # ========== 1. LARS ==========
  a <- Sys.time()
  cvlars_p <- cv.lars(X, y, plot.it = FALSE, type = "lasso", max.step = p, use.Gram = FALSE)
  lars_model <- lars(X, y, use.Gram = FALSE, type = "lasso", trace = FALSE)
  b <- Sys.time()
  time_lars <- as.numeric(difftime(b, a, units = "secs"))

  best_step <- cvlars_p$index[which.min(cvlars_p$cv)]
  coef_lars <- coef(lars_model, mode = "lambda", s = best_step)

  pred_lars <- NULL
  if (!is.null(new_x)) {
    new_x <- as.matrix(new_x)
    pred_lars <- predict(lars_model, newx = new_x, mode = "lambda", s = best_step)$fit
  }

  lars_list <- list(
    cv = cvlars_p,
    model = lars_model,
    coef.cvmin = coef_lars,
    prediction = pred_lars
  )

  # ========== 2. glmnet (default) ==========
  set.seed(seed)
  a <- Sys.time()
  cv_def <- cv.glmnet(X, y, family = "gaussian", alpha = 1)
  model_def <- glmnet(X, y, family = "gaussian", alpha = 1)
  b <- Sys.time()
  time_def <- as.numeric(difftime(b, a, units = "secs"))

  coef_def <- coef(model_def, s = cv_def$lambda.min)
  pred_def <- if (!is.null(new_x)) predict(model_def, newx = as.matrix(new_x), s = cv_def$lambda.min) else NULL

  def_list <- list(
    cv = cv_def,
    model = model_def,
    coef.cvmin = coef_def,
    prediction = pred_def
  )

  # ========== 3. glmnet (set nlambda, thresh) ==========
  set.seed(seed)
  model_tmp <- glmnet(X, y, family = "gaussian", alpha = 1)
  minn <- min(model_tmp$lambda)
  len_model1_lambda <- length(model_tmp$lambda)

  if (nlambda <= len_model1_lambda) {
    lam <- model_tmp$lambda
  } else {
    add <- seq(minn, 0, length.out = nlambda)[-1]
    lam <- c(model_tmp$lambda, add)
  }

  a <- Sys.time()
  cv_set <- cv.glmnet(X, y, family = "gaussian", alpha = 1, thresh = thresh, lambda = lam)
  model_set <- glmnet(X, y, family = "gaussian", alpha = 1, thresh = thresh, lambda = lam)
  b <- Sys.time()
  time_set <- as.numeric(difftime(b, a, units = "secs"))

  coef_set <- coef(model_set, s = cv_set$lambda.min)
  pred_set <- if (!is.null(new_x)) predict(model_set, newx = as.matrix(new_x), s = cv_set$lambda.min) else NULL

  set_list <- list(
    cv = cv_set,
    model = model_set,
    coef.cvmin = coef_set,
    prediction = pred_set
  )

  # ========== 4. 出力まとめ ==========
  result <- list(
    lars = lars_list,
    glmnet_default = def_list,
    glmnet_set = set_list,
    time = list(
      lars = time_lars,
      glmnet_default = time_def,
      glmnet_set = time_set
    )
  )

  return(result)
}

