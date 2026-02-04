#'Lasso Regression using the better package lars or glmnet with considering its computation time
#'
#' This function automatically performs Lasso regression using either lars or glmnet,
#' depending on the specified computation time threshold (`T_hope`). It selects the best
#' model based on cross-validation and can optionally make predictions on new data.
#'
#'
#' @param X A matrix or data frame of predictor variables.
#' @param y A numeric vector of response variables.
#' @param new_x Optional. A matrix or data frame for prediction. If provided, predictions will be made for `new_x`.
#' @param size An integer specifying the number of samples for the glmnet lambda search. Default is 1000.
#' @param T_hope A numeric value specifying the computation time threshold to decide between lars and glmnet. Default is 20.
#' @param seed An integer specifying the random seed for reproducibility. Default is 1.
#' @param message Logical. If `TRUE`, progress messages will be displayed. Default is `TRUE`.
#' @param line logical(1). If TRUE, the Pareto-front points in the plot are　connected with a polyline (frontier line). If FALSE, only the points are shown. Default is TRUE.
#'
#' @return A list containing the following components:
#' \item{method}{A character string indicating the method used ("lars" or "glmnet").}
#' \item{timing}{A list containing the predicted time for 'lars' and the threshold `T_hope`.}
#' \item{configuration}{A list containing the selected configuration (e.g., `nlambda`, `thresh`) if 'glmnet' is used. NULL if 'lars' is used.}
#' \item{cv}{The cross-validation result object (`cv.lars` or `cv.glmnet`).}
#' \item{model}{The fitted model object (`lars` or `glmnet`).}
#' \item{selection}{A list containing the best tuning parameter (`best_step` for 'lars', `best_lambda` for 'glmnet').}
#' \item{coefficients}{The regression coefficients of the best model.}
#' \item{prediction}{Predictions for `new_x` (if provided).}
#' \item{Pareto_front}{Object containing Pareto front information (only for 'glmnet').}
#' \item{Pareto_front_data}{Data frame of the Pareto front (only for 'glmnet').}
#'
#' @details
#' The function decides between lars and glmnet based on the computation time threshold (`T_hope`).
#' If `T_hope` is greater than the estimated time for lars, the function uses lars for Lasso regression.
#' Otherwise, glmnet is used. Cross-validation is performed to select the optimal model.
#'
#' @import dplyr
#' @import glmnet
#' @import lars
#' @export
auto_lasso<- function(X, y, new_x=NULL, size = 1000, T_hope = 20, seed=1, message = TRUE, line=TRUE) {
  X<-as.matrix(X)
  p<-ncol(X)
  y<-as.numeric(y)
  # Forecast computaion time for lars
  lars_time<-forecast_lars_cptime(X=X, T_hope=T_hope, message=message)
  print(lars_time)

  result <- list(
    method        = NA_character_,
    timing        = list(predicted_lars_time = lars_time, T_hope = T_hope),
    configuration = NULL,
    cv            = NULL,
    model         = NULL,
    selection     = list(best_step = NA_integer_, best_lambda = NA_real_),
    coefficients  = NULL,
    Pareto_front =NULL,
    Pareto_front_data  =NULL,
    prediction    = NULL
  )

  if(T_hope>lars_time){#lasso by lars
    method<-"lars"

    set.seed(seed)
    cvlars<-cv.lars(X,y,type = "lasso" ,mode ="step",max.step=p)
    lars_model<-lars(X,y,type = "lasso",trace = FALSE)
    i_best      <- which.min(cvlars$cv)
    best_step   <- cvlars$index[i_best]
    best_lambda <- lars_model$lambda[best_step]

    coef_lars<-coef(lars_model,mode = "step",s=best_step)

    result$method                 <- method
    result$cv            <- cvlars
    result$model             <- lars_model
    result$selection$best_step    <- best_step
    result$selection$best_lambda    <- best_lambda
    result$coefficients           <- coef_lars

    #prediction
    if(!is.null(new_x)){
      new_x<-as.matrix(new_x)
      result$prediction <- predict(lars_model, newx = new_x, mode = "step", s = best_step)$fit
    }
  }else{   #lasso by glmnet
    method<-"glmnet"
    set.seed(seed)

    tuning_result<-tune_configuration(X=X,size=size,T_hope=T_hope,seed=seed,message = message,line)
    n_lambda<-round(tuning_result$best_configuration$nlambda)
    thresh<-tuning_result$best_configuration$thresh

    model1<-glmnet(X,y,family = "gaussian",alpha = 1)
    minn<-min(model1$lambda)
    len_model1_lambda<-length(model1$lambda)
    model1_lambda<-model1$lambda
    lam<-numeric()
    if(n_lambda<len_model1_lambda){
      lam<-model1_lambda
    }else{
      add<-seq(minn,0,by=-(minn/(n_lambda-len_model1_lambda)))[-1]
      lam<-c(model1_lambda,add)
    }
    cv_set<-cv.glmnet(X,y,family = "gaussian",alpha = 1,thresh=thresh,lambda=lam)
    glmnet_model<-glmnet(X,y,family = "gaussian",alpha = 1,thresh=thresh,lambda=lam)
    coef_glmnet<-coef(glmnet_model,s=cv_set$lambda.min)

    result$method                   <- "glmnet"
    result$cv                       <- cv_set
    result$model             <- glmnet_model
    result$selection$best_lambda    <- cv_set$lambda.min
    result$coefficients             <- coef_glmnet
    result$configuration          <- list( thresh = thresh, seq_lambda= lam,nlambda = n_lambda)
    result$Pareto_front                   <- tuning_result$Pareto_front
    result$Pareto_front_data                   <- tuning_result$Pareto_front_data

    #predictionまで
    if(!is.null(new_x)){
      new_x<-as.matrix(new_x)
      result$prediction <-  predict(glmnet_model,newx = new_x,s=cv_set$lambda.min)
    }

  }

  return(result)
}

