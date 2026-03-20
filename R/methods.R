#' Print Method for glmnetconf Objects
#'
#' @param x A \code{glmnetconf} object.
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @method print glmnetconf
print.glmnetconf <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCall: ", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n")

  if (is.null(x$Pareto_front)) {
    # --- LARS Selected ---
    cat("Method: LARS (Least Angle Regression)\n")
    cat("Selection: Cross-Validation\n\n")
    sum_lars <- data.frame(
      Step = x$selection$best_step,
      Df   = sum(x$coefficients != 0),
      RSS  = round(min(x$cv$cv), digits)
    )
    print(sum_lars, row.names = FALSE)

  } else {
    # --- GLMNET Selected ---
    cat("Method: GLMNET (Optimized via Surrogate Model)\n")
    cat("Selection: Pareto-front & Cross-Validation\n\n")

    # glmnet の公式スタイルに準拠した3列構成
    # ベストな地点の情報を抽出
    cv_obj <- x$cv
    best_idx <- which(cv_obj$lambda == x$selection$best_lambda)

    summary_df <- data.frame(
      Df      = cv_obj$glmnet.fit$df[best_idx],
      "%Dev"  = round(cv_obj$glmnet.fit$dev.ratio[best_idx] * 100, digits),
      Lambda  = signif(x$selection$best_lambda, digits)
    )
    colnames(summary_df)[2] <- "%Dev" # 特殊文字の処理

    print(summary_df, row.names = FALSE)
  }

  cat("\n---")
  cat("\nNote: Use coef() to extract coefficients.")
  cat("\n      Use plot() to visualize the selection process.\n")

  invisible(x)
}
#' Plot Method for glmnetconf Objects
#'
#' @param x A \code{glmnetconf} object.
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @method plot glmnetconf
plot.glmnetconf <- function(x) {

  if (is.null(x$Pareto_front)) {
    cat("Decision: LARS selected because it was predicted to be faster than T_hope.\n")
  } else {
    cat("\nSelected Configuration (via Surrogate Model):\n")
    cat(sprintf("  - Convergence Threshold (thresh): %.3e\n", x$configuration$thresh))
    cat(sprintf("  - Number of Lambda values (nlambda): %d\n", x$configuration$nlambda))
    return(x$Pareto_front)
  }
}
