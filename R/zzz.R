#' @importFrom stats coef cov predict runif
#' @importFrom utils head tail
#' @useDynLib glmnetconf, .registration = TRUE
.onLoad <- function(libname, pkgname) {
  #message("Initializing .myPackageEnv")
  utils::globalVariables(c("p", "lmodel", "Pareto", "Time", "Coef_Accuracy"))

  # パッケージ専用環境の初期化
  .myPackageEnv <<- new.env(parent = emptyenv())

  # ファイル読み込みのヘルパー関数
  safe_load_csv <- function(filepath, varname) {
    if (file.exists(filepath)) {
      .myPackageEnv[[varname]] <- as.matrix(utils::read.csv(filepath))
      #message("Loaded: ", varname)
    } else {
      warning("File not found: ", filepath)
    }
  }

  # スケーリングパラメータのロード
  scaling_files <- list(
    list(file = "scaler_x_params_lars.csv", varname = "scaler_x_params_lars"),
    list(file = "scaler_y_params_lars.csv", varname = "scaler_y_params_lars"),
    list(file = "scaler_x_params_glmnet.csv", varname = "scaler_x_params_glmnet"),
    list(file = "scaler_y_params_glmnet.csv", varname = "scaler_y_params_glmnet")
  )

  for (spec in scaling_files) {
    filepath <- system.file("extdata", spec$file, package = pkgname)
    safe_load_csv(filepath, spec$varname)
  }

  # NN パラメータのロード
  nn_files <- function(prefix, indices, subfolder) {
    for (i in indices) {
      # バイアス
      bias_path <- system.file("extdata", subfolder, paste0("model.", i, ".bias.csv"), package = pkgname)
      safe_load_csv(bias_path, paste0("b", i, "_", prefix))

      # 重み
      weight_path <- system.file("extdata", subfolder, paste0("model.", i, ".weight.csv"), package = pkgname)
      safe_load_csv(weight_path, paste0("W", i, "_", prefix))
    }
  }

  # 各モデルのロード
  nn_files("lars", c(0, 2, 4, 6), "model_params_lars")
  nn_files("glmnet", c(0, 2, 4, 6), "model_params_glmnet")

  # x.csv と y.csv をロード
  data_files <- list(
    list(file = "x.csv", varname = "x"),
    list(file = "y.csv", varname = "y"),
    list(file = "x_test.csv", varname = "x_test"),
    list(file = "y_test.csv", varname = "y_test")
  )

  for (spec in data_files) {
    filepath <- system.file("extdata", spec$file, package = pkgname)
    safe_load_csv(filepath, spec$varname)
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Package loaded successfully")
}
