#include <RcppArmadillo.h>  // RcppとArmadilloライブラリをインクルード
using namespace arma;
using namespace Rcpp;
//
// スイッシュ関数の定義
double swish(double x) {
  return x / (1 + exp(-x));
}

// ベクトルにスイッシュ関数を適用
arma::vec applySwish(const arma::vec& x) {
  arma::vec result = x;
  for (size_t i = 0; i < x.n_elem; ++i) {
    result[i] = swish(x[i]);
  }
  return result;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec myNN_lars_cpp(arma::vec x_input) {
  // パッケージ専用環境を取得
  Environment myEnv = Environment::global_env()[".myPackageEnv"];

  // 必要な変数を環境から取得
  arma::mat scaler_params_x_lars = as<arma::mat>(myEnv["scaler_x_params_lars"]);
  arma::mat scaler_params_y_lars = as<arma::mat>(myEnv["scaler_y_params_lars"]);
  arma::vec b0 = as<arma::vec>(myEnv["b0_lars"]);
  arma::mat W0 = as<arma::mat>(myEnv["W0_lars"]);
  arma::vec b1 = as<arma::vec>(myEnv["b2_lars"]);
  arma::mat W1 = as<arma::mat>(myEnv["W2_lars"]);
  arma::vec b2 = as<arma::vec>(myEnv["b4_lars"]);
  arma::mat W2 = as<arma::mat>(myEnv["W4_lars"]);
  arma::vec b3 = as<arma::vec>(myEnv["b6_lars"]);
  arma::mat W3 = as<arma::mat>(myEnv["W6_lars"]);

  // スケーリング処理
  arma::vec mean = scaler_params_x_lars.col(0);
  arma::vec scale = scaler_params_x_lars.col(1);
  arma::vec x_scaled = (x_input - mean) / scale;

  // ネットワークの計算
  arma::vec y1 = b0 + W0 * x_scaled;
  arma::vec z1 = applySwish(y1);

  arma::vec y2 = b1 + W1 * z1;
  arma::vec z2 = applySwish(y2);

  arma::vec y3 = b2 + W2 * z2;
  arma::vec z3 = applySwish(y3);

  arma::vec y4 = b3 + W3 * z3;

  // 逆スケーリング処理
  arma::vec mean_y = scaler_params_y_lars.col(0);
  arma::vec scale_y = scaler_params_y_lars.col(1);
  arma::vec y_scaled_back = (y4 % scale_y) + mean_y;
  y_scaled_back = exp(y_scaled_back);  // 出力に exp() を適用

  return y_scaled_back;  // 最終出力を返す
}


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec myNN_glmnet_cpp(arma::vec x_input) {
  // パッケージ専用環境を取得
  Environment myEnv = Environment::global_env()[".myPackageEnv"];

  // 必要な変数を環境から取得
  arma::mat scaler_params_x_glmnet = as<arma::mat>(myEnv["scaler_x_params_glmnet"]);
  arma::mat scaler_params_y_glmnet = as<arma::mat>(myEnv["scaler_y_params_glmnet"]);
  arma::vec b0 = as<arma::vec>(myEnv["b0_glmnet"]);
  arma::mat W0 = as<arma::mat>(myEnv["W0_glmnet"]);
  arma::vec b1 = as<arma::vec>(myEnv["b2_glmnet"]);
  arma::mat W1 = as<arma::mat>(myEnv["W2_glmnet"]);
  arma::vec b2 = as<arma::vec>(myEnv["b4_glmnet"]);
  arma::mat W2 = as<arma::mat>(myEnv["W4_glmnet"]);
  arma::vec b3 = as<arma::vec>(myEnv["b6_glmnet"]);
  arma::mat W3 = as<arma::mat>(myEnv["W6_glmnet"]);

  // スケーリング処理
  arma::vec mean = scaler_params_x_glmnet.col(0);
  arma::vec scale = scaler_params_x_glmnet.col(1);
  arma::vec x_scaled = (x_input - mean) / scale;

  // ネットワークの計算
  arma::vec y1 = b0 + W0 * x_scaled;
  arma::vec z1 = applySwish(y1);

  arma::vec y2 = b1 + W1 * z1;
  arma::vec z2 = applySwish(y2);

  arma::vec y3 = b2 + W2 * z2;
  arma::vec z3 = applySwish(y3);

  arma::vec y4 = b3 + W3 * z3;

  // 逆スケーリング処理
  arma::vec mean_y = scaler_params_y_glmnet.col(0);
  arma::vec scale_y = scaler_params_y_glmnet.col(1);
  arma::vec y_scaled_back = (y4 % scale_y) + mean_y;
  y_scaled_back = exp(y_scaled_back);  // 出力に exp() を適用

  return y_scaled_back;  // 最終出力を返す
}
