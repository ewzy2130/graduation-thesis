# set.seed(122)
# n <- 50
# p <- 10
# X <- matrix(rnorm(n * 10), n, 10)
# col_names <- c("col1", "col2", "col3", "col4", "col5",
#                "col6", "col7", "col8", "col9", "protected")
# colnames(X) <- col_names
# X[ ,"protected"] <- sample(c(1, 2), n, replace = TRUE)
# X <- as.data.frame(X)
# X[ ,"protected"] <- sample(c(0, 1), n, replace = TRUE)
# #使用 rnorm 函数生成一个服从标准正态分布的随机矩阵，维度为 n 行 p 列
# Xtest <- matrix(rnorm(100 * p), 100, p)#大小为 101 行 p 列的零矩阵，并将其赋给变量 X.test
# col_names <- c("col1", "col2", "col3", "col4", "col5",
#                "col6", "col7", "col8", "col9", "protected")
# colnames(Xtest) <- col_names
# Xtest <- as.data.frame(Xtest)
# Xtest[ ,"protected"] <- sample(c(0, 1), 100, replace = TRUE)
#
# Y <- X[, 1] * rnorm(n)
# # missing_prob <- pnorm(X[, 1])
# # if_missing <- missing_prob < runif(n)
# # Y[if_missing] <- NA
# n.trees = 100

FairBoosting <- function(Y, X, Xtest, quantiles, n.trees = 1000, ...){
  if (class(X)[1] != "data.frame"){
    X <- as.data.frame(X)
    Xtest <- as.data.frame(Xtest)
    names(Xtest) <- names(X)
  }
  data <- data.frame(Y = Y, X)
  fit <- gbm::gbm(Y ~ ., distribution = list(name = "quantile", alpha = quantiles[1]), data = data, n.trees = n.trees)
  res <- predict(fit, Xtest, type = "response", n.trees = n.trees)
  if (length(quantiles) == 2){
    fit2 <- gbm::gbm(Y ~ ., distribution = list(name = "quantile", alpha = quantiles[2]), data = data, n.trees = n.trees)
    res2 <- predict(fit2, Xtest, type = "response", n.trees = n.trees)
    res0 <- cbind(res, res2)
  }
  
  res0 <- data.frame(res0)
  colnames(res0) <- c("q_pre.quantile = 0.05","q_pre.quantile = 0.95")
  res_new <- cbind(Xtest, res0)
  
  pred_col_1 = res_new$"q_pre.quantile = 0.05"
  pred_col_2 = res_new$"q_pre.quantile = 0.95"
  calib_sorted_1 <- res_new[order(pred_col_1), ]
  calib_sorted_2 <- res_new[order(pred_col_2), ]
  protected_col_1 = res_new[order(pred_col_1), ]$protected
  protected_col_2 = res_new[order(pred_col_2), ]$protected
  
  groupsa <- split(calib_sorted_1,  protected_col_1)#把 calib_sorted 按照 protected_col 列（受保护特征）分组
  groupsb <- split(calib_sorted_2,  protected_col_2)
  n_calib <- nrow(res_new)#计算行数
  taus <- seq(0, 1, length.out = n_calib)
  #seq(0, 1, length.out = n_calib) 创建了一个从 0 到 1 的等差序列，序列的长度由参数 length.out 指定，即 n_calib。
  #该函数会将 0 和 1 之间的数均匀分布到 n_calib 个元素中
  Ns1 <- sapply(groupsa, nrow)#根据分组计算每个分组的样本数 Ns 和占比 ps
  Ns2 <- sapply(groupsb, nrow)
  ps1 <- Ns1 / n_calib
  ps2 <- Ns2 / n_calib
  y1_fair <- rep(0, n_calib)#创建一个长度为 n_calib 的零向量，用于存储合成的公平化目标变量
  y2_fair <- rep(0, n_calib)
  interpolation = 'linear'
  sigma = 1e-2
  for (group in groupsa) {
    original_subgroup_sorted <- calib_sorted_1$`q_pre.quantile = 0.05` + runif(n_calib,-sigma,sigma)
    #对排序后的预测结果q_pre.quantile进行操作，加了一个随机数，即q~
    fair_subgroup_sorted <-rep(NA, n_calib)
    # 创建一个长度为 n_calib 的空向量 fair_subgroup_sorted，用于存储公平化的子组预测结果。
    for (k in 1:n_calib) {
      fair_subgroup_sorted[k] <- quantile(original_subgroup_sorted,taus[k])
      #使用 quantile 函数，基于 original_subgroup_sorted（q~）的值和 taus[k] 的位置，对排序后的预测结果进行插值，得到公平化的子组预测结果。
      #具体来说，它将计算在 original_subgroup_sorted 中对应 taus[k] 位置的分位数值，作为 fair_subgroup_sorted[k] 的值。
    }
    
    y1_fair <- y1_fair + ps1*fair_subgroup_sorted
  }
  pred_col_2 = res_new$"q_pre.quantile = 0.95"
  calib_sorted_2 <- res_new[order(pred_col_2), ]
  protected_col_2 = res_new[order(pred_col_2), ]$protected
  groupsb <- split(calib_sorted_2,  protected_col_2)
  Ns2 <- sapply(groupsb, nrow)
  ps2 <- Ns2 / n_calib
  y2_fair <- rep(0, n_calib)
  interpolation = 'linear'
  sigma =1e-2
  
  for (group in groupsb) {
    original_subgroup_sorted <- calib_sorted_2$`q_pre.quantile = 0.95` + runif(n_calib,-sigma,sigma)
    #对排序后的预测结果q_pre.quantile进行操作，加了一个随机数，即q~
    fair_subgroup_sorted <-rep(NA, n_calib)
    # 创建一个长度为 n_calib 的空向量 fair_subgroup_sorted，用于存储公平化的子组预测结果。
    for (k in 1:n_calib) {
      fair_subgroup_sorted[k] <- quantile(original_subgroup_sorted,taus[k])
      #使用 quantile 函数，基于 original_subgroup_sorted（q~）的值和 taus[k] 的位置，对排序后的预测结果进行插值，得到公平化的子组预测结果。
      #具体来说，它将计算在 original_subgroup_sorted 中对应 taus[k] 位置的分位数值，作为 fair_subgroup_sorted[k]的值。
    }
    
    y2_fair <- y2_fair + ps2*fair_subgroup_sorted
  }
  
  y_fair <- t(rbind(y1_fair,y2_fair))
  return( y_fair)
}

# d1 <- quantBoosting1(Y, X, Xtest, quantiles = c(0.05,0.95), n.trees = 100)
# print(d1)


FairBART <- function(Y, X, Xtest, quantiles,
                     ndpost = 100, ...){
  if (class(X)[1] != "data.frame"){
    X <- as.data.frame(X)
    Xtest <- as.data.frame(Xtest)
    names(Xtest) <- names(X)
  }
  y <- Y
  fit <- bartMachine::bartMachine(X, y, verbose = FALSE)
  if (length(quantiles) == 2){
    if (sum(quantiles) != 1){
      warning("Two quantiles should sum up to 1.")
    }
    ci_conf <- quantiles[2] - quantiles[1]
    res0 <- bartMachine::calc_prediction_intervals(
      fit, new_data = Xtest,
      pi_conf = 0.95)$interval
    res0 <- as.matrix(res0)
  } else if (length(quantiles) == 1){
    if (quantiles[1] > 0.5){
      ci_conf <- 2 * quantiles[1]
      res0 <- bartMachine::calc_prediction_intervals(
        fit, new_data = Xtest,
        pi_conf = 0.95)$interval[, 2]
      res0 <- as.numeric(res)
    }  else{
      ci_conf <- 2 * (1 - quantiles[1])
      res0 <- bartMachine::calc_prediction_intervals(
        fit, new_data = Xtest,
        pi_conf = 0.95)$interval[, 1]
      res0 <- as.numeric(res0)
    }
  }
  res0 <- data.frame(res0)
  colnames(res0) <- c("q_pre.quantile = 0.05","q_pre.quantile = 0.95")
  res_new <- cbind(Xtest, res0)
  
  pred_col_1 = res_new$"q_pre.quantile = 0.05"
  pred_col_2 = res_new$"q_pre.quantile = 0.95"
  calib_sorted_1 <- res_new[order(pred_col_1), ]
  calib_sorted_2 <- res_new[order(pred_col_2), ]
  protected_col_1 = res_new[order(pred_col_1), ]$protected
  protected_col_2 = res_new[order(pred_col_2), ]$protected
  
  groupsa <- split(calib_sorted_1,  protected_col_1)#把 calib_sorted 按照 protected_col 列（受保护特征）分组
  groupsb <- split(calib_sorted_2,  protected_col_2)
  n_calib <- nrow(res_new)#计算行数
  taus <- seq(0, 1, length.out = n_calib)
  #seq(0, 1, length.out = n_calib) 创建了一个从 0 到 1 的等差序列，序列的长度由参数 length.out 指定，即 n_calib。
  #该函数会将 0 和 1 之间的数均匀分布到 n_calib 个元素中
  Ns1 <- sapply(groupsa, nrow)#根据分组计算每个分组的样本数 Ns 和占比 ps
  Ns2 <- sapply(groupsb, nrow)
  ps1 <- Ns1 / n_calib
  ps2 <- Ns2 / n_calib
  y1_fair <- rep(0, n_calib)#创建一个长度为 n_calib 的零向量，用于存储合成的公平化目标变量
  y2_fair <- rep(0, n_calib)
  interpolation = 'linear'
  sigma = 0
  for (group in groupsa) {
    original_subgroup_sorted <- calib_sorted_1$`q_pre.quantile = 0.05` + runif(n_calib,-sigma,sigma)
    #对排序后的预测结果q_pre.quantile进行操作，加了一个随机数，即q~
    fair_subgroup_sorted <-rep(NA, n_calib)
    # 创建一个长度为 n_calib 的空向量 fair_subgroup_sorted，用于存储公平化的子组预测结果。
    for (k in 1:n_calib) {
      fair_subgroup_sorted[k] <- quantile(original_subgroup_sorted,taus[k])
      #使用 quantile 函数，基于 original_subgroup_sorted（q~）的值和 taus[k] 的位置，对排序后的预测结果进行插值，得到公平化的子组预测结果。
      #具体来说，它将计算在 original_subgroup_sorted 中对应 taus[k] 位置的分位数值，作为 fair_subgroup_sorted[k] 的值。
    }
    
    y1_fair <- y1_fair + ps1*fair_subgroup_sorted
  }
  pred_col_2 = res_new$"q_pre.quantile = 0.95"
  calib_sorted_2 <- res_new[order(pred_col_2), ]
  protected_col_2 = res_new[order(pred_col_2), ]$protected
  groupsb <- split(calib_sorted_2,  protected_col_2)
  Ns2 <- sapply(groupsb, nrow)
  ps2 <- Ns2 / n_calib
  y2_fair <- rep(0, n_calib)
  interpolation = 'linear'
  sigma = 0
  
  for (group in groupsb) {
    original_subgroup_sorted <- calib_sorted_2$`q_pre.quantile = 0.95` + runif(n_calib,-sigma,sigma)
    #对排序后的预测结果q_pre.quantile进行操作，加了一个随机数，即q~
    fair_subgroup_sorted <-rep(NA, n_calib)
    # 创建一个长度为 n_calib 的空向量 fair_subgroup_sorted，用于存储公平化的子组预测结果。
    for (k in 1:n_calib) {
      fair_subgroup_sorted[k] <- quantile(original_subgroup_sorted,taus[k])
      #使用 quantile 函数，基于 original_subgroup_sorted（q~）的值和 taus[k] 的位置，对排序后的预测结果进行插值，得到公平化的子组预测结果。
      #具体来说，它将计算在 original_subgroup_sorted 中对应 taus[k] 位置的分位数值，作为 fair_subgroup_sorted[k] 的值。
    }
    
    y2_fair <- y2_fair + ps2*fair_subgroup_sorted
  }
  
  y_fair <- t(rbind(y1_fair,y2_fair))
  return( y_fair)
  
}


# d2 <- quantBoosting1(Y, X, Xtest, quantiles = c(0.05,0.95), ndpost = 100)
# print(d1)


FairRF <-function(Y, X, Xtest, quantiles, ...){
  fit <- grf::quantile_forest(X, Y, quantiles = quantiles, ...)
  res0 <- predict(fit, Xtest, quantiles = quantiles)
  
  
  if (is.list(res0) && !is.data.frame(res0)){
    res0 <- res0$predictions
  }
  if (length(quantiles) == 1){
    res0 <- as.numeric(res0)
  } else {
    res0 <- as.matrix(res0)
  }
  res0 <- data.frame(res0)
  colnames(res0) <- c("q_pre.quantile = 0.05","q_pre.quantile = 0.95")
  res_new <- cbind(Xtest, res0)
  
  pred_col_1 = res_new$"q_pre.quantile = 0.05"
  pred_col_2 = res_new$"q_pre.quantile = 0.95"
  calib_sorted_1 <- res_new[order(pred_col_1), ]
  calib_sorted_2 <- res_new[order(pred_col_2), ]
  protected_col_1 = res_new[order(pred_col_1), ]$protected
  protected_col_2 = res_new[order(pred_col_2), ]$protected
  
  groupsa <- split(calib_sorted_1,  protected_col_1)#把 calib_sorted 按照 protected_col 列（受保护特征）分组
  groupsb <- split(calib_sorted_2,  protected_col_2)
  n_calib <- nrow(res_new)#计算行数
  taus <- seq(0, 1, length.out = n_calib)
  #seq(0, 1, length.out = n_calib) 创建了一个从 0 到 1 的等差序列，序列的长度由参数 length.out 指定，即 n_calib。
  #该函数会将 0 和 1 之间的数均匀分布到 n_calib 个元素中
  Ns1 <- sapply(groupsa, nrow)#根据分组计算每个分组的样本数 Ns 和占比 ps
  Ns2 <- sapply(groupsb, nrow)
  ps1 <- Ns1 / n_calib
  ps2 <- Ns2 / n_calib
  y1_fair <- rep(0, n_calib)#创建一个长度为 n_calib 的零向量，用于存储合成的公平化目标变量
  y2_fair <- rep(0, n_calib)
  interpolation = 'linear'
  sigma = 1e-4
  for (group in groupsa) {
    original_subgroup_sorted <- calib_sorted_1$`q_pre.quantile = 0.05` + runif(n_calib,-sigma,sigma)
    #对排序后的预测结果q_pre.quantile进行操作，加了一个随机数，即q~
    fair_subgroup_sorted <-rep(NA, n_calib)
    # 创建一个长度为 n_calib 的空向量 fair_subgroup_sorted，用于存储公平化的子组预测结果。
    for (k in 1:n_calib) {
      fair_subgroup_sorted[k] <- quantile(original_subgroup_sorted,taus[k])
      #使用 quantile 函数，基于 original_subgroup_sorted（q~）的值和 taus[k] 的位置，对排序后的预测结果进行插值，得到公平化的子组预测结果。
      #具体来说，它将计算在 original_subgroup_sorted 中对应 taus[k] 位置的分位数值，作为 fair_subgroup_sorted[k] 的值。
    }
    
    y1_fair <- y1_fair + ps1*fair_subgroup_sorted
  }
  
  
  pred_col_2 = res_new$"q_pre.quantile = 0.95"
  calib_sorted_2 <- res_new[order(pred_col_2), ]
  protected_col_2 = res_new[order(pred_col_2), ]$protected
  groupsb <- split(calib_sorted_2,  protected_col_2)
  Ns2 <- sapply(groupsb, nrow)
  ps2 <- Ns2 / n_calib
  y2_fair <- rep(0, n_calib)
  interpolation = 'linear'
  sigma = 1e-4
  
  for (group in groupsb) {
    original_subgroup_sorted <- calib_sorted_2$`q_pre.quantile = 0.95` + runif(n_calib,-sigma,sigma)
    #对排序后的预测结果q_pre.quantile进行操作，加了一个随机数，即q~
    fair_subgroup_sorted <-rep(NA, n_calib)
    # 创建一个长度为 n_calib 的空向量 fair_subgroup_sorted，用于存储公平化的子组预测结果。
    for (k in 1:n_calib) {
      fair_subgroup_sorted[k] <- quantile(original_subgroup_sorted,taus[k])
      #使用 quantile 函数，基于 original_subgroup_sorted（q~）的值和 taus[k] 的位置，对排序后的预测结果进行插值，得到公平化的子组预测结果。
      #具体来说，它将计算在 original_subgroup_sorted 中对应 taus[k] 位置的分位数值，作为 fair_subgroup_sorted[k] 的值。
    }
    
    y2_fair <- y2_fair + ps2*fair_subgroup_sorted
  }
  
  y_fair <- t(rbind(y1_fair,y2_fair))
  return( y_fair)
}

## intervals CI计算区间以及长度
summary_CI <- function(target, CI){
  len <- mean(CI[, 2] - CI[, 1])
  cr <- mean(target >= CI[, 1] & target <= CI[, 2])
  
  return(list(cr = cr, len = len))
}

## Get ITE intervals by inexact-nested-CQR
CQR_inexact_ITE_CI0 <- function(X, Y, T, Xtest, alpha = 0.05,
                                ...){
  CIfun <- conformalIte(X, Y, T,
                        alpha,
                        algo = "nest",
                        exact = FALSE,
                        ...)
  CI <- CIfun(Xtest)
  return(CI)
}

## Get ITE intervals by exact-nested-CQR
CQR_exact_ITE_CI0 <- function(X, Y, T, Xtest, alpha = 0.05, ...){
  CIfun <- conformalIte(X, Y, T,
                        alpha,
                        algo = "nest",
                        exact = TRUE,
                        ...)
  CI <- CIfun(Xtest)
  return(CI)
}

## Get ITE intervals by naive-CQR
CQR_naive_ITE_CI0 <- function(X, Y, T, Xtest, alpha = 0.05, ...){
  CIfun <- conformalIte(X, Y, T,
                        alpha,
                        algo = "naive",
                        ...)
  CI <- CIfun(Xtest)
  return(CI)
}

## Get ITE intervals by Causal Forest
CF_ITE_CI <- function(X, Y, T, Xtest, alpha = 0.05){
  fit <- grf::causal_forest(X, Y, T)
  pred <- predict(fit, Xtest, estimate.variance = TRUE)
  cutoff <- qnorm(alpha / 2, lower.tail = FALSE)
  CI <- data.frame(low = pred[, 1] - cutoff * sqrt(pred[, 2]),
                   high = pred[, 1] + cutoff * sqrt(pred[, 2]))
  return(CI)
}

quantl_syn <- function(calib_new, pred_col, protected_col, interpolation = 'linear', sigma = 1e-4) {
  # Generate sorted fair calibration set
  calib_sorted <- calib_new[order(pred_col), ]#按照5%分位数升序排序
  groups <- split(calib_sorted,  protected_col)
  n_calib <- nrow(calib_new)
  taus <- seq(0, 1, length.out = n_calib)
  Ns <- sapply(groups, nrow)
  ps <- Ns / n_calib
  
  y_fair <- rep(0, n_calib)
  for (group in groups) {
    original_subgroup_sorted <- calib_sorted$`q_pre.quantile= 0.05` + runif(n_calib,-sigma,sigma)
    fair_subgroup_sorted <-rep(NA, n_calib)
    for (k in 1:n_calib) {
      fair_subgroup_sorted[k] <- quantile(original_subgroup_sorted,taus[k])
    }
    
    y_fair <- y_fair + ps*fair_subgroup_sorted
  }
  
  return(y_fair)
}
options(max.print=100)

quantl_syn1 <- function(calib_new, pred_col, protected_col, interpolation = 'linear', sigma = 1e-4) {
  # Generate sorted fair calibration set
  calib_sorted <- calib_new[order(pred_col), ]#按照5%分位数升序排序
  groups <- split(calib_sorted,  protected_col)
  n_calib <- nrow(calib_new)
  taus <- seq(0, 1, length.out = n_calib)
  Ns <- sapply(groups, nrow)
  ps <- Ns / n_calib
  
  y_fair <- rep(0, n_calib)
  for (group in groups) {
    original_subgroup_sorted <- calib_sorted$`q_pre.quantile= 0.95` + runif(n_calib,-sigma,sigma)
    fair_subgroup_sorted <-rep(NA, n_calib)
    for (k in 1:n_calib) {
      fair_subgroup_sorted[k] <- quantile(original_subgroup_sorted,taus[k])
    }
    
    y_fair <- y_fair + ps*fair_subgroup_sorted
  }
  
  return(y_fair)
}

## Get counterfactual intervals by Causal Forest
CF_Cf_CI <- function(X, Y, T, Xtest){
  fit <- grf::causal_forest(X, Y, T)
  pred <- predict(fit, Xtest, estimate.variance = TRUE)
  CI <- data.frame(low = pred[, 1] - 1.96 * sqrt(pred[, 2]),
                   high = pred[, 1] + 1.96 * sqrt(pred[, 2]))
  
  return(CI)
}

## Get counterfactual intervals by X-learner
xlearner_Cf_CI <- function(X, Y, T, Xtest,
                           B = 50){
  if (B == 0){
    df_tau <- df_Y <- list(cr = NA, len = NA)
    return(list(tau = df_tau, Y1 = df_Y))
  }
  
  xl_rf <- causalToolbox::X_RF(feat = X, tr = T, yobs = Y, nthread = 0)
  cate_esti_rf <- causalToolbox::EstimateCate(xl_rf, Xtest)
  CI <- causalToolbox::CateCI(xl_rf, Xtest, B = B,
                              verbose = FALSE, nthread = 1)[, 2:3]
  return(CI)
}

str_outfun <- function(method){
  if (method == "RF"){
    if (!requireNamespace("randomForest")){
      stop("randomForest package needs to be installed")
    }
    return(RF)
  } else if (method == "quantRF"){
    if (!requireNamespace("grf")){
      stop("grf package needs to be installed")
    }
    return(quantRF)
  } else if (method == "Boosting"){
    if (!requireNamespace("gbm")){
      stop("gbm package needs to be installed")
    }
    return(Boosting)
  } else if (method == "quantBoosting"){
    if (!requireNamespace("gbm")){
      stop("gbm package needs to be installed")
    }
    return(quantBoosting)
  } else if (method == "BART"){
    if (!requireNamespace("bartMachine")){
      stop("bartMachine package needs to be installed")
    }
    return(BART)
  } else if (method == "quantBART"){
    if (!requireNamespace("bartMachine")){
      stop("bartMachine package needs to be installed")
    }
    return(quantBART)
  } else if (method == "FairRF"){
    if (!requireNamespace("grf")){
      stop("123")
    }
    return(FairRF)
  } else if (method == "FairBART"){
    if (!requireNamespace("bartMachine")){
      stop("bartMachine package needs to be installed")
    }
    return(FairBART)
  }else if (method == "FairBoosting"){
    if (!requireNamespace("gbm")){
      stop("gbm package needs to be installed")
    }
    return(FairBoosting)
  }else {
    stop(paste0(method, " is not supported. Please input a valid string or a function that meets the minimal requirements described in the man page"))
  }
}