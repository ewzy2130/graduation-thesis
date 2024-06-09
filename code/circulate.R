result1 <- data.frame(method = character(0), coverage = numeric(0), length = numeric(0), KS_lo = numeric(0), KS_up = numeric(0))
result2 <- data.frame(method = character(0), coverage = numeric(0), length = numeric(0), KS_lo = numeric(0), KS_up = numeric(0))
result3 <- data.frame(method = character(0), coverage = numeric(0), length = numeric(0), KS_lo = numeric(0), KS_up = numeric(0))
result4 <- data.frame(method = character(0), coverage = numeric(0), length = numeric(0), KS_lo = numeric(0), KS_up = numeric(0))
result5 <- data.frame(method = character(0), coverage = numeric(0), length = numeric(0), KS_lo = numeric(0), KS_up = numeric(0))
result6 <- data.frame(method = character(0), coverage = numeric(0), length = numeric(0), KS_lo = numeric(0), KS_up = numeric(0))

methods <- list(FairBoosting, FairBART, FairRF)
names(methods) <- c("FairBoosting", "FairBART", "FairRF")
cishu <- 100
# 外部循环：迭代不同的method
for (method_name in names(methods)) {
  method <- methods[[method_name]]
  for (i in 1:cishu) {
    B <- 0
    alpha <- 0.05
    ntrain = 5000
    ntest = 5000
    XnoS <- new_data$XnoS
    EYnoS0pre <- new_data$EYnoS0pre #EYnoS0pre是拟合的
    Etau <- new_data$Etau
    EYnoS1pre <- EYnoS0pre + Etau
    IQR0noS <- new_data$IQR0noS
    IQR1noS <- new_data$IQR1noS
    ps <- new_data$ps
    
    n <- length(ps)
    sigma0noS <- 0.5 * IQR0noS
    sigma1noS <- 0.5 * IQR1noS
    std <- sqrt(sigma0noS^2 + sigma1noS^2)
    err0 <- rnorm(n)
    err1 <- rnorm(n)
    #生成Y1和Y0
    YnoS0 <- EYnoS0pre + sigma0noS * err0 # 8312
    YnoS1 <- EYnoS1pre + sigma1noS * err1 # 8312
    
    ids <- sample(n, n, FALSE) # 8312
    trainid <- ids[1:ntrain]
    testid <- ids[ntrain + 1:ntest]
    
    XnoStrain <- XnoS[trainid, ]
    pstrain <- pmin(ps[trainid], 1)
    Ttrain <- as.numeric(runif(ntrain) <= pstrain)
    YnoS1train <- YnoS1[trainid] #生成的
    YnoS0train <- YnoS0[trainid] #生成的
    YnoStrain <- ifelse(Ttrain == 1, YnoS1train, YnoS0train)
    
    Xtest <- X[testid, ]
    YnoS1test <- YnoS1[testid]
    YnoS0test <- YnoS0[testid]
    #tautest <- Y1test - Y0test
    new_tautest <- YnoS1test - YnoS0test # 是我们关注的ITE
    CATEtest <- Etau[testid]
    stdtest <- std[testid]
    pstest <- pmin(ps[testid], 1)
    Ttest <- as.numeric(runif(ntest) <= pstest)
    YnoStest <- ifelse(Ttest == 1, YnoS1test, YnoS0test)
    
    Xtrain <- X[trainid, ]
    
    data_simul <- list(Xtrain = Xtrain, Ttrain = Ttrain, new_Ytrain = YnoStrain,
                       Xtest = Xtest, Ttest = Ttest, new_Ytest = YnoStest,
                       new_tautest=new_tautest, CATEtest = CATEtest,
                       stdtest = stdtest)
    
    Xtrain <- data_simul$Xtrain
    new_Ytrain <- data_simul$new_Ytrain
    Ttrain <- data_simul$Ttrain
    Xtest <- data_simul$Xtest
    tautest <- data_simul$tautest
    CATEtest <- data_simul$CATEtest
    stdtest <- data_simul$stdtest
    
    rbind(df0, df1, df2)
    
    quantiles <- c(alpha / 2, 1 - alpha / 2)
    
    colnames(X)[colnames(X) == "S"] <- "protected"
    colnames(Xtest)[colnames(Xtest) == "S"] <- "protected"
    colnames(Xtrain)[colnames(Xtrain) == "S"] <- "protected"
    
    Q1 <- CQR_inexact_ITE_CI0(Xtrain, new_Ytrain, Ttrain, Xtest, alpha, quantiles = quantiles, outfun = method)
    QA1 <- cbind(Xtest,Q1)
    colnames(QA1)[colnames(QA1) == "lower"] <- "q_pre.quantile= 0.05"
    y1fair <- quantl_syn(QA1,pred_col = QA1$"q_pre.quantile= 0.05",protected_col = QA1$protected)
    colnames(QA1)[colnames(QA1) == "upper"] <- "q_pre.quantile= 0.95"
    y2fair <- quantl_syn1(QA1,pred_col = QA1$"q_pre.quantile= 0.95",protected_col = QA1$protected)
    fair <- cbind(QA1,y1fair,y2fair)
    #write.csv(fair,'C:/Users/dell/Desktop/我的论文十月份汇总/fairresult1.csv')
    group01 <- fair[fair$"protected" == 1, ]
    group02 <- fair[fair$"protected" == 0, ]
    datalow1 <- group01$y1fair
    datalow2 <- group02$y1fair
    # 使用 ks.test() 函数计算 KS 距离
    ks_lo <- ks.test(datalow1, datalow2)
    
    dataup1 <- group01$y2fair
    dataup2 <- group02$y2fair
    # 使用 ks.test() 函数计算 KS 距离
    ks_up <- ks.test(dataup1, dataup2)
    
    FQ1 <- data.frame(y1fair = y1fair, y2fair = y2fair)
    coverage1 <- summary_CI(new_tautest, FQ1)
    result1_list <- list(
      method = method_name,
      coverage = coverage1$cr,
      length = coverage1$len,
      KS_lo = ks_lo$statistic,  
      KS_up = ks_up$statistic   
    )
    
    result1 <- rbind(result1, result1_list)
  }
}
# 外部循环：迭代不同的method
for (method_name in names(methods)) {
  method <- methods[[method_name]]
  for (i in 1:cishu) {
    B <- 0
    alpha <- 0.05
    ntrain = 5000
    ntest = 5000
    XnoS <- new_data$XnoS
    EYnoS0pre <- new_data$EYnoS0pre #EYnoS0pre是拟合的
    Etau <- new_data$Etau
    EYnoS1pre <- EYnoS0pre + Etau
    IQR0noS <- new_data$IQR0noS
    IQR1noS <- new_data$IQR1noS
    ps <- new_data$ps
    
    n <- length(ps)
    sigma0noS <- 0.5 * IQR0noS
    sigma1noS <- 0.5 * IQR1noS
    std <- sqrt(sigma0noS^2 + sigma1noS^2)
    err0 <- rnorm(n)
    err1 <- rnorm(n)
    #生成Y1和Y0
    YnoS0 <- EYnoS0pre + sigma0noS * err0 # 8312
    YnoS1 <- EYnoS1pre + sigma1noS * err1 # 8312
    
    ids <- sample(n, n, FALSE) # 8312
    trainid <- ids[1:ntrain]
    testid <- ids[ntrain + 1:ntest]
    
    XnoStrain <- XnoS[trainid, ]
    pstrain <- pmin(ps[trainid], 1)
    Ttrain <- as.numeric(runif(ntrain) <= pstrain)
    YnoS1train <- YnoS1[trainid] #生成的
    YnoS0train <- YnoS0[trainid] #生成的
    YnoStrain <- ifelse(Ttrain == 1, YnoS1train, YnoS0train)
    
    Xtest <- X[testid, ]
    YnoS1test <- YnoS1[testid]
    YnoS0test <- YnoS0[testid]
    #tautest <- Y1test - Y0test
    new_tautest <- YnoS1test - YnoS0test # 是我们关注的ITE
    CATEtest <- Etau[testid]
    stdtest <- std[testid]
    pstest <- pmin(ps[testid], 1)
    Ttest <- as.numeric(runif(ntest) <= pstest)
    YnoStest <- ifelse(Ttest == 1, YnoS1test, YnoS0test)
    
    Xtrain <- X[trainid, ]
    
    data_simul <- list(Xtrain = Xtrain, Ttrain = Ttrain, new_Ytrain = YnoStrain,
                       Xtest = Xtest, Ttest = Ttest, new_Ytest = YnoStest,
                       new_tautest=new_tautest, CATEtest = CATEtest,
                       stdtest = stdtest)
    
    Xtrain <- data_simul$Xtrain
    new_Ytrain <- data_simul$new_Ytrain
    Ttrain <- data_simul$Ttrain
    Xtest <- data_simul$Xtest
    tautest <- data_simul$tautest
    CATEtest <- data_simul$CATEtest
    stdtest <- data_simul$stdtest
    
    rbind(df0, df1, df2)
    
    quantiles <- c(alpha / 2, 1 - alpha / 2)
    
    colnames(X)[colnames(X) == "S"] <- "protected"
    colnames(Xtest)[colnames(Xtest) == "S"] <- "protected"
    colnames(Xtrain)[colnames(Xtrain) == "S"] <- "protected"
    
    Q2 <- CQR_exact_ITE_CI0(Xtrain, new_Ytrain, Ttrain, Xtest, alpha, quantiles = quantiles, outfun = method)
    QA2 <- cbind(Xtest,Q2)
    colnames(QA2)[colnames(QA2) == "lower"] <- "q_pre.quantile= 0.05"
    y1fair <- quantl_syn(QA2,pred_col = QA2$"q_pre.quantile= 0.05",protected_col = QA2$protected)
    colnames(QA2)[colnames(QA2) == "upper"] <- "q_pre.quantile= 0.95"
    y2fair <- quantl_syn1(QA2,pred_col = QA2$"q_pre.quantile= 0.95",protected_col = QA2$protected)
    fair <- cbind(QA2,y1fair,y2fair)
    #write.csv(fair,'C:/Users/dell/Desktop/我的论文十月份汇总/fairresult1.csv')
    group01 <- fair[fair$"protected" == 1, ]
    group02 <- fair[fair$"protected" == 0, ]
    datalow1 <- group01$y1fair
    datalow2 <- group02$y1fair
    # 使用 ks.test() 函数计算 KS 距离
    ks_lo <- ks.test(datalow1, datalow2)
    
    dataup1 <- group01$y2fair
    dataup2 <- group02$y2fair
    # 使用 ks.test() 函数计算 KS 距离
    ks_up <- ks.test(dataup1, dataup2)
    
    FQ2 <- data.frame(y1fair = y1fair, y2fair = y2fair)
    coverage2 <- summary_CI(new_tautest, FQ2)
    result2_list <- list(
      method = method_name,
      coverage = coverage2$cr,
      length = coverage2$len,
      KS_lo = ks_lo$statistic,  
      KS_up = ks_up$statistic   
    )
    
    result2 <- rbind(result2, result2_list)
  }
}
# 外部循环：迭代不同的method
for (method_name in names(methods)) {
  method <- methods[[method_name]]
  for (i in 1:cishu) {
    B <- 0
    alpha <- 0.05
    ntrain = 5000
    ntest = 5000
    XnoS <- new_data$XnoS
    EYnoS0pre <- new_data$EYnoS0pre #EYnoS0pre是拟合的
    Etau <- new_data$Etau
    EYnoS1pre <- EYnoS0pre + Etau
    IQR0noS <- new_data$IQR0noS
    IQR1noS <- new_data$IQR1noS
    ps <- new_data$ps
    
    n <- length(ps)
    sigma0noS <- 0.5 * IQR0noS
    sigma1noS <- 0.5 * IQR1noS
    std <- sqrt(sigma0noS^2 + sigma1noS^2)
    err0 <- rnorm(n)
    err1 <- rnorm(n)
    #生成Y1和Y0
    YnoS0 <- EYnoS0pre + sigma0noS * err0 # 8312
    YnoS1 <- EYnoS1pre + sigma1noS * err1 # 8312
    
    ids <- sample(n, n, FALSE) # 8312
    trainid <- ids[1:ntrain]
    testid <- ids[ntrain + 1:ntest]
    
    XnoStrain <- XnoS[trainid, ]
    pstrain <- pmin(ps[trainid], 1)
    Ttrain <- as.numeric(runif(ntrain) <= pstrain)
    YnoS1train <- YnoS1[trainid] #生成的
    YnoS0train <- YnoS0[trainid] #生成的
    YnoStrain <- ifelse(Ttrain == 1, YnoS1train, YnoS0train)
    
    Xtest <- X[testid, ]
    YnoS1test <- YnoS1[testid]
    YnoS0test <- YnoS0[testid]
    #tautest <- Y1test - Y0test
    new_tautest <- YnoS1test - YnoS0test # 是我们关注的ITE
    CATEtest <- Etau[testid]
    stdtest <- std[testid]
    pstest <- pmin(ps[testid], 1)
    Ttest <- as.numeric(runif(ntest) <= pstest)
    YnoStest <- ifelse(Ttest == 1, YnoS1test, YnoS0test)
    
    Xtrain <- X[trainid, ]
    
    data_simul <- list(Xtrain = Xtrain, Ttrain = Ttrain, new_Ytrain = YnoStrain,
                       Xtest = Xtest, Ttest = Ttest, new_Ytest = YnoStest,
                       new_tautest=new_tautest, CATEtest = CATEtest,
                       stdtest = stdtest)
    
    Xtrain <- data_simul$Xtrain
    new_Ytrain <- data_simul$new_Ytrain
    Ttrain <- data_simul$Ttrain
    Xtest <- data_simul$Xtest
    tautest <- data_simul$tautest
    CATEtest <- data_simul$CATEtest
    stdtest <- data_simul$stdtest
    
    rbind(df0, df1, df2)
    
    quantiles <- c(alpha / 2, 1 - alpha / 2)
    
    colnames(X)[colnames(X) == "S"] <- "protected"
    colnames(Xtest)[colnames(Xtest) == "S"] <- "protected"
    colnames(Xtrain)[colnames(Xtrain) == "S"] <- "protected"
    
    Q3 <- CQR_naive_ITE_CI0(Xtrain, new_Ytrain, Ttrain, Xtest, alpha, quantiles = quantiles, outfun = method)
    QA3 <- cbind(Xtest,Q3)
    colnames(QA3)[colnames(QA3) == "lower"] <- "q_pre.quantile= 0.05"
    y1fair <- quantl_syn(QA3,pred_col = QA3$"q_pre.quantile= 0.05",protected_col = QA3$protected)
    colnames(QA3)[colnames(QA3) == "upper"] <- "q_pre.quantile= 0.95"
    y2fair <- quantl_syn1(QA3,pred_col = QA3$"q_pre.quantile= 0.95",protected_col = QA3$protected)
    fair <- cbind(QA3,y1fair,y2fair)
    #write.csv(fair,'C:/Users/dell/Desktop/我的论文十月份汇总/fairresult1.csv')
    group01 <- fair[fair$"protected" == 1, ]
    group02 <- fair[fair$"protected" == 0, ]
    datalow1 <- group01$y1fair
    datalow2 <- group02$y1fair
    # 使用 ks.test() 函数计算 KS 距离
    ks_lo <- ks.test(datalow1, datalow2)
    
    dataup1 <- group01$y2fair
    dataup2 <- group02$y2fair
    # 使用 ks.test() 函数计算 KS 距离
    ks_up <- ks.test(dataup1, dataup2)
    
    FQ3 <- data.frame(y1fair = y1fair, y2fair = y2fair)
    coverage3 <- summary_CI(new_tautest, FQ3)
    result3_list <- list(
      method = method_name,
      coverage = coverage3$cr,
      length = coverage3$len,
      KS_lo = ks_lo$statistic,  
      KS_up = ks_up$statistic   
    )
    
    result3 <- rbind(result3, result3_list)
  }
}

methods <- c("quantBoosting", "quantBART", "quantRF")
# 外部循环：迭代不同的method
for (method in methods) {
  for (i in 1:cishu) {
    B <- 0
    alpha <- 0.05
    ntrain = 5000
    ntest = 5000
    XnoS <- new_data$XnoS
    EYnoS0pre <- new_data$EYnoS0pre #EYnoS0pre是拟合的
    Etau <- new_data$Etau
    EYnoS1pre <- EYnoS0pre + Etau
    IQR0noS <- new_data$IQR0noS
    IQR1noS <- new_data$IQR1noS
    ps <- new_data$ps
    
    n <- length(ps)
    sigma0noS <- 0.5 * IQR0noS
    sigma1noS <- 0.5 * IQR1noS
    std <- sqrt(sigma0noS^2 + sigma1noS^2)
    err0 <- rnorm(n)
    err1 <- rnorm(n)
    #生成Y1和Y0
    YnoS0 <- EYnoS0pre + sigma0noS * err0 # 8312
    YnoS1 <- EYnoS1pre + sigma1noS * err1 # 8312
    
    ids <- sample(n, n, FALSE) # 8312
    trainid <- ids[1:ntrain]
    testid <- ids[ntrain + 1:ntest]
    
    XnoStrain <- XnoS[trainid, ]
    pstrain <- pmin(ps[trainid], 1)
    Ttrain <- as.numeric(runif(ntrain) <= pstrain)
    YnoS1train <- YnoS1[trainid] #生成的
    YnoS0train <- YnoS0[trainid] #生成的
    YnoStrain <- ifelse(Ttrain == 1, YnoS1train, YnoS0train)
    
    Xtest <- X[testid, ]
    YnoS1test <- YnoS1[testid]
    YnoS0test <- YnoS0[testid]
    #tautest <- Y1test - Y0test
    new_tautest <- YnoS1test - YnoS0test # 是我们关注的ITE
    CATEtest <- Etau[testid]
    stdtest <- std[testid]
    pstest <- pmin(ps[testid], 1)
    Ttest <- as.numeric(runif(ntest) <= pstest)
    YnoStest <- ifelse(Ttest == 1, YnoS1test, YnoS0test)
    
    Xtrain <- X[trainid, ]
    
    data_simul <- list(Xtrain = Xtrain, Ttrain = Ttrain, new_Ytrain = YnoStrain,
                       Xtest = Xtest, Ttest = Ttest, new_Ytest = YnoStest,
                       new_tautest=new_tautest, CATEtest = CATEtest,
                       stdtest = stdtest)
    
    Xtrain <- data_simul$Xtrain
    new_Ytrain <- data_simul$new_Ytrain
    Ttrain <- data_simul$Ttrain
    Xtest <- data_simul$Xtest
    tautest <- data_simul$tautest
    CATEtest <- data_simul$CATEtest
    stdtest <- data_simul$stdtest
    
    rbind(df0, df1, df2)
    
    quantiles <- c(alpha / 2, 1 - alpha / 2)
    
    colnames(X)[colnames(X) == "S"] <- "protected"
    colnames(Xtest)[colnames(Xtest) == "S"] <- "protected"
    colnames(Xtrain)[colnames(Xtrain) == "S"] <- "protected"
    
    Q4 <- CQR_inexact_ITE_CI0(Xtrain, new_Ytrain, Ttrain,
                              Xtest, alpha, 
                              quantiles = quantiles,
                              outfun = method)
    QA4 <- cbind(Xtest, new_tautest, Q4)
    
    # 计算覆盖率和 KS 统计量
    coverage4 <- summary_CI(new_tautest, QA4)
    group1 <- QA4[QA4$"protected" == 1, ]
    group2 <- QA4[QA4$"protected" == 0, ]
    dataup1 <- group1$upper
    dataup2 <- group2$upper
    datalo1 <- group1$lower
    datalo2 <- group2$lower
    ks_lo <- ks.test(datalo1, datalo2)
    ks_up <- ks.test(dataup1, dataup2)
    
    result4_list <- list(
      method = method,
      coverage = coverage4$cr,
      length = coverage4$len,
      KS_lo = ks_lo$statistic,
      KS_up = ks_up$statistic
    )
    
    result4 <- rbind(result4, result4_list)
  }
}
for (method in methods) {
  for (i in 1:cishu) {
    B <- 0
    alpha <- 0.05
    ntrain = 5000
    ntest = 5000
    XnoS <- new_data$XnoS
    EYnoS0pre <- new_data$EYnoS0pre #EYnoS0pre是拟合的
    Etau <- new_data$Etau
    EYnoS1pre <- EYnoS0pre + Etau
    IQR0noS <- new_data$IQR0noS
    IQR1noS <- new_data$IQR1noS
    ps <- new_data$ps
    
    n <- length(ps)
    sigma0noS <- 0.5 * IQR0noS
    sigma1noS <- 0.5 * IQR1noS
    std <- sqrt(sigma0noS^2 + sigma1noS^2)
    err0 <- rnorm(n)
    err1 <- rnorm(n)
    #生成Y1和Y0
    YnoS0 <- EYnoS0pre + sigma0noS * err0 # 8312
    YnoS1 <- EYnoS1pre + sigma1noS * err1 # 8312
    
    ids <- sample(n, n, FALSE) # 8312
    trainid <- ids[1:ntrain]
    testid <- ids[ntrain + 1:ntest]
    
    XnoStrain <- XnoS[trainid, ]
    pstrain <- pmin(ps[trainid], 1)
    Ttrain <- as.numeric(runif(ntrain) <= pstrain)
    YnoS1train <- YnoS1[trainid] #生成的
    YnoS0train <- YnoS0[trainid] #生成的
    YnoStrain <- ifelse(Ttrain == 1, YnoS1train, YnoS0train)
    
    Xtest <- X[testid, ]
    YnoS1test <- YnoS1[testid]
    YnoS0test <- YnoS0[testid]
    #tautest <- Y1test - Y0test
    new_tautest <- YnoS1test - YnoS0test # 是我们关注的ITE
    CATEtest <- Etau[testid]
    stdtest <- std[testid]
    pstest <- pmin(ps[testid], 1)
    Ttest <- as.numeric(runif(ntest) <= pstest)
    YnoStest <- ifelse(Ttest == 1, YnoS1test, YnoS0test)
    
    Xtrain <- X[trainid, ]
    
    data_simul <- list(Xtrain = Xtrain, Ttrain = Ttrain, new_Ytrain = YnoStrain,
                       Xtest = Xtest, Ttest = Ttest, new_Ytest = YnoStest,
                       new_tautest=new_tautest, CATEtest = CATEtest,
                       stdtest = stdtest)
    
    Xtrain <- data_simul$Xtrain
    new_Ytrain <- data_simul$new_Ytrain
    Ttrain <- data_simul$Ttrain
    Xtest <- data_simul$Xtest
    tautest <- data_simul$tautest
    CATEtest <- data_simul$CATEtest
    stdtest <- data_simul$stdtest
    
    rbind(df0, df1, df2)
    
    quantiles <- c(alpha / 2, 1 - alpha / 2)
    
    colnames(X)[colnames(X) == "S"] <- "protected"
    colnames(Xtest)[colnames(Xtest) == "S"] <- "protected"
    colnames(Xtrain)[colnames(Xtrain) == "S"] <- "protected"
    
    Q5 <- CQR_exact_ITE_CI0(Xtrain, new_Ytrain, Ttrain,
                              Xtest, alpha, 
                              quantiles = quantiles,
                              outfun = method)
    QA5 <- cbind(Xtest, new_tautest, Q5)
    
    # 计算覆盖率和 KS 统计量
    coverage5 <- summary_CI(new_tautest, QA5)
    group1 <- QA5[QA5$"protected" == 1, ]
    group2 <- QA5[QA5$"protected" == 0, ]
    dataup1 <- group1$upper
    dataup2 <- group2$upper
    datalo1 <- group1$lower
    datalo2 <- group2$lower
    ks_lo <- ks.test(datalo1, datalo2)
    ks_up <- ks.test(dataup1, dataup2)
    
    result5_list <- list(
      method = method,
      coverage = coverage5$cr,
      length = coverage5$len,
      KS_lo = ks_lo$statistic,
      KS_up = ks_up$statistic
    )
    
    result5 <- rbind(result5, result5_list)
  }
}
for (method in methods) {
  for (i in 1:cishu) {
    B <- 0
    alpha <- 0.05
    ntrain = 5000
    ntest = 5000
    XnoS <- new_data$XnoS
    EYnoS0pre <- new_data$EYnoS0pre #EYnoS0pre是拟合的
    Etau <- new_data$Etau
    EYnoS1pre <- EYnoS0pre + Etau
    IQR0noS <- new_data$IQR0noS
    IQR1noS <- new_data$IQR1noS
    ps <- new_data$ps
    
    n <- length(ps)
    sigma0noS <- 0.5 * IQR0noS
    sigma1noS <- 0.5 * IQR1noS
    std <- sqrt(sigma0noS^2 + sigma1noS^2)
    err0 <- rnorm(n)
    err1 <- rnorm(n)
    #生成Y1和Y0
    YnoS0 <- EYnoS0pre + sigma0noS * err0 # 8312
    YnoS1 <- EYnoS1pre + sigma1noS * err1 # 8312
    
    ids <- sample(n, n, FALSE) # 8312
    trainid <- ids[1:ntrain]
    testid <- ids[ntrain + 1:ntest]
    
    XnoStrain <- XnoS[trainid, ]
    pstrain <- pmin(ps[trainid], 1)
    Ttrain <- as.numeric(runif(ntrain) <= pstrain)
    YnoS1train <- YnoS1[trainid] #生成的
    YnoS0train <- YnoS0[trainid] #生成的
    YnoStrain <- ifelse(Ttrain == 1, YnoS1train, YnoS0train)
    
    Xtest <- X[testid, ]
    YnoS1test <- YnoS1[testid]
    YnoS0test <- YnoS0[testid]
    #tautest <- Y1test - Y0test
    new_tautest <- YnoS1test - YnoS0test # 是我们关注的ITE
    CATEtest <- Etau[testid]
    stdtest <- std[testid]
    pstest <- pmin(ps[testid], 1)
    Ttest <- as.numeric(runif(ntest) <= pstest)
    YnoStest <- ifelse(Ttest == 1, YnoS1test, YnoS0test)
    
    Xtrain <- X[trainid, ]
    
    data_simul <- list(Xtrain = Xtrain, Ttrain = Ttrain, new_Ytrain = YnoStrain,
                       Xtest = Xtest, Ttest = Ttest, new_Ytest = YnoStest,
                       new_tautest=new_tautest, CATEtest = CATEtest,
                       stdtest = stdtest)
    
    Xtrain <- data_simul$Xtrain
    new_Ytrain <- data_simul$new_Ytrain
    Ttrain <- data_simul$Ttrain
    Xtest <- data_simul$Xtest
    tautest <- data_simul$tautest
    CATEtest <- data_simul$CATEtest
    stdtest <- data_simul$stdtest
    
    rbind(df0, df1, df2)
    
    quantiles <- c(alpha / 2, 1 - alpha / 2)
    
    colnames(X)[colnames(X) == "S"] <- "protected"
    colnames(Xtest)[colnames(Xtest) == "S"] <- "protected"
    colnames(Xtrain)[colnames(Xtrain) == "S"] <- "protected"
    
    Q6 <- CQR_naive_ITE_CI0(Xtrain, new_Ytrain, Ttrain,
                            Xtest, alpha, 
                            quantiles = quantiles,
                            outfun = method)
    QA6 <- cbind(Xtest, new_tautest, Q6)
    
    # 计算覆盖率和 KS 统计量
    coverage6 <- summary_CI(new_tautest, QA6)
    group1 <- QA6[QA6$"protected" == 1, ]
    group2 <- QA6[QA6$"protected" == 0, ]
    dataup1 <- group1$upper
    dataup2 <- group2$upper
    datalo1 <- group1$lower
    datalo2 <- group2$lower
    ks_lo <- ks.test(datalo1, datalo2)
    ks_up <- ks.test(dataup1, dataup2)
    
    result6_list <- list(
      method = method,
      coverage = coverage6$cr,
      length = coverage6$len,
      KS_lo = ks_lo$statistic,
      KS_up = ks_up$statistic
    )
    
    result6 <- rbind(result6, result6_list)
  }
}

print(result1)
print(result2)
print(result3)
print(result4)
print(result5)
print(result6)

types <- c("Inexact", "Exact", "Naive")

result1$type <- "Inexact"
result2$type <- "Exact"
result3$type <- "Naive"
result4$type <- "Inexact"
result5$type <- "Exact"
result6$type <- "Naive"

result1$exprid <- 1
result2$exprid <- 2
result3$exprid <- 3
result4$exprid <- 1
result5$exprid <- 2
result6$exprid <- 3

write.csv(result1, file = "./results/new_in_RF.csv", row.names = FALSE)
write.csv(result2, file = "./results/new_ex_RF.csv", row.names = FALSE)
write.csv(result3, file = "./results/new_na_RF.csv", row.names = FALSE)
write.csv(result4, file = "./results/old_in_RF.csv", row.names = FALSE)
write.csv(result5, file = "./results/old_ex_RF.csv", row.names = FALSE)
write.csv(result6, file = "./results/old_na_RF.csv", row.names = FALSE)
