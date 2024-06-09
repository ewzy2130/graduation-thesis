# 观察原始数据中是否有不公平，计算ks距离和图片
library(tidyverse)
library(cfcausal)
library(ggplot2)
library(grf)
# 读取数据
data <- read.csv("./data/Expanded_data_with_more_features_modified.csv")
X <- data %>% select(-T, -Y) %>% as.matrix
XnoS <- X[, !(colnames(X) == "S")] # 获取不含敏感变量S列的X
Y <- data$Y
T <- data$T
n <- length(Y)

group0 <- data[data$"S" == 0, ]
group1 <- data[data$"S" == 1, ]
data0 <- group0$Y
data1 <- group1$Y
data2 <- data$Y
ks_result <- ks.test(data0, data1)
print(ks_result)

# 创建数据框
df0 <- data.frame(value = data0, group = "Density of Low education")
df1 <- data.frame(value = data1, group = "Density of highly educated")
df2 <- data.frame(value = data2, group = "Density of overall")
df <- rbind(df0, df1, df2)

# 检查数据框内容
print(head(df))
print(table(df$group))

# 绘制概率密度图
ggplot(df, aes(x = value, color = group)) +
  geom_density(size = 1, alpha = 0.5, bw = 0.15) +  # 调整带宽参数
  labs(y = "Density") +
  scale_color_manual(values = c("Density of highly educated" = "#1f78b4", "Density of Low education" = "#33a02c", "Density of overall" = "#e41a1c")) +  # 使用不同颜色
  scale_linetype_manual(values = c("Density of highly educated" = "solid", "Density of Low education" = "solid", "Density of overall" = "solid")) +  # 使用实线
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +  # 设置纵轴范围从0到2
  coord_cartesian(xlim = c(0, 1.2)) +  # 设置x轴范围从0到1.2
  theme_minimal() +  # 使用最小主题
  theme(panel.background = element_blank(),  # 删除背景色
        panel.grid.major = element_blank(),  # 删除主要网格线
        panel.grid.minor = element_blank(),  # 删除次要网格线
        axis.line = element_line(color = "black"),  # 设置坐标轴线
        panel.border = element_blank(),  # 删除边框线
        axis.text = element_blank(),  # 删除刻度标签
        axis.title = element_blank(),  # 删除坐标轴标题
        legend.position = c(0.9, 0.9))  # 将图例放在右上角

# 数据分割 train样本有5814个
tmp <- sample(n, n)#生成1到n的n个随机序列
ntrain <- ceiling(0.2 * n)
trainid <- tmp[1:ntrain]#训练集的id
valid <- tmp[(ntrain + 1):n]#将随机序列tmp中的剩余部分作为验证集的索引

# 提取训练集的X, XnoS, Y, T
Xtrain <- X[trainid, ]
XnoStrain <- XnoS[trainid, ]
Ytrain <- Y[trainid]
Ttrain <- T[trainid]

# 把训练集分成两部分，一部分Y是Y1train,一部分Y是Y0train
Y1train <- Ytrain[Ttrain == 1]
X1train <- Xtrain[Ttrain == 1, ]
XnoS1train <- XnoStrain[Ttrain == 1, ]
Y0train <- Ytrain[Ttrain == 0]
X0train <- Xtrain[Ttrain == 0, ]
XnoS0train <- XnoStrain[Ttrain == 0, ]

# 剩下的23255个样本
Xval <- X[-trainid, ]
Tval <- T[-trainid]
Yval <- Y[-trainid]
XnoSval <- XnoS[-trainid, ]

# 处理列名
colnames(Xval)[colnames(Xval) == "S"] <- "protected"
colnames(X0train)[colnames(X0train) == "S"] <- "protected"
colnames(X1train)[colnames(X1train) == "S"] <- "protected"
colnames(Xtrain)[colnames(Xtrain) == "S"] <- "protected"

# 
Xcate <- as.matrix(Xval[, c("X1","X2","X3","X4","X5","X6","X7","X8","X9")])
crf <- causal_forest(Xcate,Yval,Tval)
Etau <- predict(crf, Xcate)$predictions

# Fit models
EYnoS0val <- cfcausal:::RF(Y0train, XnoS0train, XnoSval)
EY0val <- cfcausal:::RF(Y0train, X0train, Xval)# 在train上生成X(val)对应的EY0，输出表示为m-hat(x0)

#IQR0 <- myRF(Y0, X0, X, quantiles = c(0.25, 0.75))
IQRnoS0val <- cfcausal:::quantRF(Y0train, XnoS0train, XnoSval, quantiles = c(0.25, 0.75))
IQRnoS0val <- IQRnoS0val[, 2] - IQRnoS0val[, 1]
IQRnoS1val <- cfcausal:::quantRF(Y1train, XnoS1train, XnoSval, quantiles = c(0.25, 0.75))
IQRnoS1val <- IQRnoS1val[, 2] - IQRnoS1val[, 1]
#ps <- cfcausal:::RF(as.factor(Ttrain), X_noStrain, X_noSval)
#ps <- cfcausal:::RF(as.factor(Ttrain), Xtrain, Xval)
ps <- cfcausal:::Boosting(as.factor(Ttrain), Xtrain, Xval)

source("./code/function.R")
IQR0val <- FairBoosting(Y0train, X0train, Xval, quantiles = c(0.25, 0.75))
IQR0val <- IQR0val[, 2] - IQR0val[, 1]# 计算了Y0的四分位距
IQR1val <- FairBoosting(Y1train, X1train, Xval, quantiles = c(0.25, 0.75))
IQR1val <- IQR1val[, 2] - IQR1val[, 1]# 计算了Y1的四分位距
# ps <- cfcausal:::RF(as.factor(Ttrain), Xtrain, Xval)# 拟合得到倾向得分，并使其截断在0.1到0.9
ps <- pmin(pmax(ps, 0.1), 0.9)

# Store the data
data <- list(X = Xval, EY0pre = EY0val, Etau = Etau, IQR0 = IQR0val, IQR1 = IQR1val, ps = ps)
save(data, file = "./data/val.RData")
new_data <- list(XnoS = XnoSval, EYnoS0pre = EYnoS0val, Etau = Etau, IQR0noS = IQRnoS0val, IQR1noS = IQRnoS1val, ps = ps)
save(new_data, file = "./data/noSval.RData")
#这个文件是基于数据生成合成数据集，下文将基于该数据集每次生成10000个四元组等