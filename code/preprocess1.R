library(dplyr)
df1<- read.csv("./data/Expanded_data_with_more_features.csv")
df1 <- df1 %>%
  mutate(ParentEduc = if_else(ParentEduc %in% c("high school", "some high school"), 0, 1))
df1 <- df1 %>%
  mutate(T = if_else(TestPrep == "none", 0, 1))
df1 <- df1 %>% select(-TestPrep)
class(df1)
xfeatures <- c('Gender', 'EthnicGroup', 'LunchType', 'ParentMaritalStatus', 'PracticeSport', 'IsFirstChild', 'NrSiblings', 'TransportMeans', 'WklyStudyHours')
df1$Y <- rowMeans(df1[, c("MathScore", "ReadingScore", "WritingScore")], na.rm = TRUE)
df1 <- df1[, !(names(df1) %in% c("MathScore", "ReadingScore", "WritingScore"))]
protected_features <- 'ParentEduc'
df1[sapply(df1, is.character)] <- lapply(df1[sapply(df1, is.character)], as.factor)
df1[sapply(df1, is.factor)] <- lapply(df1[sapply(df1, is.factor)], as.integer)
#分类变量转换为整数编码
for (col in names(df1)) {
  if (class(df1[[col]]) == "factor") {
    df1[[col]] <- as.integer(df1[[col]])
  }
}
df1

##数据归一化处理
#install.packages('caret')
library(ggplot2)
library(lattice)
library(caret)
preprocessed_data <- preProcess(df1, method = "range")
scaled_data <- predict(preprocessed_data, newdata = df1) #归一化处理后的数据
#scaled_data
df1<- scaled_data
# 使用 dplyr 的 rename() 函数重命名列
df1 <- df1 %>%
  rename(
    X1 = Gender,
    X2 = EthnicGroup,
    S = ParentEduc,
    X3 = LunchType,
    X4 = ParentMaritalStatus,
    X5 = PracticeSport,
    X6 = IsFirstChild,
    X7 = NrSiblings,
    X8 = TransportMeans,
    X9 = WklyStudyHours
  )
write.csv(df1, "~/Desktop/cfcausal-master/Expanded_data_with_more_features_modified.csv", row.names = FALSE)
