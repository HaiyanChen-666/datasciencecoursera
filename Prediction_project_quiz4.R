######按照要求指定R包######
###########设置环境（只需做一次）#############
# 安装 renv
install.packages("renv")

# 创建项目文件夹
dir.create("E:/coursera_temporary/quiz_environment")
setwd("E:/coursera_temporary/quiz_environment")

# 初始化 renv
renv::init()

# 安装指定版本的包
renv::install("AppliedPredictiveModeling@1.1-6")
renv::install("caret@6.0-47")
renv::install("ElemStatLearn@2012.04-0")
renv::install("pgmm@1.1")
renv::install("rpart@4.1-8")
renv::install("gbm@2.1")
renv::install("lubridate@1.3-3")
renv::install("forecast@5.6")
renv::install("e1071@1.6-4")

# 保存环境快照
renv::snapshot()

# 在新机器上，只需运行：
# renv::restore()  # 会自动安装 lock 文件中记录的所有包版本[citation:5][citation:7]


# 进入您创建的项目文件夹
# 进入项目文件夹
setwd("E:/coursera_temporary/quiz_environment")

# 激活 renv 环境
renv::activate()

# 验证包版本
library(caret)
packageVersion("caret")  # 应该显示 6.0.47

######题目1######
# 运行题目代码
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
rf_model <- train(y ~ ., data = vowel.train, method = "rf", prox = TRUE)
gbm_model <- train(y ~ ., data = vowel.train, method = "gbm", verbose = FALSE)

pred1 <- predict(rf_model, vowel.test)
pred2 <- predict(gbm_model, vowel.test)

acc_rf <- mean(pred1 == vowel.test$y)
acc_gbm <- mean(pred2 == vowel.test$y)
agreement_accuracy <- mean(pred1 == pred2)

cat("随机森林 (RF) 准确率:", acc_rf, "\n")
cat("GBM 准确率:", acc_gbm, "\n")
cat("Agreement Accuracy (模型一致率):", agreement_accuracy, "\n")

######题目2######
library(caret)
library(gbm)
library(AppliedPredictiveModeling)

set.seed(3433)
data(AlzheimerDisease)

# 提示中的代码
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]

# 题目要求：设置种子 62433
set.seed(62433)

# 1. 训练三个模型
rf <- train(diagnosis ~ ., data = training, method = "rf")
gbm <- train(diagnosis ~ ., data = training, method = "gbm", verbose = FALSE)
lda <- train(diagnosis ~ ., data = training, method = "lda")

# 2. 在测试集上预测
pred_rf <- predict(rf, testing)
pred_gbm <- predict(gbm, testing)
pred_lda <- predict(lda, testing)

# 3. Stacking：将三个模型的预测堆叠，用 rf 作为元模型
# 注意：元模型的训练数据应该来自训练集的预测
stack_train <- data.frame(
    rf = predict(rf, training),
    gbm = predict(gbm, training),
    lda = predict(lda, training),
    diagnosis = training$diagnosis
)

stack_test <- data.frame(
    rf = pred_rf,
    gbm = pred_gbm,
    lda = pred_lda
)

# 用随机森林作为元模型
set.seed(62433)  # 保持一致性
meta_model <- train(diagnosis ~ ., data = stack_train, method = "rf")

# 4. Stacking 的最终预测
pred_stack <- predict(meta_model, stack_test)

# 5. 计算准确率
acc_rf <- mean(pred_rf == testing$diagnosis)
acc_gbm <- mean(pred_gbm == testing$diagnosis)
acc_lda <- mean(pred_lda == testing$diagnosis)
acc_stack <- mean(pred_stack == testing$diagnosis)

# 6. 输出结果
cat("========== 测试集准确率 ==========\n")
cat(sprintf("随机森林 (RF):      %.4f (%.2f%%)\n", acc_rf, acc_rf * 100))
cat(sprintf("GBM:                %.4f (%.2f%%)\n", acc_gbm, acc_gbm * 100))
cat(sprintf("LDA:                %.4f (%.2f%%)\n", acc_lda, acc_lda * 100))
cat(sprintf("Stacking (RF meta): %.4f (%.2f%%)\n", acc_stack, acc_stack * 100))
cat("===================================\n")

# 7. 判断 Stacking 是否更好
if(acc_stack > acc_rf && acc_stack > acc_gbm && acc_stack > acc_lda) {
    cat("\n结论: Stacking 模型比所有单个模型都好！\n")
} else if(acc_stack > acc_rf || acc_stack > acc_gbm || acc_stack > acc_lda) {
    cat("\n结论: Stacking 模型比部分单个模型好\n")
} else {
    cat("\n结论: Stacking 模型没有更好\n")
}



######题目3 ######
library(AppliedPredictiveModeling)
library(caret)
library(elasticnet)

data(concrete)

set.seed(3523)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain, ]

# 准备数据（enet 需要 x 矩阵和 y 向量）
x <- as.matrix(training[, -which(names(training) == "CompressiveStrength")])
y <- training$CompressiveStrength

set.seed(233)
# 拟合 Lasso 模型
fit <- enet(x, y, lambda = 0)

# 查看系数路径
plot(fit, xvar = "step", use.color = TRUE)

# 查看变量进入/退出的顺序
print(fit)

######题目4######
fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
download.file(fileUrl,destfile="E:/coursera_temporary/gaData.csv",method="curl")


library(lubridate)
library(forecast)

# 读取数据
dat = read.csv("E:/coursera_temporary/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

# 拟合模型
fit <- bats(tstrain)

# 预测测试集对应的时间点
n_forecast <- nrow(testing)
forecast_result <- forecast(fit, h = n_forecast, level = 95)

# 提取预测区间的下限和上限
lower <- forecast_result$lower[, 1]  # 95%下限
upper <- forecast_result$upper[, 1]  # 95%上限

# 实际值
actual <- testing$visitsTumblr

# 判断每个点是否在区间内
within_interval <- (actual >= lower) & (actual <= upper)

# 计算落在区间内的点数
points_within <- sum(within_interval)
total_points <- length(actual)

# 输出结果
cat("落在95%预测区间内的点数:", points_within, "/", total_points, "\n")
cat("覆盖率:", round(points_within / total_points * 100, 2), "%\n")

###### 题目5 ######
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain, ]
testing = concrete[-inTrain, ]

set.seed(325)
library(e1071)
library(caret)

# 拟合SVM模型（默认设置）
svm_model <- svm(CompressiveStrength ~ ., data = training)

# 预测
predictions <- predict(svm_model, testing)

# 计算RMSE
rmse_value <- RMSE(predictions, testing$CompressiveStrength)
print(rmse_value)



# 完全可复现的代码
library(AppliedPredictiveModeling)
library(caret)
library(e1071)

data(concrete)

# 正确的数据划分
set.seed(3523)
inTrain <- createDataPartition(concrete$CompressiveStrength, p = 3/4, list = FALSE)
training <- concrete[inTrain, ]
testing <- concrete[-inTrain, ]

# 检查数据量
nrow(training)  # 应该是 773
nrow(testing)   # 应该是 257

# 训练SVM
set.seed(325)
svm_model <- svm(CompressiveStrength ~ ., data = training)

# 预测
predictions <- predict(svm_model, testing)

# 计算RMSE
rmse <- sqrt(mean((predictions - testing$CompressiveStrength)^2))
print(rmse)

# 或者使用caret
rmse_caret <- RMSE(predictions, testing$CompressiveStrength)
print(rmse_caret)


