library(foreign)
library(glmnet)
##########高钾玻璃的风化前预测##########
#####数据准备#####
rich_K_weathering_chemical <- read.csv("./predict_b4_weathering.csv")
rich_K_train <- data.frame()#划分训练集与测试集.有风化的做测试集,并把风化位置为0后作为预测集
for (i in c(1:nrow(rich_K_weathering_chemical))) {#从大集合中取出高钾玻璃作为训练集
  if(rich_K_weathering_chemical[i,"rich_k"] == 1){
    rich_K_train <- rbind(rich_K_train,rich_K_weathering_chemical[i,])
  }
}
rich_K_predict <- data.frame()
for (i in c(1:nrow(rich_K_train))) {#从训练集中取出有风化的作为测试集,并把风化程度置为0
  if(rich_K_train[i,"weathering"] != 0){
    rich_K_predict <- rbind(rich_K_predict,rich_K_train[i,])
  }
}
rich_K_predict[,"weathering"] <- 0#并把风化程度置为0
train_y <- as.matrix(rich_K_train[,2:15])#定义回归训练自变量因变量
train_x <- as.matrix(rich_K_train[,16:30])#自变量
#####模型拟合#####
fit <- glmnet(train_x,train_y,"mgaussian",nlambda = 1000,alpha=0)#为了查看变量影响因素而进行的回归
plot(fit, xvar="lambda", label=TRUE)
lasso_fit <- cv.glmnet(train_x,train_y,family="mgaussian",alpha=0,type.measure = "mse",nlambda=1000)#交叉验证,最佳lambda
plot(lasso_fit)
lasso_best <- glmnet(train_x,train_y,family="mgaussian",alpha = 0,lambda = lasso_fit$lambda.min)#用λmin建立预测模型
#####模型预测#####
#定义回归预测自变量
predict_x <- as.matrix(rich_K_predict[,16:30])#自变量
predict_result <- as.data.frame(predict(lasso_best,predict_x))#进行预测
predict_result <- cbind(rich_K_predict[,1],predict_result)#优化预测表格格式
names(predict_result) <- names(rich_K_predict[,1:15])
#print(predict_result)



##########铅钡玻璃的风化前预测##########
#####数据准备#####
PbBa_weathering_chemical <- read.csv("./predict_b4_weathering.csv")
PbBa_train <- data.frame()#划分训练集与测试集.有风化的做测试集,并把风化位置为0后作为预测集
for (i in c(1:nrow(PbBa_weathering_chemical))) {#从大集合中取出铅钡玻璃作为训练集
  if(PbBa_weathering_chemical[i,"PbBa"] == 1){
    PbBa_train <- rbind(PbBa_train,PbBa_weathering_chemical[i,])
  }
}
PbBa_predict <- data.frame()
for (i in c(1:nrow(PbBa_train))) {#从训练集中取出有风化的作为测试集,并把风化程度置为0
  if(PbBa_train[i,"weathering"] != 0){
    PbBa_predict <- rbind(PbBa_predict,PbBa_train[i,])
  }
}
PbBa_predict[,"weathering"] <- 0#并把风化程度置为0
train_y <- as.matrix(PbBa_train[,2:15])#定义回归训练自变量因变量
train_x <- as.matrix(PbBa_train[,16:30])#自变量
#####模型拟合#####
fit <- glmnet(train_x,train_y,"mgaussian",nlambda = 1000,alpha=0)#为了查看变量影响因素而进行的回归
plot(fit, xvar="lambda", label=TRUE)
lasso_fit <- cv.glmnet(train_x,train_y,family="mgaussian",alpha=0,type.measure = "mse",nlambda=1000)#交叉验证,最佳lambda
plot(lasso_fit)
lasso_best <- glmnet(train_x,train_y,family="mgaussian",alpha = 0,lambda = lasso_fit$lambda.min)#用λmin建立预测模型
#####模型预测#####
#定义回归预测自变量
predict_x <- as.matrix(PbBa_predict[,16:30])#自变量
predict_result <- as.data.frame(predict(lasso_best,predict_x))#进行预测
predict_result <- cbind(PbBa_predict[,1],predict_result)#优化预测表格格式
names(predict_result) <- names(PbBa_predict[,1:15])
#print(predict_result)