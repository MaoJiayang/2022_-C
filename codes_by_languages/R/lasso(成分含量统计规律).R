library(foreign)
library(glmnet)
class_weathering_chemical <- read.csv("./class_weathering_chemical.csv")
#####完全多变量回归#####
x <- as.matrix(class_weathering_chemical[,c(-1,-16:-18)])#定义回归自变量因变量
y <- as.matrix(class_weathering_chemical[,c(16:18)])
fit <- glmnet(x,y,"mgaussian",nlambda = 1000,alpha=1)#为了查看变量影响因素而进行的回归
plot(fit, xvar="lambda", label=TRUE)
lasso_fit <- cv.glmnet(x,y,family="mgaussian",alpha=1,type.measure = "mse",nlambda=1000)#交叉验证,最佳lambda
plot(lasso_fit)
lasso_best <- glmnet(x,y,family="mgaussian",alpha = 1,lambda = lasso_fit$lambda.min)#用λmin建立预测模型

lasso_best$a0#回归截距

k <- as.matrix(lasso_best$beta$rich_k)
k <- cbind(k,as.numeric(lasso_best$beta$PbBa))
k <- cbind(k,as.numeric(lasso_best$beta$weathering))
k <- as.data.frame(k)
names(k) <- c("高钾","铅钡","风化程度")#k为回归系数矩阵
print(k)


#####高钾玻璃的风化情况#####
richK_weathering_chemical <- read.csv("./richK_weathering_chemical.csv")
x <- as.matrix(richK_weathering_chemical[,c(-1,-16)])#定义回归自变量因变量
y <- as.matrix(richK_weathering_chemical[,16])#风化程度
fit <- glmnet(x,y,"gaussian",nlambda = 1000,alpha=0)#为了查看变量影响因素而进行的回归
plot(fit, xvar="lambda", label=TRUE)
lasso_fit <- cv.glmnet(x,y,family="gaussian",alpha=0,type.measure = "mse",nlambda=1000)#交叉验证,最佳lambda
plot(lasso_fit)
lasso_best <- glmnet(x,y,family="gaussian",alpha = 0,lambda = lasso_fit$lambda.min)#用λmin建立预测模型

lasso_best$a0#回归截距

k <- as.data.frame(as.matrix(lasso_best$beta))
names(k) <- "风化程度(0-2)"
print(k)

#####铅钡玻璃的风化情况#####
PbBa_weathering_chemical <- read.csv("./PbBa_weathering_chemical.csv")
x <- as.matrix(PbBa_weathering_chemical[,c(-1,-16)])#定义回归自变量因变量
y <- as.matrix(PbBa_weathering_chemical[,16])#风化程度
fit <- glmnet(x,y,"gaussian",nlambda = 1000,alpha=0)#为了查看变量影响因素而进行的回归
plot(fit, xvar="lambda", label=TRUE)
lasso_fit <- cv.glmnet(x,y,family="gaussian",alpha=0,type.measure = "mse",nlambda=1000)#交叉验证,最佳lambda
plot(lasso_fit)
lasso_best <- glmnet(x,y,family="gaussian",alpha = 0,lambda = lasso_fit$lambda.min)#用λmin建立预测模型

lasso_best$a0#回归截距

k <- as.data.frame(as.matrix(lasso_best$beta))
names(k) <- "风化程度(0-2)"
print(k)