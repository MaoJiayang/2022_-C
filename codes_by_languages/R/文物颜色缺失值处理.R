library(randomForest)
library("pROC")
relic_classify <- read.csv("C:/Users/Nucleon/Desktop/笔记文档/项目三：数模国赛C题数据/codes/R/raw_datasets/relic_classify.csv")

data_one_hot <- model.matrix(~relic_classify[,2]-1,relic_classify)#行号不算在内,随后onehot
for (i in c(3:ncol(relic_classify))) {
  data_one_hot <- cbind(data_one_hot,model.matrix(~relic_classify[,i]-1,relic_classify))
}

train <- data_one_hot[c(-19,-40,-48,-58),-6]#去掉缺失值的那一列onehot编码
pre <- data_one_hot[c(19,40,48,58),-6]

train_x <- train[,-c(6:13)]#定义训练集x与y
train_y <- train[,c(6:13)]
pre_x <- pre[,-c(6:13)]#定义需要补全的那几个数据的x
write.table(train_x,"train_x.csv",row.names=FALSE,col.names=TRUE,sep=",")
write.table(train_y,"train_y.csv",row.names=FALSE,col.names=TRUE,sep=",")
write.table(pre_x,"pre_x.csv",row.names=FALSE,col.names=TRUE,sep=",")