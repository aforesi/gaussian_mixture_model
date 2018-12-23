library(mclust)
setwd("C:/Users/Andrew Foresi/Documents/A Masters Year 1/CSE 780/Assignment 5/wisconsin breast cancer")
X <- read.csv("breast-cancer-wisconsin-assgn5.csv", header = FALSE)
head(X)
boxplot(scale(X[,c(2:10)]))
tumors <- as.matrix(scale(X[,c(1:10)]))
head(tumors$Chromatin)
pairs(tumors)


data_delete <- rep(0,699)
k<-1
for(i in 1:699){
  data_delete[k]<-i;
  k<-k+1;
}
data_delete <- sample(data_delete, 174)
dataMclustDA <- MclustDA(X[-data_delete,c(2:10)], X[-data_delete,11])
#summary(dataMclustDA, parameters = TRUE)
summary(dataMclustDA, newdata = X[data_delete,c(2:10)], newclass = X[data_delete,11], parameters = TRUE)
head(dataMclustDA$call)
?adjustedRandIndex()

a <- rep(1:3, 3)
a
b <- rep(c("A", "B", "C"), 3)
b
adjustedRandIndex(a, b)



############ CLASSIFICATION TREE #################
head(X)
set.seed(3)
formula("V11~ V2 +V3 +V4 +V5 +V6 +V7 +V8 +V9 +V10")
# ecoli$class <- factor(ecoli$class)
bag.tumor=randomForest(V11~ V2 +V3 +V4 +V5 +V6 +V7 +V8 +V9 +V10, data = X, subset=train,mtry=5,importance=TRUE,type="class")
bag.tumor
train = sample(1: nrow(X), nrow(X)/2)
tumor.test=X[-train,c(11)]

length(tumor.test)
tumor.pred=predict(bag.tumor,X[-train,],type="class")
tab<-table(tumor.test,tumor.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand

importance(bag.tumor)
varImpPlot(bag.tumor)
