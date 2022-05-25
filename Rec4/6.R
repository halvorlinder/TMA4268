library(ISLR)
data("Weekly")
library(GGally)



errorrate = function(table){
  return(1-(table[1,1]+table[2,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2]))
}




ggpairs(Weekly)
#?Weekly


logmodel = glm(Direction ~ . - Year - Today - Direction, data=Weekly, family="binomial")
summary(logmodel)

glm.probs_Weekly = predict(logmodel, type="response")
glm.preds_Weekly = ifelse(glm.probs_Weekly > 0.5, "Up", "Down")
conf.glm = table(glm.preds_Weekly, Weekly$Direction)
conf.glm
errorrate(conf.glm)



Weekly_trainID = (Weekly$Year < 2009)
Weekly_train = Weekly[Weekly_trainID,]
Weekly_test = Weekly[!Weekly_trainID,]



logmodel2 = glm(Direction ~ Lag2, data=Weekly_train, family="binomial")
summary(logmodel2)

glm.probs_Weekly = predict(logmodel2, type="response", newdata = Weekly_test)
glm.preds_Weekly = ifelse(glm.probs_Weekly > 0.5, "Up", "Down")
conf.glm2 = table(glm.preds_Weekly, Weekly_test$Direction)
conf.glm2
errorrate(conf.glm2)



library(MASS)
ldamodel = lda(Direction ~ Lag2, data=Weekly_train)

lda.pred_Weekly = predict(ldamodel, newdata = Weekly_test)$class
lda.prob_Weekly = predict(ldamodel, newdata = Weekly_test)$posterior
conf.lda = table(lda.pred_Weekly, Weekly_test$Direction)
conf.lda
errorrate(conf.lda)



qdamodel = qda(Direction ~ Lag2, data=Weekly_train)
qda.pred_Weekly = predict(qdamodel, newdata = Weekly_test)$class
qda.prob_Weekly = predict(qdamodel, newdata = Weekly_test)$posterior
conf.qda = table(qda.pred_Weekly, Weekly_test$Direction)
conf.qda
errorrate(conf.qda)



library(class)
knn.train = as.matrix(Weekly_train$Lag2)
knn.test = as.matrix(Weekly_test$Lag2)
set.seed(123)
knnmodel = knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction, k=1, prob=T)
conf.knn1 = table(knnmodel, Weekly_test$Direction)
conf.knn1
errorrate(conf.knn1)

K=30
knn.error = rep(NA,K)
set.seed(234)
for(k in 1:K){
  knn.pred = knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction, k=k)
  knn.error[k] = mean(knn.pred != Weekly_test$Direction)
}
knn.error.df = data.frame(k=1:K, error = knn.error)
ggplot(knn.error.df, aes(x=k, y=error))+geom_point(col="blue")+geom_line(linetype="dotted")

knn12model = knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction, k=12, prob=T)
conf.knn12 = table(knnmodel, Weekly_test$Direction)
conf.knn12
errorrate(conf.knn1)

KNNProbs = attributes(knn12model)$prob
down= which(knn12model == "Down")
KNNProbs[down] = 1-KNNProbs[down]


library(pROC)
library(plotROC)
roc.glm = roc(response = Weekly_test$Direction, predictor = glm.probs_Weekly, direction = "<")
plot(roc.glm)
auc(roc.glm)

roc.lda = roc(response = Weekly_test$Direction, predictor = lda.prob_Weekly[,2], direction = "<")
plot(roc.lda)
auc(roc.lda)

roc.qda = roc(response = Weekly_test$Direction, predictor = qda.prob_Weekly[,2], direction = "<")
plot(roc.qda)
auc(roc.qda)

roc.knn12 = roc(response = Weekly_test$Direction, predictor = KNNProbs, direction = "<")
plot(roc.lda)
auc(roc.lda)
