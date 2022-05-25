x1 = c(3,2,1,0,-1,2,1)
x2 = c(3,0,1,1,0,1,0)
y = c('A', 'A', 'A', 'B', 'B', 'B', 'B')

df = data.frame(x1, x2, y)

euc_dist <- function(new_x1, new_x2) {
  dist_list = c()
  for(row in 1:nrow(df)){
    dist_list <- append(dist_list, (sqrt((new_x1 - df[row, 1])^2 + (new_x2 - df[row, 2])^2)))
  }
  df.euc = data.frame(dist_list, df$y)
  print(df.euc$dist_list)
  print(df.euc[order(df.euc$dist_list), ])
  return(df.euc)
}

euc_1 = euc_dist(1,2)

KNN <- function(K) {
  return(as.data.frame(table(euc_1$df.y[1:K])))
}

class_count = KNN(4)
print(class_count)


covMatG = matrix(ncol=2, c(0.1502, 0.0055, 0.0055, 0.1998))
meanG = c(214.97, 141.52)

covMatF = matrix(ncol=2, c(0.1240, 0.0116, 0.0116, 0.3112))
meanF = c(214.82, 139.45)

n.G = 500
n.F = 500
n = n.G + n.F
K = 2

covMat.pool = (n.G - 1)/(n - K) * covMatG + (n.F - 1)/(n - K) * covMatF
print(covMat.pool)

x_0 = c(214.0, 140.4)

classify_G = t(x_0) %*% solve(covMat.pool) %*% (meanG - meanF) - (1/2) * t(meanG) %*% solve(covMat.pool) %*% meanF
print(classify_G)
# Fake note

discriminant_QDA <- function(x, covMat, mean, pre_prob) {
  val = (-1/2)%*%t(x)%*%solve(covMat)%*%x + t(x)%*%solve(covMat)%*%mean - (1/2)*t(mean)%*%solve(covMat)%*%mean - (1/2)*log(det(covMat)) + log(pre_prob)
  return(val)
}

discriminant_G = discriminant_QDA(x_0, covMatG, meanG, n.G/n)
print(discriminant_G)

discriminant_F = discriminant_QDA(x_0, covMatF, meanF, n.F/n)
print(discriminant_F)


# PROBLEM 6

library(ISLR)
df = data("Weekly")

library(ggplot2)
library(GGally)

summary(Weekly)
ggpairs(Weekly, aes(color=Direction))
?Weekly

glm.weekly = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                 data = Weekly, family="binomial")
summary(glm.weekly)

glm.probs_Weekly = predict(glm.weekly, type="response")
glm.preds_Weekly = ifelse(glm.probs_Weekly > 0.5, "Up", "Down")
table(glm.preds_Weekly, Weekly$Direction)

Weekly_trainID = (Weekly$Year < 2009)
Weekly_train = Weekly[Weekly_trainID,]
Weekly_test = Weekly[!Weekly_trainID,]

glm2.weekly = glm(Direction ~ Lag2, data = Weekly_train, family="binomial")
glm2.probs_Weekly = predict(glm2.weekly, newdata = Weekly_test, type="response")
glm2.preds_Weekly = ifelse(glm2.probs_Weekly > 0.5, "Up", "Down")
table(glm2.preds_Weekly, Weekly_test$Direction)

library(MASS)

lda.weekly = lda(Direction ~Lag2, data = Weekly_train)
lda.preds_Weekly = predict(lda.weekly, newdata = Weekly_test)$class
lda.Weekly_prob = predict(lda.weekly, newdata=Weekly_test)$posterior
summary(lda.preds_Weekly)

table(lda.preds_Weekly, Weekly_test$Direction)

qda.weekly = qda(Direction ~Lag2, data = Weekly_train)
qda.preds_Weekly = predict(qda.weekly, newdata = Weekly_test)$class
qda.Weekly_prob = predict(qda.weekly, newdata=Weekly_test)$posterior
summary(qda.preds_Weekly)

table(qda.preds_Weekly, Weekly_test$Direction)

library(class)
knn.train = as.matrix(Weekly_train$Lag2)
knn.test = as.matrix(Weekly_test$Lag2)
set.seed(123)
knn.weekly = knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction, k=11, prob=T)
table(knn.weekly, Weekly_test$Direction)

K=30
knn.error = rep(NA,K)
set.seed(234)
for(k in 1:K){
  knn.pred = knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction, k=k)
  knn.error[k] = mean(knn.pred != Weekly_test$Direction)
}
knn.error.df = data.frame(k=1:K, error = knn.error)
ggplot(knn.error.df, aes(x=k, y=error))+geom_point(col="blue")+geom_line(linetype="dotted")

#get the probabilities for the classified class
knn.probs_weekly = attributes(knn.weekly)$prob
# since we want the probability for Up, we need to take 1-p for the elements that gives probability for Down
down= which(knn.weekly == "Down")
knn.probs_weekly[down] = 1-knn.probs_weekly[down]

#install.packages("plotROC")
#install.packages("pROC")
library(pROC)
library(plotROC)
yourRoc = roc(response = Weekly_test$Direction, predictor = yourModelsPredictedProb, direction = "<")
#you can use this function for all your methods and plot them using plot(yourRoc)
#or use ggplot2
dat = data.frame(Direction = Weekly_test$Direction, glm = glm2.probs_Weekly,
                 lda = lda.Weekly_prob[,2], qda = qda.Weekly_prob[,2], knn = knn.probs_weekly)
dat_long = melt_roc(dat, "Direction", c("glm", "lda", "qda", "knn"))
ggplot(dat_long, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = F) +
  xlab("1-Specificity") + ylab("Sensitivity")
#glm is very similar to lda, so the roc-curve for glm is not shown.
