# Rec ex 8

# 2a
library(ISLR)
data("Carseats")
set.seed(4268)
n = nrow(Carseats)
split <- sample(c(rep(0, 0.7 * nrow(Carseats)), rep(1, 0.3 * nrow(Carseats))))
Carseats.train = Carseats[split == 0, ]
Carseats.test = Carseats[split == 1, ]

# 2b
library(tree)
carseats.tree = tree(Sales~., data = Carseats.train)
plot(carseats.tree, type="uniform")
text(carseats.tree, cex=0.7)
carseats.pred = predict(carseats.tree, newdata = Carseats.test)
carseats.mse = mean((carseats.pred - Carseats.test$Sales)^2)

# 2c
set.seed(4268)
carseats.tree.huge = tree(Sales~., data = Carseats.train, control = tree.control(nrow(Carseats), mincut = 2, minsize = 4, mindev = 0.001))
cv.tree.huge = cv.tree(carseats.tree.huge)
plot(cv.tree.huge$dev ~  cv.tree.huge$size,type= "b", lwd=2, col="red", xlab="Tree Size", ylab="Deviance", xlim=c(1, 15))
rev_list = rev(cv.tree.huge$dev)
best_size = which.min(rev_list) # 6 is the best tree size according to CV
prune.carseats = prune.tree(carseats.tree.huge, best = best_size)
plot(prune.carseats, type = "uniform")
text(prune.carseats, cex=0.4)
prune.carseats.pred = predict(prune.carseats, newdata = Carseats.test)
prune.carseats.mse = mean((prune.carseats.pred - Carseats.test$Sales)^2)

# 2d
library(randomForest)
dim(Carseats)
bag.Carseats = randomForest(Sales~., data = Carseats.train, mtry = 10, ntree = 500, importance = TRUE)
bag.Carseats.pred = predict(bag.Carseats, newdata = Carseats.test)
bag.Carseats.mse = mean((bag.Carseats.pred - Carseats.test$Sales)^2)


# 2e
rf.Carseats = randomForest(Sales~., data = Carseats.train, mtry = 3, ntree = 500, importance = TRUE)
rf.Carseats.pred = predict(rf.Carseats, newdata = Carseats.test)
rf.Carseats.mse = mean((rf.Carseats.pred - Carseats.test$Sales)^2)

varImpPlot(rf.Carseats,pch=20, main = "Random forest")

# 2f
library(gbm)
?gbm
carseats.boost = gbm(Sales~., Carseats.train, distribution = "gaussian",
                     n.trees = 500, interaction.depth = 4, shrinkage = 0.1)
carseats.boost.pred = predict(carseats.boost, newdata = Carseats.test)
carseats.boost.mse = mean((carseats.boost.pred - Carseats.test$Sales)^2)
