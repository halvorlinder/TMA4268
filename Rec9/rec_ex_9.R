library(e1071)

# Problem 3
set.seed(10111)
x <- matrix(rnorm(40), 20, 2)
y <- rep(c(-1, 1), c(10, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = y + 3, pch = 19, xlab = expression(X[1]), ylab = expression(X[2]))
dat = data.frame(x, y = as.factor(y))
svmfit = svm(y~., data=dat, kernel="linear", cost = 1, scale = TRUE)

make.grid = function(x, n =75){
  # takes as input the data matrix x
  # and number of grid points n in each direction
  # the default value will generate a 75x75 grid
  grange = apply(x,2,range) # range for x1 and x2
  x1 = seq(from = grange[1,1], to = grange[2,1], 
           length.out = n) # sequence from the lowest to the upper value of x1
  x2 = seq(from = grange[1,2], to = grange[2,2], 
           length.out = n) # sequence from the lowest to the upper value of x2
  expand.grid(X1=x1, X2=x2) #create a uniform grid according to x1 and x2 values
}

plot(svmfit, dat, col=c("lightcoral", "lightgreen"))
x = as.matrix(dat[, c("X1", "X2")])
xgrid=make.grid(x, 200)
ygrid=predict(svmfit,xgrid)
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch=10, cex=1)
summary(svmfit)
svmfit$index
points(x[svmfit$index, ], pch=3, cex=2)
points(x,col=(y+3)^2,pch=19)

beta_0 = svmfit$rho
svmfit$coefs
svmfit$index[1]

beta = 0
for(i in 1:10) {
  beta = beta + svmfit$coefs[i, 1] * x[svmfit$index[i], ]
}
library(zeallot)

c(beta_1, beta_2) %<-% beta

# Problem 4

load(url("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda"))
#names(ESL.mixture)
rm(x,y)
attach(ESL.mixture)
plot(x,col=y+1, pch=19, xlab = expression(X[1]), ylab = expression(X[2]))
dat=data.frame(y=factor(y),x)

r.cv <- tune(svm,factor(y)~.,data=dat,kernel="radial",
             ranges=list(cost=c(0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50, 100),
                         gamma=c(0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50, 100)))
summary(r.cv)
bestmod = r.cv$best.model
plot(bestmod, dat, col = c("lightgrey", "darkgray"))


xgrid = make.grid(x)
ygrid=predict(bestmod,xgrid)
plot(xgrid,col=as.numeric(ygrid),pch=20,cex=.2)
points(x,col=y+1,pch=19)
# decision boundary
func=predict(bestmod,xgrid,decision.values=TRUE)
func=attributes(func)$decision
contour(unique(xgrid[,1]),unique(xgrid[,2]),matrix(func,75,75),
        level=0,add=TRUE) #svm boundary



