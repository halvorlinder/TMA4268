library(ISLR)
library(GGally)
data(Auto)
View(Auto)

range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
sd(Auto$mpg)
sd(Auto$cylinders)
sd(Auto$displacement)
sd(Auto$horsepower)
sd(Auto$weight)
sd(Auto$acceleration)
summary(Auto)

ReducedAuto = Auto[-(10:85),]
summary(ReducedAuto)
apply(ReducedAuto[,1:7], 2, sd)

ggpairs(Auto[,!(names(Auto)%in%c("name", "origin", "cylinders"))])

ggplot(Auto, aes(as.factor(cylinders), mpg))+
  geom_boxplot()
  
ggplot(Auto, aes(as.factor(origin), mpg))+
  geom_boxplot()
quant = c(1,3,4,5,6,7)
cov(Auto[, quant])






