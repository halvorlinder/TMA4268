library(ISLR)
Auto = subset(Auto, select = -name)
# Auto$origin = factor(Auto$origin)
summary(Auto)
Auto$origin = factor(Auto$origin)
library(GGally)
ggpairs(Auto)
cor(Auto[,-8])
r.mpg = lm(mpg ~ cylinders + displacement + horsepower + weight +  acceleration + year + origin, data = Auto)
summary(r.mpg)
anova(r.mpg)
library(ggfortify)
autoplot(r.mpg, smooth.colour = NA)

set.seed(2332)
n = 100
par(mfrow = c(2, 3))
for (i in 1:6) {
  sim = rnorm(n)
  qqnorm(sim, pch = 1, frame = FALSE)
  qqline(sim, col = "blue", lwd = 1)
}
r.mpg2 = lm(mpg ~ I(displacement^2) + I(weight^2) + year*origin, data = Auto)
summary(r.mpg2)
