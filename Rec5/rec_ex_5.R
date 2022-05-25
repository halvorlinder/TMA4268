n_samples = c(10, 1000, 100000)
k = 1000

for(i in 1:3){
  avg = 0
  for(k in 1:k){
    x = sample(1:n_samples[i], n_samples[i], replace = T)
    num = length(which(x == 1))
    if(num == 0){
      avg = avg + 1
    }
  }
  avg = avg / k
  print(1-avg)
}

curve(1-(1-1/x)^x, 1, 100, ylim=c(0.6, 1))
abline(h=1-1/exp(1), lty=2)

library(car)
library(boot)
SLID = na.omit(SLID)
n = dim(SLID)[1]
SLID.lm = lm(wages ~., data=SLID)
summary(SLID.lm)$coeff["age", ]

boot.fn = function(data, index) {
  return(coef(lm(wages~., data=data, subset=index)))
}

beta_hat = c()
B=1000
for(i in 1:B){
  index = sample(nrow(SLID), nrow(SLID), replace = T)
  beta_hat[i] = boot.fn(SLID, index)["age"]
}

sd(beta_hat)
quantile(beta_hat, c(0.025, 0.975))
confint.SLID = confint(SLID.lm)

