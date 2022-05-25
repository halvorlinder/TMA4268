beta0 = 1
beta1 = 3
true_beta = c(beta0, beta1) # vector of model coefficients
true_sd = 1 # choosing true sd
X = runif(100, 0, 1) # simulate the predictor variable X
Xmat = model.matrix(~X, data = data.frame(X)) # create design matrix
ci_int = ci_x = pi_y = 0 # Counts how many times the true value is within the confidence interval
nsim = 1000
for (i in 1:nsim) {
#i = 1
  y = rnorm(n = 100, mean = Xmat %*% true_beta, sd = rep(true_sd, 100))
  mod = lm(y ~ x, data = data.frame(y = y, x = X))
  ci = confint(mod)
  pi = predict(mod, data.frame(x=i), interval = "predict")
  y_0 = rnorm(1, mean=0, sd=1) + beta0 + beta1*i
  ci_int[i] = ifelse(ci[1,1]<beta0 && beta0<ci[1,2], 1, 0) # if true value of beta0 is within the CI then 1 else 0
  ci_x[i] = ifelse(ci[2,1]<beta1 && beta1<ci[2,2], 1, 0) # if true value of beta_1 is within the CI then 1 else 0
  pi_y[i] = ifelse(pi[1,2]<y_0 && y_0<pi[1,3], 1, 0) # if true value of beta_1 is within the CI then 1 else 0
}
c(mean(ci_int), mean(ci_x), mean(pi_y))
