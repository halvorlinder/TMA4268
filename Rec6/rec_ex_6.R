library(ISLR)
library(GGally)

data("Credit")
Credit = subset(Credit, select = -c(ID))
Credit = na.omit(Credit)
Credit2 = subset(Credit, select = c(Balance, Age, Cards, Education, Income, Limit, Rating))

plot <- ggpairs(Credit2)
plot

split1<- sample(c(rep(0, 0.8 * nrow(Credit)), rep(1, 0.2 * nrow(Credit))))

credit_train = Credit[split1 == 0, ]
credit_test = Credit[split1 == 1, ]

library(leaps)
best_sub = regsubsets(Balance ~ ., credit_train, nvmax=11)
reg_summary = summary(best_sub)

par(mfrow = c(2,2))
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(reg_summary$adjr2) # 11

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)


# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg_summary$cp) # 10
points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg_summary$bic) # 6
points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k = 10
set.seed(1)
folds = sample(1:k, nrow(credit_train), replace=TRUE)
cv.errors = matrix(NA, k, 11, dimnames = list(NULL, paste(1:11)))

for (j in 1:k) {
  best_subset_method = regsubsets(Balance~., data=credit_train[folds != j,], nvmax=11)
  for(i in 1:11){
    pred = predict(best_subset_method, credit_train[folds == j, ], id=i)
    cv.errors[j, i] = mean((credit_train$Balance[folds == j] - pred)^2)
  }
}

mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
cv_min = which.min(mean.cv.errors)
points(cv_min, mean.cv.errors[cv_min], col ="red", cex = 2, pch = 20)

variables <- names(coef(best_subset_method,id=4))
variables <- variables[!variables %in% "(Intercept)"]
bsm_formula <- as.formula(best_subset_method$call[[2]])
bsm_design_matrix <- model.matrix(bsm_formula,credit_data_training)[, variables]
bsm_data_train <- data.frame(Balance = credit_data_training$Balance, bsm_design_matrix)




library(glmnet)
x_train <- model.matrix(Balance~.,credit_train)[,-1]
y_train <- credit_train$Balance
x_test <- model.matrix(Balance~.,credit_train)[,-1]
y_test <- credit_train$Balance
ridge_mod <- glmnet(x_train,y_train,alpha=0)
set.seed(1)
cv.out=cv.glmnet(x_train, y_train,alpha=0)
plot(cv.out)
best_lambda_ridge <- cv.out$lambda.min
best_lambda_ridge
ridge_predictions = predict(ridge_mod,s=best_lambda_ridge,newx=x_test)
ridge_square_errors <- as.numeric((ridge_predictions-y_test)^2)


x <- model.matrix(Balance~.,Credit)[,-1]
credit_pca <- prcomp(x, center = TRUE, scale. = TRUE) 
print(credit_pca)
plot(credit_pca, type = "l")
summary(credit_pca)


library(pls)
set.seed(1)
pcr_model <- pcr(Balance~., data=credit_train,scale=TRUE, validation="CV")
validationplot(pcr_model,val.type="MSEP")

pcr_predictions = predict(pcr_model,credit_test,ncomp=10)
pcr_square_errors <- as.numeric((pcr_predictions-credit_test$Balance)^2)
mean(pcr_square_errors)

plsr_model <- plsr(Balance~., data=credit_train,scale=TRUE, validation="CV")
validationplot(plsr_model,val.type="MSEP")
plsr_predictions = predict(plsr_model,credit_test,ncomp=3)
plsr_square_errors <- as.numeric((plsr_predictions-credit_test$Balance)^2)
mean(plsr_square_errors)
