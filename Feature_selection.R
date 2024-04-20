
#this dataset is from Multiple linear Regression
df_train_adjusted


#load library for the bestset selection
library(leaps)


#perform subset variable selection
bestsub.fit <- regsubsets(price ~ ., data = df_train_adjusted)

summary(bestsub.fit)

bestsub_sum <- summary(bestsub.fit)

par(mfrow = c(2,2))
plot(bestsub_sum$rss, xlab = "Number of predictors")
plot(bestsub_sum$adjr2, xlab = "Number of predictors")
which.max(bestsub_sum$adjr2)


plot(bestsub.fit, scale = "r", family = "mono", main = "R-squared plot")
plot(bestsub.fit, scale = "adjr2"
     ,  family = "mono", main = "Adjusted R-squared plot")
plot(bestsub.fit, scale = "Cp",  family = "mono", main = "Cp plot")
plot(bestsub.fit, scale = "BIC",  family = "mono", main = "BIC plot")


#Stepwise selection
#forward stepwise selection
forward_stepwise <- regsubsets(price ~ ., data = df_train_adjusted, method = "forward")

summary(forward_stepwise)

forward_stepwise_sum <- summary(forward_stepwise)

#forward stepwise selection
backward_stepwise <- regsubsets(price ~ ., data = df_train_adjusted, method = "backward")

backward_stepwise_sum <- summary(backward_stepwise)
backward_stepwise_sum

coef(backward_stepwise,4)
coef(backward_stepwise,2)



#Ridge Regression
library(glmnet)
#install.packages("glmnet")

x <- model.matrix(price ~ ., df_train_adjusted)[,-7]
y <- df_train_adjusted$price

#performing Ridge
grid <- 10^seq(10, -2, length = 100)

#Lasso Regression
lasso.fit <- glmnet(x,y
  , alpha = 1, lambda = grid)

par(mfrow = c(1,1))
plot(lasso.fit)
text(lasso.fit, pretty = 0)


dim(coef(lasso.fit))




lasso.fit$lambda[90]
coef(lasso.fit)[, 90]
