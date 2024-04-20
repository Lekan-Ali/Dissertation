############# model building ####################

#scale price to base10
df3 %>% 
  mutate(price = log(price,10)) -> df4



#split dataset into train and test set
set.seed(1)
train <- sample(1:nrow(df4), dim(df4)[1]*0.8)

#train set
df_train_set <- df4[train,]

#test set
df_test_set <- df4[-train,]


#build simple linear model
lm.fit <- lm(price ~ engine, data = df_train_set)

#model summary
summary(lm.fit)


#diagnostic plots
par(mfrow = c(2,2))
plot(lm.fit)



#build a polynomial model
lm.fit.poly <- lm(price ~ engine + I(engine^2), data = df_train_set)

#polynomial summary
summary(lm.fit.poly)


#Step 3
#ANOVA to compare both models
anova_fit <- anova(lm.fit, lm.fit.poly)

#summary of anova
summary(anova_fit)


#Step 4 plot polynomial and regression lines to data
par(mfrow = c(1,2))

#plotting regression line
plot(df_train_set$engine, df_train_set$price
     , pch = 1, cex = 0.5
     , xlab = "engine", ylab = "price", main = "Least square regression fit", lty = 3, family = "mono", col = alpha(1,0.4))
abline(lm.fit, col = "blue", lwd = 4
       , lty = 2)

#Plotting polynomial line
plot(df_train_set$engine, df_train_set$price
     , pch = 1, cex = 0.5
     , xlab = "engine", ylab = "price"
     , main = "polynomial regression fit", lty = 2, family = "mono"
     , col = alpha(1,0.4))

pred <- predict(lm.fit.poly)
ix <- sort(df_train_set$engine, index.return=T)$ix
lines(df_train_set$engine[ix], pred[ix], col = "red", lwd = 4
      , lty =2)

