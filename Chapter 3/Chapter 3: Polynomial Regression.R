#Step 1: Load the dataset, Credit dataset in the ISLR2 package.
library(ISLR2) #this is needed to load the Credit data.


#step 2: perform quadratic regression
#quadratic polynomial model
lm.quadratic <- lm(Balance ~ Rating + I(Rating^2), data = Credit)

#Model summary of quadratic polynomial regression
# see Table 3.6
summary(lm.quadratic)



#step 3: perform cubic regression
lm.cubic <- lm(Balance ~ poly(Rating,3), data = Credit)

#Model summary of cubic polynomial regression
# see Table 3.7
summary(lm.cubic)



#Step 4: comparing the simple linear model to quadratic polynomial model to verify the better model
#see Table 3.8 in Dissertation
anova(lm.simple, lm.quadratic)



#Step 5: Cubic and polynomial fit applied to the Rating and Balance scatterplot
# see Figure 3.5 in dissertation
#Plotting polynomial line
plot(Credit$Rating, Credit$Balance
     , pch = 1, cex = 0.5
     , xlab = "Rating", ylab = "Balance"
     , lty = 2, family = "mono"
     , col = alpha(1,0.6))

# quadratic lines in red
pred <- predict(lm.quadratic)
ix <- sort(Credit$Rating, index.return=T)$ix
lines(Credit$Rating[ix], pred[ix], col = "red", lwd = 2
      , lty =1)

#linear regression line in blue
abline(lm.simple, col = "blue", lwd = 2
       , lty = 1)
