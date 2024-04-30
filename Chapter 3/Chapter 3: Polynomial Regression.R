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