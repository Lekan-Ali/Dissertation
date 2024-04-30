#Step 1: Load the dataset, Credit dataset in the ISLR2 package.
library(ISLR2) #this is needed to load the Credit data.

# Step 2: Perform multiple linear regression
lm.multi <- lm(Balance ~ Rating + Income + Own + Limit + Age + Student + Married, data = Credit)

#Model summary
# see Table 3.4 and 3.5 in Dissertation
summary(lm.multi)
