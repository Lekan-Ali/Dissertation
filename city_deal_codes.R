---------
  library(ISLR2)
library(tidyverse)
library(tree)
library(randomForest)
#install.packages("randomForest")

citydeal <- readxl::read_excel("activity_outcome_sheet_only.xlsx")


citydeal %>% 
  select(-`Row Labels`) -> citydeal

colnames(citydeal)


Employed_any_any_time <- factor(ifelse(citydeal$Employed < 1, "No","Yes"))
Employed_any_any_time
glimpse(Employed_any_any_time)
summary(Employed_any_any_time)


#create new dataframe
citydeal %>% 
  data.frame(Employed_any_any_time) %>% 
  select(-Employed) -> citydeal3


tree.fit <- tree(Employed_any_any_time ~ ., data = citydeal3)

summary(tree.fit)
par(mfrow = c(1,1))
plot(tree.fit, fill = "red")
text(tree.fit, pretty = 0)

#split data set into 2 to test predictions
set.seed(25)

#remove employed variable, another variable already created to replace it
train_set <- sample(1:nrow(citydeal3), 6500)

citydeal_test_set <- citydeal3[-citydeal_train_set,]

citydeal_train_set <- citydeal3[train_set,]

citydeal_fit <- tree(Employed_any_any_time ~ ., 
                     data = citydeal3)

summary(citydeal_fit)






#random forest
city.rf <- randomForest(Employed_any_any_time ~ ., 
                        data = citydeal3, importance = TRUE)


importance(city.rf)
summary(city.rf)
varImpPlot(city.rf)

