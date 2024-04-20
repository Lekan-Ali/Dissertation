#this dataset is from the linear and polynomial sheet
df_train_set


#performing multiple linear regression
#Step 1: Remove columns that have multiple factors

df_train_adjusted <- df_train_set %>% 
  select(-addref, -city, 
         -body, -make, -model, -color, -registered)




#Step 2: fit model
lm.multi <- lm(price ~ ., data = df_train_adjusted)


#Step 3: Check model summary
summary(lm.multi)


#Step 3: diagnostic plot
par(mfrow = c(1,1))
plot(lm.multi)




#Step 4 Advanced diagnostic plot
library(car)
par(mfrow = c(1,1))

#1. Normality plot
qqPlot(lm.multi
       , labels = row.names(df_train_adjusted)
       , main = "Normality plot"
       , simulate = TRUE
       , family = "mono"
       , grid = F)

#checking for outliers from the normalty plot
df_train_adjusted[11638,]
fitted(lm.multi)[11638]


df_train_adjusted[869,]
fitted(lm.multi)[869]

#2. Linearity Assumptions
crPlots(lm.multi)


#3. Homoscedacity Assumption
ncvTest(lm.multi)
spreadLevelPlot(lm.multi, family = "mono")



#4. Independence of Error
durbinWatsonTest(lm.multi)



#5. Collinearity
vif(lm.multi)


#6. High leverage points
hat.plot <- function(model){
  a <- length(coefficients(model))
  b <- length(fitted(model))
  plot(hatvalues(model), main = "")
  abline(h=c(2,3)*a/b, col = "red", lty = 2)
}

hat.plot(lm.multi)

#7. Influential Observations
cutoff <- 200/(nrow(df_train_adjusted)-length(lm.multi$coefficients)-2)
plot(lm.multi, which = 4, cook.levels=cutoff, main = "Influential Observations", family = "mono")
abline(h=cutoff, lty=2, col = "red")

#Added variable plot
avPlots(lm.multi, ask = F, d=list(method="identify")
        , grid = F, col = alpha(1,0.4)
        , main = paste("Added-variable plots and influential observations"), family = "mono")





# Influential Plot
influencePlot(lm.multi, main = ""
              , family = "mono"
              , col = alpha(1,0.4))



