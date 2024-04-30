
## this is linear diagnostics analysis for the simple linear regression

#Step 1: Load the dataset, Credit dataset in the ISLR2 package.
library(ISLR2) #this is needed to load the Credit data.

# Step 2: Perform simple linear regression
lm.simple <- lm(Balance ~ Rating, data = Credit)

#Model summary
summary(lm.simple)


#to create multiple plots in 1 canvas
par(mfrow = c(2,2))

# Step 3: Plot diagnostic plots
# see Figure 3.6, 3.7 and 3.8 in Dissertation
plot(lm.simple)



########### #Step 4: plot to pin-point outliers ########################
#see Figure 3.9 in Dissertation

#create new dataset from the credit data and add fitted, residual values from linear model
full_model_after_fitting <- Credit %>% 
  mutate(fitted = lm.simple$fitted.values
         , residuals = lm.simple$residuals
         , row_numb = row.names(Credit))


#scatter plot
#calculate the residuals and create a new column to the dataset
p1 <- full_model_after_fitting %>% 
  mutate(res = residuals(lm(Balance ~ Rating)))


#create a scatter plot
p2 <- ggplot(p1, aes(Rating, Balance)) +
  geom_point(alpha = 0.5, color = "red")


#add regression line to the plot
p3 <- p2 + geom_smooth(se = F, method = "lm") 


# create a new dataset - this is needed to pinpoint outliers
full_model_after_fitting %>% 
  filter(residuals < - 700 | residuals > 700) -> df_outliers



# add the vertical line to show outliers
p4 <- p3 + 
  geom_segment(data = df_outliers
               , aes(xend = Rating
                     , yend = Balance - residuals)) 


# add theme 
p5 <- p4 + theme(axis.text=element_text(size = 10)
                 , panel.grid.minor = element_blank()
                 , panel.grid.major = element_line(linetype = 2, size = 0.2
                                                   , colour = 'grey80'))


# add text to identify outliers
p6 <- p5 + geom_label_repel(data = full_model_after_fitting %>% 
                              filter(residuals < - 700 | residuals > 700)
                            , aes(label = row_numb)) 

# create a linear regression model without the outliers
full_model_after_fitting %>% 
  filter(residuals > - 700 & residuals < 700) -> df_no_outliers

# plot the linear model regression line without the outliers
p7 <- p6 + geom_smooth(data = df_no_outliers
                       , method = "lm", se = F, color = "black"
                       , alpha = 0.9, size = 0.8, linetype = 2)

# final plot should have two regression lines (1 blue line and 1 dashed black line)
p7


########### #Step 5: plot to pin-point high-leverage observations ########################
#see Figure 3.10 in Dissertation

#finding the high leverage observation(s)
full_model_after_fitting <- Credit %>% 
  mutate(fitted = lm.simple$fitted.values
         , residuals = lm.simple$residuals
         , row_numb = row.names(full_model_after_fitting))


#scatter plot
#calculate the residuals and create a new column to the dataset
q1 <- full_model_after_fitting %>% 
  mutate(res = residuals(lm(Balance ~ Rating)))


#create a scatter plot
q2 <- ggplot(q1, aes(Rating, Balance)) +
  geom_point(alpha = 0.5, color = "red")


#add regression line to the plot
q3 <- q2 + geom_smooth(se = F, method = "lm") 


# create a new dataset - this is needed to pinpoint observations with high leverage
full_model_after_fitting %>% 
  filter(Rating > 950) -> df_high_leverage



# add theme 
q4 <- q3 + theme(axis.text=element_text(size = 10)
                 , panel.grid.minor = element_blank()
                 , panel.grid.major = element_line(linetype = 2, size = 0.2
                                                   , colour = 'grey80'))


# add text to identify outliers
q5 <- q4 + geom_label_repel(data = df_high_leverage
                            , aes(label = row_numb)) 

# create a linear regression model without the outliers
full_model_after_fitting %>% 
  filter(Rating < 850) -> df_no_high_leverage

# plot the linear model regression line without the outliers
q6 <- q5 + geom_smooth(data = df_no_high_leverage
                       , method = "lm", se = F, color = "black"
                       , alpha = 0.9, size = 0.8, linetype = 2)

# final plot should have two regression lines (1 blue line and 1 dashed black line)
# this was not included in the dissertation
q6



#draw a line representing the mean and add text (this is an addition to the plot above or q6)
# see Figure 3.10 in dissertation
q2 +
  geom_hline(yintercept = mean(full_model_after_fitting$Balance)) +
  geom_vline(xintercept = mean(full_model_after_fitting$Rating)) +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')) +
  geom_label_repel(data = df_high_leverage
                   , aes(label = row_numb)
                   , size = 2
                   , nudge_x = -0.1) +
  annotate("text", x = 750, y = 570
           , label = "average balance ($520)", size = 3) +
  annotate("text", x = 470, y = 1900
           #, angle = 90
           , label = "average rating (354)", size = 3)



#observations with high-leverage
#hat.plot <- function(fit){
#  p <- length(coefficients(fit))
#  n <- length(fitted(fit))
#  plot(hatvalues(fit), main = "Index plot")
#  abline(h=c(2,3)*p/n, col = "red", lty = 2)
#  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
#}

#hat.plot(lm.simple)

#Observations with substantial impact
#cutoff <- 4/(nrow(Credit)-length(lm.simple$coefficents)-2)
#plot(lm.simple, which=4, cook.levels=cutoff)
#abline(h=cutoff, lty = 2, col = "red")

#full_model_after_fitting[c(356,122, 276),]







########### #Step 6: plot to pin-point influential observations and effects when removed########################
#see Figure 3.11 in Dissertation



#scatter plot
#calculate the residuals and create a new column to the dataset
r1 <- full_model_after_fitting %>% 
  mutate(res = residuals(lm(Balance ~ Rating)))



#create a scatter plot
r2 <- ggplot(p1, aes(Rating, Balance)) +
  geom_point(alpha = 0.5, color = "red")



#add regression line to the plot
r3 <- r2 + geom_smooth(se = F, method = "lm") 



# create a new dataset - this is needed to pinpoint outliers
full_model_after_fitting[c(356, 122, 276),] -> df_influentialobs



# add theme 
r4 <- r3 + theme(axis.text=element_text(size = 10)
                 , panel.grid.minor = element_blank()
                 , panel.grid.major = element_line(linetype = 2, size = 0.2
                                                   , colour = 'grey80'))


# add text to identify outliers
r5 <- r4 + geom_label_repel(data = df_influentialobs
                            , aes(label = row_numb)) 

# create a linear regression model without the outliers
full_model_after_fitting[c(-356, -122, -276),] -> df_no_influential

# plot the linear model regression line without the outliers
r6 <- r5 + geom_smooth(data = df_no_influential
                       , method = "lm", se = F, color = "black"
                       , alpha = 0.9, size = 0.8, linetype = 2)

# final plot should have two regression lines (1 blue line and 1 dashed black line)
r6




