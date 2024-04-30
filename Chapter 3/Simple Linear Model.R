#Codes used for Chapter 2: Section 3.3.1, Simple Linear Regression.

#Step 1: Load the dataset, Credit dataset in the ISLR2 package.
library(ISLR2) #this is needed to load the Credit data.
library(tidyverse) #this is needed for ggplot2
library(gridExtra) 

#Step 2: plot the relationship between credit Rating (predictor) and credit Balance (Response).
# see Figure 3.1 in Dissertation or Figure "rating_balance_plot.png" in the Chapter 3 folder of this project
ggplot(Credit, aes(Rating, Balance)) +
  geom_point(col = "red") +
  theme(panel.grid.major = element_line(linetype = 2
                                        , colour = "grey70"
                                        , size = 0.2)
        , panel.grid.minor = element_blank())



#Step 3: Plot 50 observations to illustrate the least square approach to minimizing sum of residuals
# see Figure 3.2 in Dissertation or see Figure "least_squares_plot.png" in Chapter 3 folder of this project
Credit %>% 
  top_n(50) %>% 
  mutate(res = residuals(lm(Balance ~ Rating))) %>% 
  ggplot(aes(Rating, Balance)) +
  geom_point(alpha = 0.5, color = "red") +
  geom_smooth(se = F, method = "lm") +
  geom_segment(aes(xend = Rating, yend = Balance - res)) +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80'))





#Step 4: Fit credit Rating on credit Balance using linear model
# see Figure 3.3 or Fig 3.4 in Dissertation or see Figure "least_squares_plot.png" in Chapter 3 folder of this project
lm.simple <- lm(Balance ~ Rating, data = Credit)

#summary
summary(lm.simple)





#Step 5:
# see Figure 3.4 in Dissertation in the Figures folder in Chapter 3 folder of this project
p1 <- Credit %>% 
  ggplot(aes(Rating, Balance)) +
  geom_point(alpha = 0.5, color = "red") +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80'))


p2 <- Credit %>% 
  ggplot(aes(Rating, Balance)) +
  geom_point(alpha = 0.3, color = "red") +
  geom_smooth(se = F, method = "lm") +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80'))


grid.arrange(p1, p2, ncol = 2)






#Step 6: Multiple linear regression Model
# see Figure 3.4 in Dissertation in the Figures folder in Chapter 3 folder of this project