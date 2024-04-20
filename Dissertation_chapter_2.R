#install.packages("tidyverse")
#install.packages("ISLR2")
#install.packages("gridExtra")


library(tidyverse)
library(gridExtra)
library(ISLR2)


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






p1 <- Credit %>% 
  #top_n(100) %>%
  ggplot(aes(Rating, Balance)) +
  geom_point(alpha = 0.5, color = "red") +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80'))


p2 <- Credit %>% 
  #top_n(100) %>%
  ggplot(aes(Rating, Balance)) +
  geom_point(alpha = 0.3, color = "red") +
  geom_smooth(se = F, method = "lm") +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80'))


grid.arrange(p1, p2, ncol = 2)




#build the linear model
lm.fit <- lm(Balance~ Rating, data = df)

summary(lm.fit)

plot(df$Rating, df$Balance
     , xlab = "Rating"
     , ylab = "Balance")

abline(lm.fit)



#simple linear regression model
lm.simple <- lm(Balance ~ Rating, data = Credit)


#summary
summary(lm.simple)

#plot diagnostic plot for simple linear regression
par(mfrow = c(2,2))
plot(lm.simple)
