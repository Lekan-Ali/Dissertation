#decision trees
library(tree)
tree.credit <- tree(Balance ~ Rating + Limit, data = Credit, )

summary(tree.credit)
plot(tree.credit, size = 0.1)
text(tree.credit, pretty = 0, cex = 0.8, col = "red")

library(ISLR2)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)



plot(tree.boston)
text(tree.boston, pretty = 0)



library(randomForest)
set.seed(1)

#removing ID from Credit data
Credit %>% 
  dplyr::select(-ID) -> Credit

# train sample of 80%
train <- sample(1:nrow(Credit), nrow(Credit)*0.8)

#run model with only training data
tree.credit <- tree(Balance ~ ., data = Credit, subset = train)


plot(tree.credit)
text(tree.credit, pretty = 0)

#pruning tree
cv.cred <- cv.tree(tree.credit)
plot(cv.cred$size, cv.cred$dev, type = "b")

prune.credit <- prune.tree(tree.credit, best = 5)
plot(prune.credit)
text(prune.credit, pretty = 0)


#score
yhat <- predict(tree.credit, newdata = Credit[-train, ])
cred.test <- Credit[-train, "Balance"]
plot(yhat, cred.test)
abline(0,1)
tree.test <- mean((yhat - cred.test)^2)

tree.test
.sqrt(tree.test)



rf.credit <- randomForest(Balance ~ ., data = Credit
                          , subset = train, mtry = 5, importance = TRUE)


yhat.rf <- predict(rf.credit, newdata = Credit[-train, ])
m_test <- mean((yhat.rf - cred.test)^2)
sqrt(m_test)
m_test


.varImpPlot(rf.credit)
importance(rf.credit)

importance(rf.credit)[,2] %>% 
  as.data.frame() %>% 
  mutate(total = ./sum(.)*100) %>% 
  arrange(-total) %>% 
  dplyr::select(total) -> df_feature_imp_1


feature_names <- row.names(df_feature_imp_1)

df_feature_imp_2 <- data.frame(feature_names, f$total)

names(df_feature_imp_2)[2] <- "total_share"

df_feature_imp_2 %>% 
  ggplot(aes(reorder(feature_names,total_share), total_share)) +
  geom_col(fill = "grey70") + 
  geom_col(data = df_feature_imp_2 %>% 
             filter(total_share > 10), fill = "#FF9999") +
  coord_flip() +
  scale_y_continuous(expand = c(0,1)) +
  #theme_minimal() +
  theme(panel.grid.minor.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.major.x = element_line(linetype = 2
                                            , colour = "black"
                                            , size = 0.2)
        , axis.text = element_text(size = 8)
        , axis.title.x = element_text(size = 10)) +
  labs(y = "feature importance in percentage share (%)"
       , x = "") +
  geom_text(data = df_feature_imp_2 %>% 
              filter(total_share < 10)
            , aes(label = round(total_share,1), hjust = -0.3), size = 3.5) +
  geom_text(data = df_feature_imp_2 %>% 
              filter(total_share > 10)
            , aes(label = round(total_share,1), hjust = 1.2), size = 3.5)
  




















df_feature_imp_2 %>% ggplot(aes())

geom_text(data = df_make_pivoted %>% #add text for number of cars greater than 5k
            filter(metrics == "number_of_cars") %>%
            filter(value > 5000),
          aes(label = scales::comma(round(value,1)),hjust = 1))


bp <- ggplot(f, aes(x = total
              , y = reorder(b, total))) +
  geom_col(fill = "grey60")



  bp + geom_text(data = f %>% filter(total <10), aes(label = round(total,1)))

geom_text(data = df_make_pivoted %>% #add text for number of cars greater than 5k
            filter(metrics == "number_of_cars") %>%
            filter(value > 5000),
          aes(label = scales::comma(round(value,1)),hjust = 1))


barplot(f$total, names.arg = row.names(f), horiz = FALSE, ylab = "feature importance share (%)", xlab = "features", family = "mono", ylim = c(0,50), space = 0.1)




text(f, labels = round(f$total,1), pos = 3, cex = 1)




barplot(f$total, names.arg = row.names(f), horiz = TRUE)
