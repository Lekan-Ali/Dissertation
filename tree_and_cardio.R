#install for tree partition
#remotes::install_github("grantmcdermott/parttree")


# install.packages("remotes")
remotes::install_github("grantmcdermott/parttree")

remotes::install_github("grantmcdermott/parttree")

library(parsnip)
library(titanic) ## Just for a different data set
set.seed(123) ## For consistent jitter
titanic_train$Survived = as.factor(titanic_train$Survived)
## Build our tree using parsnip (but with rpart as the model engine)
ti_tree =
  decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification") %>%
  fit(Survived ~ Pclass + Age, data = titanic_train)
## Plot the data and model partitions
titanic_train %>%
  ggplot(aes(x=Pclass, y=Age)) +
  geom_jitter(aes(col=Survived), alpha=0.7) +
  geom_parttree(data = ti_tree, aes(fill=Survived), alpha = 0.1) +
  theme_minimal()






#df <- readxl::read_excel("Pakistan_used_cars.xlsx")
library(tidyverse)
library(corrplot)
library(GGally)


#Step 1: Load dataset
cardio <- read_csv("cardio_data_processed.csv")


#Step 2: view dataset
glimpse(cardio)


#Step 3: remove id column and age and bp_category
cardio_df2 <- cardio %>% 
  dplyr::select(-id, -age, -bp_category)



#Step 4:
#Preprocess dataset
cardio_df3 <- cardio_df2 %>% 
  mutate(gender = case_when(gender == 1 ~ "Female", TRUE ~ "Male")
         , cholesterol = case_when(cholesterol == 1 ~ "Normal", cholesterol == 2 ~ "Above Normal", TRUE ~ "Above Normal")
         , gluc = case_when(gluc == 1 ~ "Normal", gluc == 2 ~ "Above Normal", TRUE ~ "Above Normal")
         , smoke = case_when(smoke == 0 ~ "Non-smoker", TRUE ~ "Smoker")
         , alco = case_when(alco == 0 ~ "No", TRUE ~ "Yes")
         , active = case_when(active == 0 ~ "Not Active", TRUE ~ "Active")
         , cardio = case_when(cardio == 0 ~ "Absense", TRUE ~ "Presence"))


# convert to factors
cardio_df3$gluc <- as.factor(cardio_df3$gluc)
cardio_df3$smoke <- as.factor(cardio_df3$smoke)
cardio_df3$alco <- as.factor(cardio_df3$alco)
cardio_df3$active <- as.factor(cardio_df3$active)
cardio_df3$cardio <- as.factor(cardio_df3$cardio)
cardio_df3$gender <- as.factor(cardio_df3$gender)

# check dataset to confirm changes
glimpse(cardio_df3)


# dataset for anova analysis
cardio_df4 <- cardio_df2 %>% 
  mutate(gender = case_when(gender == 1 ~ "Female", TRUE ~ "Male")
         , cholesterol = case_when(cholesterol == 1 ~ "Normal", cholesterol == 2 ~ "Above Normal", TRUE ~ "Above Normal")
         , gluc = case_when(gluc == 1 ~ "Normal", gluc == 2 ~ "Above Normal", TRUE ~ "Above Normal")
         , smoke = case_when(smoke == 0 ~ "Non-smoker", TRUE ~ "Smoker")
         , alco = case_when(alco == 0 ~ "No", TRUE ~ "Yes")
         , active = case_when(active == 0 ~ "Not Active", TRUE ~ "Active"))



#Step 4: scatterplot matrix for numeric columns
#collate only numeric columns

adj <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) +
    geom_density(..., alpha = 0.2, fill = NA) +
    theme(axis.text = element_text(size = 0.5))
}
ggpairs(cardio_df3
        , mapping = aes(color = cardio)
        , columns = c("age_years", "height", "weight", "ap_hi", "ap_lo"
                      , "gender")
        , upper = list(continuous = wrap("cor", size = 3))
        , diag = list(continuous = adj))



ggplot(cardio_df3, aes(active)) +
  geom_bar() +
  facet_grid(cardio~gender)


ggplot(cardio_df3, aes(ap_hi, ap_lo,color = cardio)) +
  geom_point(alpha = 0.7) +
  facet_grid(~active)
  


library(ISLR2)

ggplot(Credit, aes(Rating, Limit)) +
  geom_point() +
  geom_vline(xintercept = 278.5) +
  geom_segment(y = 5353, x = 278.5, yend = 5353, xend = 278.5)



aov(cardio ~ smoke, data = cardio_df4)













#Step 5:Perform futher analysis (ANOVA)
summary(aov(cardio ~ bmi, data = cardio))
library(gplots)
library(HH)
plotmeans(cardio ~ active*gender, data = cardio_df3)

ancova(cardio ~ active + gender , data = cardio_df3)

#Step 6: Perform 
