library(tidyverse)
library(VIM)
library(corrplot)


#load in dataset

#intall packages
#install.packages("corrplot")
#install.packages("GGally")
#install.packages("scales")


# load libraries
library(tidyverse)
library(VIM) #to view missing columns
library(corrplot) #to visualize correlation plot
library(ggplot2)
library(GGally)
library(grid)
library(gridExtra)
library(scales)



#read in dataset
df <- readxl::read_excel("Pakistan_used_cars.xlsx")

# check column names
colnames(df)


#check dimension
dim(df)

# check data types
glimpse(df)


#top 10 view of dataset
head(df,10)


#correlation of numeric features
## Step1: extract only numeric features and save in another variable
select_if(df, is.numeric) %>% 
  na.omit() %>% 
  cor() -> df_numeric


## Step 2corrplot of numeric features
corrplot(df_numeric, order = 'AOE' 
         , method = "number"
         , pch = 
           , number.cex = 1)


## Dealing with missing Values
aggr(df, prop = FALSE, numbers = TRUE, cex.lab = 1.2)
