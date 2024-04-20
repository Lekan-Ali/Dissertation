#read in dataset
df <- readxl::read_excel("Pakistan_used_cars.xlsx")

#replace missing values for the assembly column
df2 <- df %>% 
  mutate(assembly = case_when(assembly == "Imported" ~ "Imported"
                              , TRUE ~ "Local"))



#drop other rows with missing values since missing values rows is less than 20% of our data
df2 %>% 
  na.omit() -> df3

#box plots for transmission, fuel and assembly
c1 <- df3 %>% 
  ggplot(aes(log(price,10), transmission)) +
  geom_boxplot() +
  theme(axis.text=element_text(size = 10)
        , legend.title = element_blank()
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')) +
  coord_flip() +
  labs(y="")


c2 <- df3 %>% 
  ggplot(aes(log(price,10), transmission, fill = fuel)) +
  geom_boxplot() +
  theme(legend.position = "top"
        , legend.title = element_blank()
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80'))+
  #scale_fill_discrete(breaks=c('Petrol', 'Hybrid', 'Diesel')) +
  coord_flip() +
  labs(x="", y="")


c3 <- df3 %>% 
  ggplot(aes(log(price,10), transmission, fill = assembly)) +
  geom_boxplot() +
  theme(legend.position = "top"
        ,, legend.title = element_blank()
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')) +
  #scale_fill_discrete(breaks=c('Local', 'Imported')) +
  coord_flip() +
  labs(x="", y="")

grid.arrange(c1, c2, c3, ncol = 3)