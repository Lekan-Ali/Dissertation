#read in dataset
df <- readxl::read_excel("Pakistan_used_cars.xlsx")

#replace missing values for the assembly column
df2 <- df %>% 
  mutate(assembly = case_when(assembly == "Imported" ~ "Imported"
                              , TRUE ~ "Local"))



#drop other rows with missing values since missing values rows is less than 20% of our data
df2 %>% 
  na.omit() -> df3



#plot of numeric variables with transmission (engine, mileage and year + price + transmission)


d1 <- df3 %>% 
  ggplot(aes(engine/1e3,log(price,10), color = transmission)) +
  geom_point(alpha = 0.2) +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')
        , legend.position = "top"
        , legend.title = element_blank()) +
  labs(x = "engine (in thousand)"
       , y = "")



d2 <- df3 %>% 
  ggplot(aes(mileage/1e3, log(price,10), color = transmission)) +
  geom_point(alpha = 0.2) +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')
        , legend.position = "top"
        , legend.title = element_blank()) +
  scale_x_continuous(expand = c(0,15)) +
  labs(x = "mileage (in thousand)"
       , y = "")



d3 <- df3 %>% 
  ggplot(aes(year,log(price,10), color = transmission)) +
  geom_point(alpha = 0.2) +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')
        , legend.position = "top"
        , legend.title = element_blank())




grid.arrange(d3, d1, d2, ncol = 3)