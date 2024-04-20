#read in dataset
df <- readxl::read_excel("Pakistan_used_cars.xlsx")

#replace missing values for the assembly column
df2 <- df %>% 
  mutate(assembly = case_when(assembly == "Imported" ~ "Imported"
                              , TRUE ~ "Local"))



#drop other rows with missing values since missing values rows is less than 20% of our data
df2 %>% 
  na.omit() -> df3



#plot of price and cars engine
a1 <-  df3 %>% 
  ggplot(aes(engine/1e3,log(price,10))) +
  geom_point(alpha = 0.2, color = "black") +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')) +
  geom_smooth(linetype = 2, color = 'red', size = 0.8
              , se = F) +
  labs(x = "engine volume (in thousand)")





#plot of price and cars mileage
a2 <- df3 %>% 
  ggplot(aes(mileage/1e3, log(price,10))) +
  geom_point(alpha = 0.2, color = "black") +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')) +
  geom_smooth(linetype =2, color = 'red', size = 0.8
              , se = F) +
  scale_x_continuous(expand = c(0,15)) +
  labs(x = "mileage (in thousand)")



#plot of price and car production year
a3 <- df3 %>% 
  ggplot(aes(year,log(price,10))) +
  geom_point(alpha = 0.2, color = "black") +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')) +
  geom_smooth(linetype =2, color = 'red', size = 1
              , se = F)


#plot of price vs engine, mileage and year
grid.arrange(a1, a2, a3, ncol = 3)