#read in dataset
df <- readxl::read_excel("Pakistan_used_cars.xlsx")

#replace missing values for the assembly column
df2 <- df %>% 
  mutate(assembly = case_when(assembly == "Imported" ~ "Imported"
                              , TRUE ~ "Local"))



#drop other rows with missing values since missing values rows is less than 20% of our data
df2 %>% 
  na.omit() -> df3



# group dataset by assembly
df3 %>% 
  group_by(assembly) %>% 
  summarize(count = n()) -> df3_assembly


# group dataset by transmission
df3 %>% 
  group_by(transmission) %>% 
  summarize(count = n()) -> df3_transmission



# group dataset by fuel
df3 %>% 
  group_by(fuel) %>% 
  summarize(count = n()) -> df3_fuel


#create column plots for assembly, transmission and fuel
b1 <- df3_assembly %>% 
  ggplot(aes(assembly, count)) +
  geom_col() +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major.x = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')) +
  geom_text(aes(label = scales::comma(count)), vjust = -0.2) +
  labs(y = "number of cars listed") +
  scale_y_continuous(labels = comma)


b2 <- df3_transmission %>% 
  ggplot(aes(transmission, count)) +
  geom_col() +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major.x = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')) +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = scales::comma(count)), vjust = -0.2) +
  labs(y = "")


b3 <- df3_fuel %>% 
  ggplot(aes(fuel, count)) +
  geom_col() +
  theme(axis.text=element_text(size = 10)
        , panel.grid.minor = element_blank()
        , panel.grid.major.x = element_blank()
        , panel.grid.major = element_line(linetype = 2, size = 0.2
                                          , colour = 'grey80')) +
  geom_text(aes(label = scales::comma(count)), vjust = -0.2) +
  scale_y_continuous(labels = comma) +
  labs(y = "")



grid.arrange(b1, b2, b3, ncol = 3)