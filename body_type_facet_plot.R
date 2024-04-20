
#read in dataset
df <- readxl::read_excel("Pakistan_used_cars.xlsx")

#replace missing values for the assembly column
df2 <- df %>% 
  mutate(assembly = case_when(assembly == "Imported" ~ "Imported"
                              , TRUE ~ "Local"))



#drop other rows with missing values since missing values rows is less than 20% of our data
df2 %>% 
  na.omit() -> df3


#### plot to show car bodies by mean and median price #########
# summarize by car body type
df3 %>% 
  group_by(body) %>% 
  summarise(number_of_cars = n()
            , "avg_mean_price(in million)" = mean(price)/1e6
            ,  "avg_median_price(in million)" = median(price)/1e6) %>% 
  arrange(-number_of_cars) %>% 
  mutate(rank = rank(number_of_cars)) -> df_body

#pivot longer to create facet plot
df_body_pivoted <- df_body %>% 
  pivot_longer(c(-body,-rank), names_to = "metrics"
               , values_to = "value")



df_body_pivoted$metrics <- factor(df_body_pivoted$metrics
                                  , levels = c("number_of_cars"
                                               , "avg_mean_price(in million)"
                                               , "avg_median_price(in million)"))

#facet plot without the figures
df_body_facet_plot <- df_body_pivoted %>% 
  ggplot(aes(reorder(body,rank), value)) +
  geom_col(fill = "grey65") +
  coord_flip() +
  facet_wrap(metrics ~ ., scales = "free_x") +
  theme(axis.text=element_text(size = 12)
        , panel.grid.minor = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.major.x = element_line(linetype = 2, size = 0.2
                                            , colour = 'grey40')
        , strip.background = element_rect(fill = "grey30")
        , strip.text = element_text(color = "white", size = 12, face = "bold")) +
  scale_y_continuous(labels = scales::comma)

df_body_facet_plot

#text or figures added to number of cars plot
df_body_facet_plot_number_of_cars <- df_body_facet_plot +
  geom_text(data = df_body_pivoted %>% #add text for number of cars less than 5k
              filter(metrics == "number_of_cars") %>%
              filter(value < 5000),
            aes(label = scales::comma(round(value,1)),hjust = -0.09)) +
  geom_text(data = df_body_pivoted %>% #add text for number of cars greater than 5k
              filter(metrics == "number_of_cars") %>%
              filter(value > 5000),
            aes(label = scales::comma(round(value,1)),hjust = 1))


#text or figures added to average mean price of cars
df_body_facet_plot_avg_mean_price <- df_body_facet_plot_number_of_cars +
  geom_col(data = df_body_pivoted %>% #to highlight car bodies with outliers
             filter(metrics == "avg_mean_price(in million)") %>%
             filter(body == "SUV" | body == "Pick Up" | body == "Coupe" | body == "Convertible")
           , fill = "#FF9999") +
  geom_text(data = df_body_pivoted %>% #add text for number of cars less than 5k
              filter(metrics == "avg_mean_price(in million)") %>%
              filter(value < 12),
            aes(label = round(value,1),hjust = -0.09)) +
  geom_text(data = df_body_pivoted %>% #add text for number of cars less than 5k
              filter(metrics == "avg_mean_price(in million)") %>%
              filter(value > 12),
            aes(label = round(value,1),hjust = 1.1))



#text or figures added to average median price of cars
df_body_facet_plot_avg_median_price <- df_body_facet_plot_avg_mean_price +
  geom_col(data = df_body_pivoted %>% #to highlight some cars with outliers
             filter(metrics == "avg_median_price(in million)") %>%
             filter(body == "SUV" | body == "Pick Up" | body == "Coupe" | body == "Convertible")
           , fill = "#FF9999") +
  geom_text(data = df_body_pivoted %>% #add text for number of cars less than 5k
              filter(metrics == "avg_median_price(in million)") %>%
              filter(value < 12),
            aes(label = round(value,1),hjust = -0.09)) +
  geom_text(data = df_body_pivoted %>% #add text for number of cars less than 5k
              filter(metrics == "avg_median_price(in million)") %>%
              filter(value > 12),
            aes(label = round(value,1),hjust = 1.1)) +
  labs(y = "values", x="body type of cars")


#final plot
df_body_facet_plot_avg_median_price
