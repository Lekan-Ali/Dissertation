#### plot to show car make by mean and median price #########
# summarize by car body type
df3 %>% 
  group_by(make) %>% 
  summarise(number_of_cars = n()
            , "avg_mean_price(in million)" = mean(price)/1e6
            ,  "avg_median_price(in million)" = median(price)/1e6) %>% 
  arrange(-number_of_cars) %>% 
  mutate(rank = rank(number_of_cars)) %>% 
  filter(rank > 27) -> df_make_top_23




#pivot longer to create facet plot
df_make_pivoted <- df_make_top_23 %>% 
  pivot_longer(c(-make,-rank), names_to = "metrics"
               , values_to = "value")



df_make_pivoted$metrics <- factor(df_make_pivoted$metrics
                                  , levels = c("number_of_cars"
                                               , "avg_mean_price(in million)"
                                               , "avg_median_price(in million)"))

#facet plot without the figures
df_make_facet_plot <- df_make_pivoted %>% 
  ggplot(aes(reorder(make,rank), value)) +
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

df_make_facet_plot

#text or figures added to number of cars plot
df_make_facet_plot_number_of_cars <- df_make_facet_plot +
  geom_text(data = df_make_pivoted %>% #add text for number of cars less than 5k
              filter(metrics == "number_of_cars") %>%
              filter(value < 5000),
            aes(label = scales::comma(round(value,1)),hjust = -0.09)) +
  geom_text(data = df_make_pivoted %>% #add text for number of cars greater than 5k
              filter(metrics == "number_of_cars") %>%
              filter(value > 5000),
            aes(label = scales::comma(round(value,1)),hjust = 1))


df_make_facet_plot_number_of_cars





#text or figures added to average mean price of cars
df_make_facet_plot_avg_mean_price <- df_make_facet_plot_number_of_cars +
  geom_col(data = df_make_pivoted %>% #to highlight car bodies with outliers
             filter(metrics == "avg_mean_price(in million)") %>%
             filter(make == "Lexus" | make == "Range" | make == "Mercedes")
           , fill = "#FF9999") +
  geom_text(data = df_make_pivoted %>% #add text for number of cars less than 5k
              filter(metrics == "avg_mean_price(in million)") %>%
              filter(value < 12),
            aes(label = round(value,1),hjust = -0.09)) +
  geom_text(data = df_make_pivoted %>% #add text for number of cars less than 5k
              filter(metrics == "avg_mean_price(in million)") %>%
              filter(value > 12),
            aes(label = round(value,1),hjust = 1.1))



#text or figures added to average median price of cars
df_make_facet_plot_avg_median_price <- df_make_facet_plot_avg_mean_price +
  geom_col(data = df_make_pivoted %>% #to highlight some cars with outliers
             filter(metrics == "avg_median_price(in million)") %>%
             filter(make == "Lexus" | make == "Range" | make == "Mercedes")
           , fill = "#FF9999") +
  geom_text(data = df_make_pivoted %>% #add text for number of cars less than 5k
              filter(metrics == "avg_median_price(in million)") %>%
              filter(value < 12),
            aes(label = round(value,1),hjust = -0.09)) +
  geom_text(data = df_make_pivoted %>% #add text for number of cars less than 5k
              filter(metrics == "avg_median_price(in million)") %>%
              filter(value > 12),
            aes(label = round(value,1),hjust = 1.1)) +
  labs(y = "values", x="")


df_make_facet_plot_avg_median_price



#final plot
df_make_facet_plot_avg_median_price