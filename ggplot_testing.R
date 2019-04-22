test_summary <-
  model_cluster_assignments %>%
  sample_n(1000) %>%
  group_by(cluster) %>%
  summarise(count = n()) %>%
  mutate(p = count / sum(count)) %>%
  arrange(p) %>%
  mutate(Hub = LETTERS[sequence(n())] %>% as_factor()) %>%
  print()


#fc8d59 #red
#ffffbf #yellow
#91cf60 #green

colors <- c("Normal" = "#91cf60", "Heavy" = "#fc8d59", "Slow" = "#ffffbf")


test_data_plot <-
  # data_summary_new %>%
  test_summary %>%

  mutate(p_exp = vlookup(data = model_cluster_summary, match_value = Hub, match_column = "Hub", return_value = "p"),
         p_dif = p_exp - p,
         d_magnitude = abs(p_dif),
         d_sign = ifelse(p_dif > 0, 1, -1),
         d_threshold_factor = case_when(p < 0.05 ~ 1,
                                        p < 0.10 ~ 0.75,
                                        p < 0.20 ~ 0.50,
                                        TRUE     ~ 0.20),
         d_magnitude_thr = p * d_threshold_factor,
         d_warn = (d_magnitude > d_magnitude_thr) * d_sign,
         d_color = case_when(d_warn == -1 ~ "Slow",
                             d_warn ==  1 ~ "Heavy",
                             d_warn ==  0 ~ "Normal") %>%
           factor(levels = c("Normal", "Slow", "Heavy"))
  )

test_data_plot %>% view()
test_data_plot$d_color


test_data_plot %>%
  ggplot(aes(x = Hub, y = p, fill = d_color)) +
  geom_col(color = "black") +
  coord_flip() +
  geom_text(aes(label = scales::number(count, big.mark = ",")), nudge_y = 0.025, size = 3) +
  scale_y_continuous(name = NULL, limits = c(0, 0.60), labels = scales::percent_format()) +
  scale_fill_manual(values = colors, drop = FALSE, name = "Live vs. Expected Requests", guide = guide_legend(title.position = "top")) +
  ggridges::theme_ridges() +
  theme(legend.position = "bottom", legend.justification = "center") +
  labs(title = "Percent of All Traffic by Hub", x = NULL)
