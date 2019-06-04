
# New Shiny Dashboard -----------------------------------------------------

# Maybe flexhdashboard could do the job?
# Maybe rmarkdown could do the job?
# Maybe gganimate could do the job?

# One point at a time
# Show NYC map with updating points
# Plot number of rides per hour and hub
# Indicate when hub is slow/normal/heavy in traffic
# Show denial of service rate
# Plot total trip time by hub
# Incorporate cost/revenue/profit

# Good Luck Future KP


# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse, leaflet)

# Live - Sample -----------------------------------------------------------

data_live <- read_rds("data/live_metadata.rds")
data_hubs <- read_rds("data/model_cluster_summary.rds")

# 1 - 10 ------------------------------------------------------------------

i_hi <- 750
i_lo <- max(1, i_hi - 250)

data_moving <- data_live %>% slice(1:i_hi)
data_subset <- data_moving %>% slice(i_lo:i_hi)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng = ~center_lon,
                   lat = ~center_lat,
                   label = ~str_c("HUB - ", hub),
                   opacity = 1,
                   weight = 1,
                   fillOpacity = 1,
                   fillColor = ~hub_color,
                   color = "black",
                   data = data_hubs) %>%
  addCircles(lng = ~start_lon, lat = ~start_lat,
             weight = 1,
             opacity = 0.40,
             fillOpacity = 0.40,
             fillColor = ~hub_color,
             color = "black",
             radius = 200,
             data = data_subset)

data_summary <-
  data_moving %>%
  ungroup() %>%
  arrange(desc(datetime)) %>%
  distinct(hour, hub, .keep_all = TRUE) %>%
  select(hour, hub, ride = summary_n_request, busy = busy, deny = summary_p_outlier, time = datetime) %>%
  arrange(hour, hub)

data_summary %>%
  ggplot(aes(x = hour, y = ride, fill = factor(busy, levels = c("Normal", "Slow", "Heavy")))) +
  geom_col(color = "black") +
  facet_grid(rows = vars(hub), scales = "free_y") +
  scale_x_discrete(drop = FALSE, name = NULL) +
  scale_fill_manual(values = c("Normal" = "#91cf60", "Heavy" = "#fc8d59", "Slow" = "#ffffbf"),
                    drop = FALSE,
                    name = "Live vs. Expected Requests",
                    guide = guide_legend(title.position = "top")) +
  theme(legend.position = "bottom", legend.justification = "center")



# Animation ---------------------------------------------------------------

pacman::p_load(tidyverse, gganimate)

ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() +
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

mtcars %>%
  ggplot(aes(factor(cyl), mpg)) +
  geom_boxplot() +
  transition_states(gear, transition_length = 2, state_length = 1) +
  ease_aes("sine-in-out")


data_live %>%
  ungroup() %>%
  arrange(desc(datetime)) %>%
  distinct(hour, hub, .keep_all = TRUE) %>%
  select(hour, hub, ride = summary_n_request, busy = busy, deny = summary_p_outlier, time = datetime) %>%
  arrange(hour, hub) %>%

  ggplot(aes(x = hub, y = ride, fill = factor(busy, levels = c("Normal", "Slow", "Heavy")))) +
  geom_col(color = "black") +
  scale_x_discrete(drop = FALSE, name = NULL) +
  scale_fill_manual(values = c("Normal" = "#91cf60", "Heavy" = "#fc8d59", "Slow" = "#ffffbf"),
                    drop = FALSE,
                    name = "Live vs. Expected Requests",
                    guide = guide_legend(title.position = "top")) +
  theme(legend.position = "bottom", legend.justification = "center") +
  transition_states(hour, transition_length = 2, state_length = 2) +
  ease_aes("sine-in-out")

# library(gapminder)
#
# ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
#   geom_point(alpha = 0.7, show.legend = FALSE) +
#   scale_colour_manual(values = country_colors) +
#   scale_size(range = c(2, 12)) +
#   scale_x_log10() +
#   facet_wrap(~continent) +
#   # Here comes the gganimate specific bits
#   labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
#   transition_time(year) +
#   ease_aes('linear')

# data_live %>%
#   filter(hub == "A") %>%
#   sample_n(100) %>%
#   ggplot(aes(x = 1, y = summary_n_request, fill = factor(busy, levels = c("Normal", "Slow", "Heavy")))) +
#   geom_point(shape = 21) +
#   scale_fill_manual(values = c("Normal" = "#91cf60", "Heavy" = "#fc8d59", "Slow" = "#ffffbf"),
#                     drop = FALSE,
#                     name = "Live vs. Expected Requests",
#                     guide = guide_legend(title.position = "top")) +
#   transition_time(datetime)


# Fee Plots ---------------------------------------------------------------



data_live %>%
  ungroup() %>%
  arrange(desc(datetime)) %>%
  distinct(hub, .keep_all = TRUE) %>%

  ggplot() +
  aes(
      y = forcats::fct_rev(hub),
      x = "A",
      fill = busy,
      size = cost_rate,
      label = str_glue("Hub {hub} - {dollar(cost_rate)}")) +
  geom_label(label.padding = unit(0.50, "lines")) +
  scale_radius(range = c(4, 8), guide = "none") +
  # scale_radius(range = c(3, 6), guide = "none") +
  scale_fill_manual(values = c("Normal" = "#91cf60", "Heavy" = "#fc8d59", "Slow" = "#ffffbf"),
                    drop = FALSE,
                    name = NULL,
                    guide = "none") +
  theme_no_axes(base.theme = theme_minimal()) +
  labs(title = "Request Fee per Mile",
       subtitle = "Fees are adjusted based on hub request volume",
       caption = "Higher fees attract more drivers, while lower fees push drivers into other areas")


# Profit Margins ----------------------------------------------------------

data_live %>%
  group_by(hub) %>%
  summarise(mean_expense = mean(cost_expense),
            mean_revenue = mean(cost_revenue),
            mean_profit  = mean(cost_profit),
            n_rides      = n())

data_live$cost_profit %>% summary()
data_live %>% filter(cost_profit < 0) %>% view()
  select(distance_trip, contains("cost"))

data_live %>%
  ggplot() +
  aes(x = fct_rev(hub), y = cost_profit, fill = hub) +
  geom_boxplot(varwidth = TRUE, outlier.alpha = 0.05, outlier.size = 1) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 25), labels = dollar_format()) +
  scale_fill_manual(values = data_hubs %>% select(hub, hub_color) %>% deframe(),
                    guide = "none") +
  theme_minimal() +
  labs(x = "Hub", y = NULL, title = "Driver's Profit per Request", caption = "Box height is determined by number of requests")


# Expected Rides ----------------------------------------------------------

data_live %>%
  filter(day == 1, hour == 7) %>%
  ggplot(aes(x = 1, y = summary_n_request)) +
  geom_point() +
  # geom_ribbon(aes(ymin = 0, ymax = max(0, n_predict - n_resid_lb)), fill = "blue", alpha = 0.40) +
  geom_hline(aes(yintercept = n_predict + n_resid_ub), color = "red") +
  geom_hline(aes(yintercept = n_predict), color = "green") +
  geom_hline(aes(yintercept = n_predict + n_resid_lb), color = "orange") +
  facet_grid(cols = vars(hub))

data_live %>%
  slice(1:200) %>%
  arrange(desc(datetime)) %>%
  distinct(hub, .keep_all = TRUE) %>%

  ggplot() +
  aes(x = hub,
      y = summary_n_request,
      fill = factor(busy, levels = c("Normal", "Slow", "Heavy"))) +
  geom_col(color = "black") +
  scale_x_discrete(drop = FALSE, name = NULL) +
  scale_fill_manual(values = c("Normal" = "#91cf60", "Heavy" = "#fc8d59", "Slow" = "#ffffbf"),
                    drop = FALSE,
                    name = NULL,
                    guide = guide_legend(title.position = "top")) +
  labs(title = "Request Volume by Hour", y = "Number of Requests", x = NULL,
       subtitle = str_c("Hour - ", .data$hour %>% str_pad(2, side = "left", pad = "0"), ":00")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.justification = "center") +
  geom_errorbar(aes(ymin = n_predict + n_resid_lb,
                    ymax = n_predict + n_resid_ub),
                width = 0.10)

