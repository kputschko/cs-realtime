
# Explore - Hub Load, Distance, Cost --------------------------------------
# determine normal rate of requests per hour (rph)
# decrease price if rph is lower than normal
# increase price if rph is higher than normal

# determine normal distance covered per hub
# if request is too far away, deny service

# determine number of completed requests per hour
# determine number of drivers needed to cover requests per hour
# if too many drivers, decrease price
# if too few drivers, increase price

# determine driver end hub
# should driver return to initial hub or to nearest hub?

# rank requests by profit margin?
# in eyes of single driver, give them best requests based on ending points


# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse, kp.helpers)


# Functions ---------------------------------------------------------------

fx_vlookup <- function(data, match_value, match_column, return_value) {
  match <- match(match_value, data[[match_column]])
  data[[return_value]][match]
}


# Import ------------------------------------------------------------------

data_cost <- read_rds("data/test_cost.rds")
data_clusters <- read_rds("data/model_cluster_summary.rds")


# Data Center -------------------------------------------------------------

# To predict number of requests and gauge what is too many or too few
data_requests <-
  data_cost %>%
  count(day, weekday, hour, hour_label, hub) %>%
  print()

# To identify which requests are too far from the hub
data_hub_distance_boundaries <-
  data_cost %>%
  group_by(hub) %>%
  summarise(iqr = IQR(distance_hub),
            q3 = quantile(distance_hub, 0.75),
            boundary = q3 + iqr * 1.5)


# Requests per Hour -------------------------------------------------------

data_cost %>%
  count(day, hour_label, hour) %>%
  ggplot(aes(x = hour, fill = hour_label, y = n)) +
  geom_boxplot(outlier.size = 0.75) +
  theme_kp() +
  labs(y = "Count", x = "Hour of Day", fill = "Time of Day", title = "Requests per Hour")

data_requests %>%
  ggplot(aes(x = hour, fill = hour_label, y = n)) +
  # geom_point(shape = 21) +
  geom_boxplot() +
  scale_y_sqrt() +
  theme_kp()

# Distance Monitor --------------------------------------------------------
# Distance rules should be based on distance alone, not time or day

data_cost %>%
  ggplot(aes(x = hub, y = distance_hub, fill = hub)) +
  geom_boxplot(varwidth = TRUE, outlier.size = 0.75, outlier.alpha = 0.50)


# Drivers per Hub ---------------------------------------------------------

# determine number of requests per hour, by hub
# determine average length of trip per hour, by hub
# determine average number of trips ONE driver could complete, by hub
# determine estimated number of drivers required per hour, by hub
# IT HAS TO BE BY HUB + HOUR

# Then, when live we look at one hour at a time
# And gauge if number of trips per driver is normal
# And request more/fewer drivers by increasing/decreasing price

data_cost %>%
  mutate(time_total = time_hub + time_trip) %>%
  ggplot(aes(x = hub, y = time_total, fill = hub)) +
  geom_boxplot()


data_cost %>%
  group_by(hub, hour) %>%
  summarise(mean_total_trip_time = mean(time_hub + time_trip),
            est_trips_per_hour = 60 / mean_total_trip_time,
            n_requests = n(),
            n_rph = n_requests / 24,
            r_dph = n_rph / est_trips_per_hour) %>%
  ggplot(aes(x = hour, y = r_dph)) +
  geom_point() +
  facet_grid(vars(hub)) +
  coord_equal(ratio = 0.50) +
  scale_y_continuous(limits = c(0, 14))

data_cost %>%
  group_by(day, weekday, hour, hour_label, hub) %>%
  summarise(rph = n(),
            time_mean_total = mean(time_hub + time_trip),
            tph = 60 / time_mean_total,
            dph = rph / tph)

data_cost %>%
  group_by(day, weekday, hour, hour_label, hub) %>%
  summarise(n_requests = n(),
            mean_trip_time = mean(time_hub + time_trip),
            est_trips_per_driver = 60 / mean_trip_time
            # est_driver_per_hub =
              ) %>%
  arrange(hub)

