
# Prepare Data ------------------------------------------------------------

pacman::p_load(tidyverse, h2o, feather, lubridate, geosphere)


# Import Data -------------------------------------------------------------
# import raw data from the feather file, which comes directly from the online source

data_raw <-
  read_feather("taxi_request/uber_0714.feather") %>%
  rename(datetime = `Date/Time`) %>%
  select(-Base)


# Testing - End Points ----------------------------------------------------
# Build the process to randomly assign end points

n <- 20000

data_start <-
  data_raw %>%
  sample_n(n) %>%
  rename(start_lat = Lat, start_lon = Lon) %>%
  print()

data_end <-
  data_raw %>%
  sample_n(n) %>%
  rename(end_lat = Lat, end_lon = Lon) %>%
  print()

data_start_end <-
  data_start %>%
  bind_cols(data_end) %>%
  rowwise() %>%
  mutate(distance = distm(c(start_lon, start_lat), c(end_lon, end_lat))) %>%
  print()


data_start_end %>%
  filter(distance < 30000,
         distance > 500) %>%

  ggplot(aes(x = distance)) +
  geom_histogram(binwidth = 1000, color = "black", fill = "gray90") +
  scale_y_sqrt("sqrt(count)") +
  scale_x_continuous(labels = scales::unit_format(scale = 1/1000, unit = "km")) +
  labs(title = "Randomly Assigned End Point", subtitle = "Histogram Bins of 2km") +
  theme_minimal()


# Actual Start + End Points -----------------------------------------------

data_split <-
  data_raw %>%
  modelr::resample_partition(c(start = 0.50, end = 0.50)) %>%
  map(as_tibble)

data_start <-
  data_split$start %>%
  rename(start_lat = Lat, start_lon = Lon)

data_end <-
  data_split$end %>%
  select(end_lat = Lat, end_lon = Lon) %>%
  slice(1:nrow(data_start))

data_combine <-
  data_start %>%
  bind_cols(data_end) %>%
  rowwise() %>%
  mutate(distance = distm(c(start_lon, start_lat), c(end_lon, end_lat))) %>%
  ungroup() %>%
  print()

# --- Remove first and last 5% of points based on distance between the two points
# data_filter <-
#   data_combine %>%
#   ungroup() %>%
#   mutate(rank = dplyr::percent_rank(distance),
#          group = cut(distance, quantile(distance, probs = seq(0, 1, by = 0.05)), labels = FALSE)) %>%
#   print()
# data_filter %>% filter(rank < 0.05) %>% summarise(max(distance))
# data_filter %>% filter(rank > 0.95) %>% summarise(min(distance))
# data_filter %>% filter(group == 1) %>% summarise(max(distance))
# data_filter %>% filter(group == 20) %>% summarise(min(distance))

data_filter <-
  data_combine %>%
  ungroup() %>%
  mutate(group = cut(distance, quantile(distance, probs = seq(0, 1, by = 0.05)), labels = FALSE)) %>%
  filter(group != 1, group != 20) %>%
  select(-group) %>%
  arrange(datetime) %>%
  print()


# Plot - Final Start + End ------------------------------------------------

data_filter %>%
  ggplot(aes(x = distance)) +
  geom_histogram(binwidth = 1000, color = "black", fill = "gray90") +
  scale_y_sqrt("sqrt(count)") +
  scale_x_continuous(labels = scales::unit_format(scale = 1/1000, unit = "km")) +
  labs(title = "Randomly Assigned End Points", subtitle = "Histogram Bins of 1 km") +
  theme_minimal()


# Tidy Data ---------------------------------------------------------------

# --- Create labels for hours
map_hours <-
  lst(daytime = 7:14,
      evening = 15:21,
      night = c(0:6, 22, 23)) %>%
  enframe("hour_label", "hour") %>%
  unnest() %>%
  arrange(hour)


# --- Export labels
write_csv(map_hours, "data/hour_labels.csv")


# --- Deconstruct date/time column and convert to factor
data_tidy <-
  data_filter %>%
  mutate(weekday = wday(datetime, label = TRUE, abbr = FALSE),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime)) %>%
  left_join(map_hours) %>%
  arrange(datetime) %>%
  mutate_at(vars(weekday, year, month, day, hour, hour_label), factor, ordered = FALSE) %>%
  print()


# Modeling Split ----------------------------------------------------------
# Export directly to 'data' folder

data_tidy %>%
  modelr::resample_partition(c(train = 0.10, test = 0.10, live = 0.10, holdout = 0.70)) %>%
  enframe() %>%
  slice(-4) %>%
  mutate(path = str_glue("data/{name}.rds")) %>%
  select(path, value) %>%
  deframe() %>%
  walk2(., names(.), ~ .x %>% as_tibble() %>% write_rds(.y))

