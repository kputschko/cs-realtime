
# Predict GPS Events ------------------------------------------------------

pacman::p_load(
  tidyverse,
  feather,
  broom,
  lubridate,
  rlang,
  sp,
  flexclust,
  RColorBrewer,
  ggQC
)

# Helper Function ---------------------------------------------------------

fx_vlookup <- function(match_value, data, match_column, return_value) {
  match <- match(match_value, data[[match_column]])
  data[[return_value]][match]
}

# Import Data -------------------------------------------------------------

data_raw <-  read_feather("taxi_request/uber_0714.feather")


# Tidy Data ---------------------------------------------------------------

data_tidy <-
  data_raw %>%
  select(-Base) %>%
  rename(datetime = `Date/Time`) %>%
  mutate(weekday = lubridate::wday(datetime, label = TRUE, abbr = FALSE),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime),
         hour_label = case_when(
           hour %in% 7:14 ~ "daytime",
           hour %in% 15:21 ~ "evening",
           TRUE ~ "night"
         )) %>%
  arrange(datetime) %>%
  print()


# Cluster -----------------------------------------------------------------

model_size <- 30000
model_proportion <- model_size / nrow(data_raw)
model_prop_holdout  <- 1 - (model_proportion * 3)

model_data <-
  data_tidy %>%
  modelr::resample_partition(c(train = model_proportion,
                               test  = model_proportion,
                               live  = model_proportion,
                               hold  = model_prop_holdout))

model_cluster_list <-
  tibble(k = 1:10) %>%
  mutate(model = map(k, ~model_data$train %>% as_tibble() %>% select(Lat, Lon) %>% kmeans(centers = .x)),
         glance = map(model, glance))


model_cluster_champion <-
  model_cluster_list %>%
  unnest(glance) %>%
  arrange(k) %>%
  mutate(rate = 100 * (tot.withinss - lag(tot.withinss)) / lag(tot.withinss)) %>%
  mutate(mark_potential = ifelse(rate < lag(rate), 1, 0)) %>%
  group_by(mark_potential) %>%
  mutate(mark_count = sequence(n()),
         best_k = ifelse(mark_potential == 1 & mark_count == 1, TRUE, FALSE)) %>%
  ungroup() %>%
  filter(best_k) %>%
  select(k, tot.withinss, model) %>%
  print()


model_cluster_k <-
  model_cluster_champion %>%
  pull(k)

model_cluster <-
  kcca(model_data$train %>% as_tibble() %>% select(Lat, Lon),
       k = model_cluster_k,
       family = kccaFamily("kmeans"))

model_cluster_centers <-
  model_cluster %>%
  attr("centers") %>%
  as_tibble() %>%
  rowid_to_column("cluster")

model_cluster_assignments <-
  model_data$test %>%
  as_tibble() %>%
  add_column(cluster = predict(model_cluster, .[c("Lat", "Lon")]))


model_cluster_distances <-
  model_cluster_assignments %>%
  mutate(Lon.center = fx_vlookup(cluster, model_cluster_centers, "cluster", "Lon"),
         Lat.center = fx_vlookup(cluster, model_cluster_centers, "cluster", "Lat")) %>%
  rowwise() %>%
  mutate(distance = geosphere::distm(c(Lon, Lat), c(Lon.center, Lat.center))) %>%
  ungroup() %>%
  print()


model_cluster_summary <-
  model_cluster_distances %>%
  group_by(cluster) %>%
  summarise(Lat = unique(Lat.center),
            Lon = unique(Lon.center),
            count = n(),
            d_avg = mean(distance),
            d_med = median(distance),
            d_iqr = IQR(distance),
            d_out = if_else(distance > (d_med + d_iqr * 2), 1, 0) %>% mean()) %>%
  mutate(p = count / sum(count)) %>%
  arrange(p) %>%
  mutate(Hub = letters[sequence(n())] %>% str_to_upper() %>% as_factor(),
         Color = brewer.pal(n(), "Set1"))

data_points_hub <-
  model_cluster_distances %>%
  left_join(model_cluster_summary %>% select(cluster, Hub)) %>%
  print()


# Time of Day -------------------------------------------------------------

data_tidy %>%
  ggplot(aes(x = hour)) +
  geom_bar()


# Quality Control ---------------------------------------------------------
# Look at highest capacity hub

# test_qc_cluster <- data_points_hub %>% filter(p == max(p)) %>% pull(cluster) %>% print()

data_points_hub %>%
  # filter(cluster == test_qc_cluster) %>%
  arrange(datetime) %>%
  count(Hub, day, hour) %>%
  complete(Hub, day, hour, fill = list(n = 0)) %>%
  mutate(hour_label =
           case_when(
             hour %in% 7:14 ~ "daytime",
             hour %in% 15:21 ~ "evening",
             TRUE ~ "night"
           ),
         night_value = ifelse(hour_label == "night" & hour > 12, hour - 24, hour)) %>%

  ggplot(aes(x = factor(night_value), y = n, color = hour_label, group = hour_label)) +
  geom_point(alpha = 0.20) +
  geom_smooth(se = FALSE, color = "black") +
  facet_grid(rows = vars(Hub), scales = "free_y")

  ggplot(aes(x = factor(night_value), y = n, group = 1, color = hour_label)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_QC(method = "XmR") +
  geom_point(alpha = 0.20) +
  facet_grid(rows = vars(Hub), cols = vars(hour_label), scales = "free_x")


# Post Cluster ------------------------------------------------------------

model_cluster_distances %>%
  count(cluster, hour) %>%
  complete(cluster, hour, fill = list(n = 0)) %>%

  ggplot(aes(x = hour, y = n, color = factor(cluster))) +
  geom_smooth()

data_distances <-
  model_cluster_distances %>%
  gather(weekday, indicator, contains("weekday_")) %>%
  filter(indicator == 1) %>%
  mutate(weekday = str_remove(weekday, "weekday_"))

data_tidy %>%
  count(weekday, hour) %>%
  complete(weekday, hour, fill = list(n = 0)) %>%

  ggplot(aes(x = hour, y = n, color = factor(weekday))) +
  geom_smooth()


# Research Questions ------------------------------------------------------

# Cluster based on location alone?
# Collapse Hours: 11p-5a, 6a-4p, 5p-10p?
# Expectation for next hour?
# Expectation for next day?
# When should we adjust clusters?
# - When the hub load is higher/lower than normal?
# - Quality control metrics?

# 1. What is the expected distance drivers in a particular hub are expected to drive in a given weekday and hour?
# - distance ~ weekday + hour + cluster

# 2. What is the expected number of rides to be given in a particular hub during a given weekday and hour?
# - n ~ weekday + hour + cluster

model_glm_distance <-
  glm(distance ~ hour + factor(cluster) + weekday, data = data_distances)

model_glm_distance %>% tidy()

data_test <- data_distances %>% group_by(cluster) %>% sample_n(1) %>% print()
model_glm_distance %>% predict(data_test)


