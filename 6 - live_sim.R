
# Live Simulation ---------------------------------------------------------

pacman::p_load(tidyverse, kp.helpers, h2o, foreach)


# Functions ---------------------------------------------------------------

fx_vlookup <- function(data, match_value, match_column, return_value) {
  match <- match(match_value, data[[match_column]])
  data[[return_value]][match]
}

# Import ------------------------------------------------------------------

data_live <- read_rds("data/live.rds")
data_outlier <- read_rds("data/test_outlier_bounds.rds")
data_cluster <- read_rds("data/model_cluster_summary.rds") %>% select(-size, -prop)


# H2O ---------------------------------------------------------------------

h2o.init()

model_cluster <- h2o.loadModel("data/model_cluster/model_cluster")


# Live Data ---------------------------------------------------------------

data_live_iterations <-
  data_live %>%
  group_by(year, month, day, weekday, hour, hour_label) %>%
  group_nest(keep = TRUE) %>%
  slice(1:10) %>%
  mutate(h2o = map(data, ~ .x %>% select(start_lat, start_lon) %>% as.h2o()),
         cluster = map(h2o, ~ h2o.predict(model_cluster, .x) %>% as_tibble() %>% magrittr::add(1) %>% rename(centroid = predict))) %>%
  print()


# df_summary will tell us how many rides were denied service for being too far away
# and will indicate if the hub*hour traffic was slower or heavier than normal

data_live_iterations_hub <-
  data_live_iterations %>%
  select(-h2o) %>%
  mutate(df_cluster = map2(data, cluster, bind_cols),
         df_hub = map(df_cluster,
                      ~ .x %>%
                        left_join(data_cluster, by = "centroid") %>%
                        left_join(data_outlier %>% select(hub, weekday, hour, hour_label, distance_ub),  by = c("weekday", "hour", "hour_label", "hub")) %>%
                        rowwise() %>%
                        mutate(distance_hub = geosphere::distm(x = c(start_lon, start_lat), y = c(center_lon, center_lat))) %>%
                        ungroup() %>%
                        mutate(outlier_distance = ifelse(distance_hub > distance_ub, 1, 0))
         ),
         df_summary = map(df_hub,
                          ~ .x %>%
                            group_by(hub, year, month, day, weekday, hour, hour_label) %>%
                            summarise(p_distance_outlier = mean(outlier_distance),
                                      n_rides = n()) %>%
                            left_join(data_outlier %>% select(-distance_ub), by = c("weekday", "hour", "hour_label", "hub")) %>%
                            mutate(residual = n_rides - n_predict,
                                   busy = case_when(residual < n_resid_lb ~ "Slow",
                                                    residual > n_resid_ub ~ "Heavy",
                                                    TRUE ~ "Normal"))
         )
  ) %>%
  select(-data, -cluster, -df_cluster) %>%
  print()

data_live_iterations_hub$df_hub[[1]]
data_live_iterations_hub$df_summary[[9]] %>% view()
