
# Cluster - Predictions -----------------------------------------------------

pacman::p_load(tidyverse, kp.helpers, h2o, geosphere)


# Functions ---------------------------------------------------------------

fx_vlookup <- function(data, match_value, match_column, return_value) {
  match <- match(match_value, data[[match_column]])
  data[[return_value]][match]
}

# H2O ---------------------------------------------------------------------

h2o.init()

# Import ------------------------------------------------------------------

data_test <- read_rds("data/test.rds")
data_hours <- read_rds("data/hour_labels.rds")
data_clusters <- read_rds("data/model_cluster_summary.rds")
model_cluster <- h2o.loadModel("data/model_cluster/model_cluster")


# Prepare Data ------------------------------------------------------------
# assign each starting point to a hub
# get distance from hub to starting point

model_prediction <-
  data_test %>%
  select(start_lat, start_lon) %>%
  as.h2o() %>%
  h2o.predict(model_cluster, newdata = .) %>%
  as_tibble()

data_assignment <-
  data_test %>%
  bind_cols(model_prediction + 1) %>%
  left_join(data_clusters %>% select(centroid, contains("center"), hub, hub_color),
            by = c("predict" = "centroid")) %>%
  rowwise() %>%
  mutate(distance_hub = distm(c(start_lon, start_lat), c(center_lon, center_lat))) %>%
  ungroup() %>%
  print()

data_cost <-
  data_assignment %>%
  mutate(cost_expense = 0.25 * distance_hub / 1000,
         cost_revenue = 2 + 0.75 * distance_trip / 1000,
         time_trip = distance_trip / 670.5,
         time_hub = distance_hub / 670.5) %>%
  select(year, month, day, weekday, hour, hour_label, hub,
         contains("distance_"), contains("time_"), contains("cost_"),
         contains("start_"), contains("end_"), contains("center_"),
         hub_color, datetime) %>%
  arrange(datetime) %>%
  print()


# Export Data -------------------------------------------------------------

write_rds(data_cost, "data/test_cost.rds")


# Shutdown ----------------------------------------------------------------

h2o.shutdown(FALSE)
