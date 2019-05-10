
# H2O Modeling ------------------------------------------------------------

pacman::p_load(tidyverse, h2o, DataExplorer, feather, lubridate)

# Import Data -------------------------------------------------------------

data_raw <-  read_feather("taxi_request/uber_0714.feather")


# Tidy Data ---------------------------------------------------------------

map_hours <-
  lst(daytime = 7:14,
      evening = 15:21,
      night = c(0:6, 22, 23)) %>%
  enframe("hour_label", "hour") %>%
  unnest() %>%
  arrange(hour)

data_tidy <-
  data_raw %>%
  select(-Base) %>%
  rename(datetime = `Date/Time`) %>%
  mutate(weekday = lubridate::wday(datetime, label = TRUE, abbr = FALSE),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime)) %>%
  left_join(map_hours) %>%
  arrange(datetime) %>%
  mutate_at(vars(weekday, year, month, day, hour, hour_label), factor, ordered = FALSE) %>%
  print()


# Cluster -----------------------------------------------------------------

model_size <- 30000
model_proportion <- model_size / nrow(data_raw)
model_prop_holdout  <- 1 - (model_proportion * 3)

model_data <-
  data_tidy %>%
  select(-datetime) %>%
  modelr::resample_partition(c(train = model_proportion,
                               test  = model_proportion,
                               live  = model_proportion,
                               hold  = model_prop_holdout))


h2o.init()

data_h2o_train <-
  model_data %>%
  pluck("train") %>%
  as.h2o()

data_h2o_test <-
  model_data %>%
  pluck("test") %>%
  as.h2o()



model_cluster <-
  h2o.kmeans(data_h2o_train, x = c("Lat", "Lon"),
             nfolds = 3,
             k = 5, #estimate_k = TRUE,
             standardize = FALSE)

cluster_centers <-
  model_cluster@model$centers %>%
  as_tibble() %>%
  mutate(Hub = LETTERS[sequence(n())] %>% factor()) %>%
  rename(lat_hub = lat, lon_hub = lon)


model_cluster_predict <-
  h2o.predict(model_cluster, data_h2o_test) %>%
  as.vector()

data_test_cluster <-
  model_data$test %>%
  as_tibble() %>%
  mutate(centroid = model_cluster_predict + 1,
         centroid = centroid %>% as.character()) %>%
  left_join(cluster_centers)


# Predict -----------------------------------------------------------------

model_predict_train <-
  data_test_cluster %>%
  count(Hub, weekday, year, month, day, hour_label, hour) %>%
  as.h2o()


model_drf <-
  h2o.randomForest(y = "n",
                   x = c("Hub", "weekday", "hour", "hour_label"),
                   training_frame = model_predict_train)

# parameters taken from auto-ml
model_gbm <-
  h2o.gbm(y = "n",
          x = c("Hub", "weekday", "hour", "hour_label"),
          training_frame = model_predict_train,
          nfolds = 5,
          score_tree_interval = 5,
          ntrees = 100,
          max_depth = 10,
          min_rows = 1,
          sample_rate = 0.70,
          col_sample_rate = 0.40,
          stopping_metric = "AUTO")

# model_automl <-
#   h2o.automl(y = "n",
#              x = c("Hub", "weekday", "hour", "hour_label"),
#              training_frame = model_predict_train,
#              max_runtime_secs = 600)

test_prediction <- model_predict_train[runif(1, 1, 2000) %>% floor(), ]
test_prediction

model_automl@leader %>% h2o.predict(test_prediction)
model_gbm %>% h2o.predict(test_prediction)

h2o.varimp_plot(model_automl@leader)

complete_prediction_df <-
  data_test_cluster %>%
  distinct(Hub, weekday, hour) %>%
  arrange(Hub, weekday, hour) %>%
  complete(Hub, weekday, hour) %>%
  left_join(map_hours %>% mutate_all(factor))

complete_prediction_h2o <- complete_prediction_df %>% as.h2o()

complete_prediction <- h2o.predict(model_gbm, complete_prediction_h2o) %>% as.vector()

full_prediction <-
  complete_prediction_df %>%
  add_column(predict = complete_prediction)


full_prediction %>%
  ggplot(aes(x = hour, y = predict, color = hour_label, group = hour_label)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA)) +
  geom_smooth(se = FALSE, color = "black") +
  facet_grid(rows = vars(Hub), cols = vars(hour_label), scales = "free")


# Predict Distance --------------------------------------------------------

data_for_modeling_distance <-
  data_test_cluster %>%
  rowwise() %>%
  mutate(distance = geosphere::distm(x = c(Lat, Lon), y = c(lat_hub, lon_hub)))

data_for_modeling_distance_h2o <- data_for_modeling_distance %>% as.h2o()

# 1 - 35 trees, 3 mean depth, 1817 rmse
# model_distance_auto <-
#   h2o.automl(y = "distance",
#              x = c("hour", "hour_label", "Hub", "weekday"),
#              training_frame = data_for_modeling_distance_h2o,
#              nfolds = 5,
#              max_runtime_secs = 600)

# model_distance_auto@leader@parameters

# Based on Auto ML
model_distance_gbm <-
  h2o.gbm(y = "distance",
          x = c("Hub", "weekday", "hour", "hour_label"),
          training_frame = data_for_modeling_distance_h2o,
          nfolds = 5,
          score_tree_interval = 5,
          ntrees = 35,
          max_depth = 10,
          min_rows = 50,
          sample_rate = 0.90,
          col_sample_rate = 0.40,
          stopping_metric = "AUTO")

model_distance_gbm %>% h2o.varimp_plot()

complete_prediction_df <-
  data_test_cluster %>%
  distinct(Hub, weekday, hour) %>%
  arrange(Hub, weekday, hour) %>%
  complete(Hub, weekday, hour) %>%
  left_join(map_hours %>% mutate_all(factor))

complete_prediction_h2o <- complete_prediction_df %>% as.h2o()

complete_prediction_distance <- h2o.predict(model_distance_gbm, complete_prediction_h2o) %>% as.vector()

full_prediction_distance <-
  complete_prediction_df %>%
  add_column(predict = complete_prediction_distance)

full_prediction_distance %>%
  ggplot(aes(x = hour, y = predict, color = hour_label, group = hour_label)) +
  geom_point() +
  scale_y_continuous(limits = c(0, NA)) +
  geom_smooth(se = FALSE, color = "black") +
  facet_grid(rows = vars(Hub), cols = vars(hour_label), scales = "free")



# Shutdown ----------------------------------------------------------------

h2o.shutdown(FALSE)
