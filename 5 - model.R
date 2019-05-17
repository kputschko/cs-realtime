
# Modeling ----------------------------------------------------------------

pacman::p_load(tidyverse, h2o)


# Import ------------------------------------------------------------------

data_cost <- read_rds("data/test_cost.rds")
data_clusters <- read_rds("data/model_cluster_summary.rds")


# Data Preparation --------------------------------------------------------

data_requests_per_hour <-
  data_cost %>%
  count(day, weekday, hour, hour_label, hub) %>%
  print()


data_distance_limits <-
  data_cost %>%
  group_by(hub, hour_label) %>%
  summarise(iqr = IQR(distance_hub),
            q3 = quantile(distance_hub, 0.75),
            distance_ub = q3 + iqr * 1.5) %>%
  print()


# H2O ---------------------------------------------------------------------

h2o.init()

h2o_req <- data_requests_per_hour %>% as.h2o()


# Model - Requests --------------------------------------------------------

# model_auto <-
#   h2o.automl(x = c("weekday", "hour", "hub", "hour_label"),
#              y = "n",
#              training_frame = h2o_req,
#              nfolds = 5,
#              max_runtime_secs = 900)
#
# model_auto@leader@parameters

model_gbm <-
  h2o.gbm(x = c("weekday", "hour", "hub", "hour_label"),
          y = "n",
          training_frame = h2o_req,
          nfolds = 5,
          ntrees = 82,
          max_depth = 17,
          min_rows = 30,
          learn_rate = 0.08,
          distribution = "gaussian",
          sample_rate = 0.80,
          col_sample_rate = 0.40)

predict_requests <-
  data_requests_per_hour %>%
  distinct(weekday, hour, hour_label, hub) %>%
  bind_cols(as.h2o(.) %>%
              h2o.predict(model_gbm, .) %>%
              as_tibble() %>%
              rename(n_predict = predict))

predict_residuals <-
  data_requests_per_hour %>%
  group_by(hub, weekday, hour, hour_label) %>%
  summarise(n_actual = mean(n)) %>%
  left_join(predict_requests) %>%
  mutate(residual = n_actual - n_predict) %>%
  group_by(hub, hour_label) %>%
  mutate(n_iqr = IQR(residual),
         n_q1 = quantile(residual, 0.25),
         n_q3 = quantile(residual, 0.75),
         n_resid_lb = n_q1 - 1.5 * n_iqr,
         n_resid_ub = n_q3 + 1.5 * n_iqr,) %>%
  select(-n_iqr, -n_q1, -n_q3) %>%
  print()


# Export ------------------------------------------------------------------

predict_residuals %>%
  select(-n_actual, -residual) %>%
  left_join(data_distance_limits %>% select(-iqr, -q3)) %>%
  write_rds("data/test_outlier_bounds.rds")
