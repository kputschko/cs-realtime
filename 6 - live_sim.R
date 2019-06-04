
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


# Live Data - Hourly ------------------------------------------------------
# Done in hourly batches

# data_live_iterations <-
#   data_live %>%
#   group_by(year, month, day, weekday, hour, hour_label) %>%
#   group_nest(keep = TRUE) %>%
#   slice(1:10) %>%
#   mutate(h2o = map(data, ~ .x %>% select(start_lat, start_lon) %>% as.h2o()),
#          cluster = map(h2o, ~ h2o.predict(model_cluster, .x) %>% as_tibble() %>% magrittr::add(1) %>% rename(centroid = predict))) %>%
#   print()


# df_summary will tell us how many rides were denied service for being too far away
# and will indicate if the hub*hour traffic was slower or heavier than normal

# data_live_iterations_hub <-
#   data_live_iterations %>%
#   select(-h2o) %>%
#   mutate(df_cluster = map2(data, cluster, bind_cols),
#          df_hub = map(df_cluster,
#                       ~ .x %>%
#                         left_join(data_cluster, by = "centroid") %>%
#                         left_join(data_outlier %>% select(hub, weekday, hour, hour_label, distance_ub),  by = c("weekday", "hour", "hour_label", "hub")) %>%
#                         rowwise() %>%
#                         mutate(distance_hub = geosphere::distm(x = c(start_lon, start_lat), y = c(center_lon, center_lat))) %>%
#                         ungroup() %>%
#                         mutate(outlier_distance = ifelse(distance_hub > distance_ub, 1, 0),
#                                cost_expense = 0.25 * (distance_trip + distance_hub) / 1000,
#                                cost_rate = 0.75,
#                                cost_revenue = 2 + cost_rate * distance_trip / 1000,
#                                cost_profit = cost_revenue - cost_expense,
#                                time_trip = distance_trip / 670.5,
#                                time_hub = distance_hub / 670.5)
#          ),
#          df_summary = map(df_hub,
#                           ~ .x %>%
#                             group_by(hub, year, month, day, weekday, hour, hour_label) %>%
#                             summarise(p_distance_outlier = mean(outlier_distance),
#                                       n_rides = n()) %>%
#                             left_join(data_outlier %>% select(-distance_ub), by = c("weekday", "hour", "hour_label", "hub")) %>%
#                             mutate(residual = n_rides - n_predict,
#                                    busy = case_when(residual < n_resid_lb ~ "Slow",
#                                                     residual > n_resid_ub ~ "Heavy",
#                                                     TRUE ~ "Normal"))
#          )
#   ) %>%
#   select(-data, -cluster, -df_cluster) %>%
#   print()
#
# data_live_iterations_hub$df_hub[[9]] %>% View("live")
# data_live_iterations_hub$df_summary[[9]] %>% View("summary")
#
# data_live_iterations_hub$df_summary[[8]] %>%
#   ungroup() %>%
#   select(-year, -month, -day, -weekday, -hour, -hour_label, -p_distance_outlier) %>%
#   mutate(rate = case_when(busy == "Normal" ~ 0.75,
#                           busy == "Slow" ~ 0.75 * (n_resid_lb / residual),
#                           busy == "Heavy" ~ 0.75 * 1.05 * (residual / n_resid_ub))
#   )



# Live Data - Actual ------------------------------------------------------
# Processed as Points are Received
# Parallel processing for Shiny Application output
# - Parallel processing DOES NOT WORK, the running summary is separated by core

system.time({

  running_requests <- tibble()

  live_simulation <-

    # for (i in 1:20) {
    foreach(i = 1:20) %do% {

      # Recieve the Request
      new_request <- data_live[i, ]

      # Assign to Hub
      hub_assignment <-
        new_request %>%
        select(start_lat, start_lon) %>%
        as.h2o() %>%
        h2o.predict(model_cluster, .) %>%
        magrittr::add(1) %>%
        as_tibble() %>%
        rename(centroid = predict) %>%
        bind_cols(new_request)

      # Approve / Deny Service
      approve_service <-
        hub_assignment %>%
        left_join(data_cluster, by = "centroid") %>%
        left_join(data_outlier %>% select(hub, weekday, hour, hour_label, distance_ub),  by = c("weekday", "hour", "hour_label", "hub")) %>%
        rowwise() %>%
        mutate(distance_hub = geosphere::distm(x = c(start_lon, start_lat), y = c(center_lon, center_lat))) %>%
        ungroup() %>%
        mutate(outlier_distance = ifelse(distance_hub > distance_ub, 1, 0))

      # Running Summary
      running_summary <-
        running_requests %>%
        bind_rows(approve_service) %>%
        group_by(hub, year, month, day, weekday, hour, hour_label) %>%
        summarise(p_distance_outlier = mean(outlier_distance),
                  n_rides = n()) %>%
        arrange(year, month, day, weekday, hour, hub)

      # Determine Request Volume
      request_volume <-
        running_summary %>%
        left_join(data_outlier %>% select(-distance_ub), by = c("weekday", "hour", "hour_label", "hub")) %>%
        mutate(residual = n_rides - n_predict) %>%
        mutate(busy = case_when(residual < n_resid_lb ~ "Slow",
                                residual > n_resid_ub ~ "Heavy",
                                TRUE ~ "Normal"))

      # Adjust Request Fee Based on Request Volume
      price_estimate <-
        approve_service %>%
        left_join(request_volume %>% select(hub, year, month, day, weekday, hour, busy, n_resid_lb, residual, n_resid_ub),
                  by = c("weekday", "year", "month", "day", "hour", "hub")) %>%
        mutate(cost_rate = case_when(busy == "Normal" ~ 0.75,
                                     busy == "Slow" ~ 0.75 * (n_resid_lb / residual),
                                     busy == "Heavy" ~ 0.75 * 1.05 * (residual / n_resid_ub))) %>%
        mutate(cost_rate = case_when(busy == "Slow" & distance_trip > 10000 ~ cost_rate * 2,
                                     busy == "Slow" ~ min(0.25, cost_rate),
                                     TRUE ~ cost_rate)) %>%
        mutate(cost_expense = 0.25 * (distance_trip + distance_hub) / 1000,
               cost_revenue = 2 + cost_rate * distance_trip / 1000,
               cost_profit = cost_revenue - cost_expense,
               time_trip = distance_trip / 670.5,
               time_hub = distance_hub / 670.5,
               time_total = time_hub + time_trip)

      # Output
      running_requests <-
        bind_rows(running_requests, price_estimate) %>%
        arrange(datetime)


      lst(req = running_requests,
          sum = running_summary)

    }

  gc()

})


# Live Data - Presentation ------------------------------------------------

# Recieve the Request
pres_requests <- data_live

# Assign Hub
pres_hubs <-
  data_live %>%
  select(start_lat, start_lon) %>%
  as.h2o() %>%
  h2o.predict(model_cluster, .) %>%
  magrittr::add(1) %>%
  as_tibble() %>%
  rename(centroid = predict) %>%
  bind_cols(pres_requests)

# Approve / Deny Service
pres_approve <-
  pres_hubs %>%
  left_join(data_cluster, by = "centroid") %>%
  left_join(data_outlier %>% select(hub, weekday, hour, hour_label, distance_ub),  by = c("weekday", "hour", "hour_label", "hub")) %>%
  rowwise() %>%
  mutate(distance_hub = geosphere::distm(x = c(start_lon, start_lat), y = c(center_lon, center_lat))) %>%
  ungroup() %>%
  mutate(outlier_distance = ifelse(distance_hub > distance_ub, 1, 0))

# Running Summaries
pres_summary <-
  pres_approve %>%
  # slice(1:50) %>%
  group_by(hub, year, month, day, weekday, hour, hour_label) %>%
  mutate(summary_n_request = sequence(n())) %>%
  group_by(year, month, day, hub) %>%
  mutate(summary_p_outlier = cummean(outlier_distance)) %>%
  left_join(data_outlier %>% select(-distance_ub), by = c("weekday", "hour", "hour_label", "hub")) %>%
  mutate(residual = summary_n_request - n_predict) %>%
  mutate(busy = case_when(residual < n_resid_lb ~ "Slow",
                          residual > n_resid_ub ~ "Heavy",
                          TRUE ~ "Normal")) %>%
  mutate(cost_rate = case_when(busy == "Normal" ~ 0.75,
                               busy == "Slow" ~ 0.75 * (n_resid_lb / residual),
                               busy == "Heavy" ~ 0.75 * 1.05 * (residual / n_resid_ub))) %>%
  mutate(cost_expense = 0.25 * (distance_trip + distance_hub) / 1000,
         cost_revenue = case_when(busy == "Slow" & distance_trip > 10000 ~ 2 + 2 * cost_rate * distance_trip / 1000,
                                  TRUE ~ 2 + max(0.25, cost_rate) * distance_trip / 1000),
         cost_profit = cost_revenue - cost_expense,
         time_trip = distance_trip / 670.5,
         time_hub = distance_hub / 670.5,
         time_total = time_hub + time_trip) %>%
  ungroup() %>%
  rowid_to_column()

# How to present this in Shiny Dashboard?
# - Get Last Traffic Status?
pres_summary %>%
  ungroup() %>%
  arrange(desc(datetime)) %>%
  distinct(hour, hub, .keep_all = TRUE) %>%
  select(hour, hub, ride = summary_n_request, busy = busy, deny = summary_p_outlier, time = datetime) %>%
  arrange(hour, hub)


# Export ------------------------------------------------------------------

write_rds(pres_summary, "data/live_metadata.rds", compress = "gz")
