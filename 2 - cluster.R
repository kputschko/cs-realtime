
# Clustering ----------------------------------------------------------------

pacman::p_load(tidyverse, h2o, leaflet, RColorBrewer)


# Load Data ---------------------------------------------------------------

data_train <- read_rds("data/train.rds")
data_hours <- read_csv("data/hour_labels.csv")


# H2O ---------------------------------------------------------------------

h2o.init()

data_train_h2o <-
  data_train %>%
  select(contains("start_")) %>%
  as.h2o()


# Cluster -----------------------------------------------------------------

model_cluster <-
  h2o.kmeans(data_train_h2o,
             nfolds = 5,
             k = 6,
             standardize = FALSE,
             model_id = "model_cluster",
             score_each_iteration = TRUE,
             max_iterations = 50)

model_cluster_size <- model_cluster %>% h2o.centers() %>% as_tibble()
model_cluster_center <- model_cluster %>% h2o.centroid_stats(xval = TRUE) %>% as_tibble()

model_cluster_summary <-
  model_cluster_center %>%
  bind_cols(model_cluster_size) %>%
  rename(center_lat = start_lat, center_lon = start_lon) %>%
  select(-within_cluster_sum_of_squares) %>%
  arrange(-size) %>%
  mutate(hub = LETTERS[centroid],
         prop = size / sum(size),
         hub_color = brewer.pal(n(), "Set1")) %>%
  print()



# Cluster - Plot ----------------------------------------------------------

model_cluster_summary %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng = ~center_lon,
                   lat = ~center_lat,
                   label = ~str_c("HUB - ", hub),
                   opacity = 1,
                   weight = 1,
                   fillOpacity = 1,
                   fillColor = ~hub_color,
                   color = "black")


# Export - Cluster --------------------------------------------------------

model_cluster_summary %>% write_csv("data/model_cluster_summary.csv")

h2o.saveModel(model_cluster, "data/model_cluster", force = TRUE)

# Shutdown H2O ------------------------------------------------------------

h2o.shutdown(prompt = FALSE)
