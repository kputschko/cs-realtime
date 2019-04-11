
# Uber Traffic Data -------------------------------------------------------

pacman::p_load(tidyverse, sp, ggmap)

# Requires a Google Maps API Key

# Data --------------------------------------------------------------------

# Google Map - Drive Time
# https://www.r-bloggers.com/adding-google-drive-times-and-distance-coefficients-to-regression-models-with-ggmap-and-sp/

# GitHub - Uber Data
# https://github.com/fivethirtyeight/uber-tlc-foil-response/tree/master/uber-trip-data


data_uber_0714 <-
  read_csv("https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-jul14.csv?raw=true",
           n_max = 1000)


# Driving Time ------------------------------------------------------------

test <-
  data_uber_0714 %>%
  mutate(avg_lat = mean(Lat), avg_lon = mean(Lon))


# Returns KM
# sp::spDists(x = test %>% select(Lon, Lat) %>% as.matrix(),
#             y = test %>% select(avg_lon, avg_lat) %>% as.matrix(),
#             longlat = TRUE)

# Returns Minutes
# ggmap::mapdist(c(Lat, Lon), to = c(avg_lat, avg_lon), mode = "driving"))

test %>%
  head(10) %>%
  qmplot(Lon, Lat, data = ., geom = c("point"))
