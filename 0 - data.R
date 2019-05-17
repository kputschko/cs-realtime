
# Get All Files -----------------------------------------------------------

pacman::p_load(tidyverse, feather)


# GitHub Files ------------------------------------------------------------

file_list <-
  lst(
    "https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-apr14.csv?raw=true",
    "https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-may14.csv?raw=true",
    "https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-jun14.csv?raw=true",
    "https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-jul14.csv?raw=true",
    "https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-aug14.csv?raw=true",
    "https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-sep14.csv?raw=true"
  )

last_file <- "https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-sep14.csv?raw=true"

full_file <-
  file_list %>%
  map(read_csv,
      n_max = 10,
      col_types = cols(`Date/Time` = col_datetime("%m/%d/%Y %H:%M:%S"),
                       Lat = col_double(),
                       Lon = col_double(),
                       Base = col_character())) %>%
  bind_rows()

