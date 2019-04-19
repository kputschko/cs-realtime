
# Plotly Testing ----------------------------------------------------------

one_plot <- function(d) {
  plot_ly(d, x = ~price) %>%
    add_annotations(
      ~unique(clarity), x = 0.5, y = 1,
      xref = "paper", yref = "paper", showarrow = FALSE
    )
}

diamonds %>%
  split(.$clarity) %>%
  lapply(one_plot) %>%
  subplot(nrows = 2, shareX = TRUE, titleX = FALSE) %>%
  hide_legend()


# Histogram ---------------------------------------------------------------


fx_plotly_helper <- function(data) {
  data %>%
    plot_ly(x = ~distance) %>%
    add_histogram(histnorm = "probability", nbinsx = 100) %>%
    add_annotations(~unique(Hub), x = 0.50, y = 1, xref = "paper", yref = "paper", showarrow = FALSE)
}



data_new %>%
  mutate(Hub_1 = Hub) %>%
  nest(-Hub_1) %>%
  mutate(myplot = map(data, fx_plotly_helper)) %>%
  pull(myplot) %>%
  subplot(nrows = 5, shareX = TRUE, shareY = TRUE)



# Density Plot ------------------------------------------------------------


t <-
  data_new %>%
  nest(-Hub) %>%
  mutate(
    color = vlookup(Hub, plot_cluster_centers, "Hub", "color"),
    dens = map(data, ~density(.x[["distance"]])),
    dens_df = map(dens, ~tibble(x = .x$x, y = .x$y)),
    dens_prop = map(dens_df, ~ .x %>% mutate(yp = y / sum(y))),
    plot = map2(dens_prop, color, ~ .x %>% plot_ly(x = ~x, y = ~yp, fill = "tozeroy", fillcolor = toRGB(.y)) %>% add_lines(line = list(color = "black"))))

t$dens_prop[[1]]

t$dens_df[[1]]$y %>% summary()
t$dens_prop[[1]]$yp %>% summary()

t$plot[[1]]
t$plot[[2]]
t$plot[[3]]
t$plot[[4]]

t %>% pull(plot) %>% subplot(nrows = 5, shareX = TRUE, shareY = TRUE)
