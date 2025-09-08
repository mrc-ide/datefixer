# Inputs

sim_result$observed_data

delay_map <- data.frame(
  from = c("onset", "onset", "onset",
           "hospitalisation", "onset", "hospitalisation"),
  to = c("report", "death", "hospitalisation",
         "discharge", "hospitalisation", "death"),
  group = I(list(1:4, 2, 3, 3, 4, 4))
)

delay_params <- data.frame(
  group = c(1:4, 2, 3, 3, 4, 4),
  from = c("onset", "onset", "onset", "onset", "onset", "onset",
           "hospitalisation", "onset", "hospitalisation"),
  to = c("report", "report", "report", "report", "death", "hospitalisation",
         "discharge", "hospitalisation", "death"),
  delay_mean = c(10, 10, 10, 10, 15, 7, 20, 7, 12),
  delay_cv = c(0.3, 0.3, 0.3, 0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
)

# Change to use distribution quantiles?
MCMC_settings <- list(init_options = list(mindelay = 0, maxdelay = 100))


#' Function to work out number of steps between events
#'
#' @example
#' delay_map <- data.frame(
#'   from = c("onset", "onset", "onset",
#'            "hospitalisation", "onset", "hospitalisation"),
#'   to = c("report", "death", "hospitalisation",
#'          "discharge", "hospitalisation", "death"),
#'   group = I(list(1:4, 2, 3, 3, 4, 4))
#' )
#'
#' calculate_transitive_steps(delay_map)
#'
calculate_transitive_steps <- function(delay_map) {

  all_events <- unique(c(delay_map$from, delay_map$to))
  event_graph <- graph_from_data_frame(delay_map[, c("from", "to")],
                                       directed = TRUE, vertices = all_events)
  # Shortest path between pairs of events
  distances_matrix <- distances(event_graph, mode = "out")

  distances_df <- as.data.frame(as.table(distances_matrix))
  names(distances_df) <- c("from", "to", "steps")
  distances_df$steps[is.infinite(distances_df$steps)] <- NA
  distances_df$from <- as.character(distances_df$from)
  distances_df$to <- as.character(distances_df$to)

  return(distances_df)
}


#' Calculate the lower and upper delay boundaries based on quantiles
#' @param delay_params The data frame of delay distribution parameters
#' @param quantile_range A vector of two probabilities (e.g. c(0.01, 0.99))
#' @return A data frame with `from`, `to`, `group`, `min_delay`, and `max_delay`
#' @example
# delay_params <- data.frame(
#   group = c(1:4, 2, 3, 3, 4, 4),
#   from = c("onset", "onset", "onset", "onset", "onset", "onset",
#            "hospitalisation", "onset", "hospitalisation"),
#   to = c("report", "report", "report", "report", "death", "hospitalisation",
#          "discharge", "hospitalisation", "death"),
#   delay_mean = c(10, 10, 10, 10, 15, 7, 20, 7, 12),
#   delay_cv = c(0.3, 0.3, 0.3, 0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
# )
# quantile_range <- c(0.01, 0.99)
# calculate_delay_boundaries(delay_params, quantile_range)
#'
calculate_delay_boundaries <- function(delay_params, quantile_range) {
  delay_params %>%
    mutate(
      shape = (1 / delay_cv)^2,
      scale = delay_mean / shape,
      # find the delay values at the specified quantiles
      min_delay = qgamma(quantile_range[1], shape = shape, scale = scale),
      max_delay = qgamma(quantile_range[2], shape = shape, scale = scale)
    ) %>%
    select(group, from, to, min_delay, max_delay)
}


