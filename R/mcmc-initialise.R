# Initialise data for the MCMC

#' Function to work out number of steps between events
#' @param delay_map Data frame defining all delay rules
#' @return Data frame of the steps between each event date
#' @importFrom igraph graph_from_data_frame distances
#' @export
#' @examples
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
#' @importFrom dplyr %>% mutate select
#' @importFrom stats qgamma
#' @export
#' @examples
#' delay_params <- data.frame(
#'   group = c(1:4, 2, 3, 3, 4, 4),
#'   from = c("onset", "onset", "onset", "onset", "onset", "onset",
#'            "hospitalisation", "onset", "hospitalisation"),
#'   to = c("report", "report", "report", "report", "death", "hospitalisation",
#'          "discharge", "hospitalisation", "death"),
#'   delay_mean = c(10, 10, 10, 10, 15, 7, 20, 7, 12),
#'   delay_cv = c(0.3, 0.3, 0.3, 0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
#' )
#' quantile_range <- c(0.01, 0.99)
#' calculate_delay_boundaries(delay_params, quantile_range)
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


#' Initialise the augmented data for an individual (row)
#' @param individual_data A single row data frame for an individual
#' @param delay_map Data frame defining all delay rules
#' @param delay_boundaries Data frame with calculated min/max delays
#' @return A single row of the data frame with dates corrected and imputed
#' @importFrom igraph graph_from_data_frame all_shortest_paths
#' @importFrom dplyr inner_join
#' @importFrom stats median
#' @importFrom utils head tail
#' @export
#' @examples
#' # Inputs
#' individual_data <- data.frame(
#'   id = 1, group = 3,
#'   onset = as.Date("2025-05-01"),
#'   hospitalisation = as.Date("2025-05-02"),
#'   discharge = as.Date(NA)
#' )
#'
#' delay_map <- data.frame(
#'   from = c("onset", "hospitalisation"),
#'   to = c("hospitalisation", "discharge"),
#'   group = I(list(3, 3))
#' )
#'
#' delay_boundaries <- data.frame(
#'   group = 3,
#'   from = c("onset", "hospitalisation"),
#'   to = c("hospitalisation", "discharge"),
#'   # Set a min_delay of 3 days for onset -> hospitalisation
#'   min_delay = c(3, 7),
#'   max_delay = c(10, 20)
#' )
#'
#' result <- initialise_row(individual_data, delay_map, delay_boundaries)
#'
#' # hospitalisation identified as incompatible, incompatible date and missing
#' # date both imputed
#' result
#' # id group      onset hospitalisation  discharge
#' #  1     3 2025-05-01      2025-05-07 2025-05-20
#'
initialise_row <- function(individual_data, delay_map, delay_boundaries) {

  current_group <- individual_data$group

  group_delay_map <- delay_map[sapply(delay_map$group,
                                      function(g) current_group %in% g), ]
  group_delay_boundaries <- delay_boundaries[sapply(delay_boundaries$group,
                                                    function(g) current_group %in% g), ]
  group_dates <- unique(c(group_delay_map$from, group_delay_map$to))

  # Find all incompatible delays (direct and transitive)
  group_transitive_steps <- calculate_transitive_steps(group_delay_map)
  incompatible_events <- list()
  valid_paths <- subset(group_transitive_steps, !is.na(steps) & !steps %in% 0)

  group_graph <- graph_from_data_frame(
    group_delay_map[, c("from", "to")], directed = TRUE)

  for (i in 1:nrow(valid_paths)) {
    from_event <- valid_paths$from[i]
    to_event <- valid_paths$to[i]

    date1 <- individual_data[[from_event]]
    date2 <- individual_data[[to_event]]

    if (!is.na(date1) && !is.na(date2)) {
      # For the current path, find the direct (1-step) delays that compose it
      path_nodes_list <- all_shortest_paths(group_graph,
                                            from = from_event,
                                            to = to_event)$res

      if (length(path_nodes_list) > 0) {
        path_nodes <- names(path_nodes_list[[1]])
        direct_rules_on_path <- data.frame(from = head(path_nodes, -1),
                                           to = tail(path_nodes, -1))

        # Sum the min/max boundaries of these direct delays to get the total allowed range
        boundaries <- inner_join(direct_rules_on_path,
                                 group_delay_boundaries,
                                 by = c("from", "to"))
        if (nrow(boundaries) == nrow(direct_rules_on_path)) {
          min_allowed <- sum(boundaries$min_delay)
          max_allowed <- sum(boundaries$max_delay)

          # Check if the observed total delay is outside the allowed range
          if (as.numeric(date2 - date1) < min_allowed || as.numeric(date2 - date1) > max_allowed) {
            incompatible_events[[length(incompatible_events) + 1]] <- c(from_event, to_event)
          }
        }
      }
    }
  }

  # Remove the most problematic date
  if (length(incompatible_events) > 0) {
    problem_counts <- table(unlist(incompatible_events))
    max_problems <- max(problem_counts)
    candidates_for_removal <- names(problem_counts[problem_counts == max_problems])

    date_to_remove <- if (length(candidates_for_removal) == 1) {
      candidates_for_removal
    } else {
      # remove the most outlying
      date_values <- unlist(individual_data[1, candidates_for_removal])
      median_val <- median(date_values, na.rm = TRUE)
      outlier_idx <- which.max(abs(date_values - median_val))
      candidates_for_removal[outlier_idx]
    }

    individual_data[[date_to_remove]] <- as.Date(NA)
  }

  # Impute missing dates - find nicer way to do this?
  max_iter <- 10
  iter <- 0
  while(any(is.na(individual_data[, group_dates])) && iter < max_iter) {
    for (j in seq_len(nrow(group_delay_boundaries))) {
      from_event <- group_delay_boundaries$from[j]
      to_event <- group_delay_boundaries$to[j]
      bounds <- group_delay_boundaries[j, ]

      imputed_delay <- floor(median(c(bounds$min_delay, bounds$max_delay)))

      if (!is.na(individual_data[[from_event]]) && is.na(individual_data[[to_event]])) {
        individual_data[[to_event]] <- individual_data[[from_event]] + imputed_delay
      } else if (is.na(individual_data[[from_event]]) && !is.na(individual_data[[to_event]])) {
        individual_data[[from_event]] <- individual_data[[to_event]] - imputed_delay
      }
    }
    iter <- iter + 1
  }

  individual_data[, group_dates] <- individual_data[, group_dates] + 0.5
  
  individual_data
}

#' Initialises augmented data based on observed data
#'
#' @param observed_data A data frame of observed dates
#' @param delay_map A data frame defining the delay rules
#' @param delay_params A data frame with delay distribution parameters
#' @param init_settings A list containing `quantile_range` (e.g. c(0.01, 0.99))
#'
#' @return A list with two data frames: `augmented_data` and `error_indicators`
#' @export
#' @importFrom dplyr %>% group_by group_modify case_when
#' @importFrom generics setdiff
#' @examples
#' # Define the delay_map data frame
#' delay_map <- data.frame(
#'   from = c("onset", "onset", "onset",
#'            "hospitalisation", "onset", "hospitalisation"),
#'   to = c("report", "death", "hospitalisation",
#'          "discharge", "hospitalisation", "death"),
#'   group = I(list(1:4, 2, 3, 3, 4, 4))
#' )
#'
#' # Define the delay parameters data frame
#' delay_params <- data.frame(
#'   group = c(1:4, 2, 3, 3, 4, 4),
#'   from = c("onset", "onset", "onset", "onset", "onset", "onset",
#'            "hospitalisation", "onset", "hospitalisation"),
#'   to = c("report", "report", "report", "report", "death", "hospitalisation",
#'          "discharge", "hospitalisation", "death"),
#'   delay_mean = c(10, 10, 10, 10, 15, 7, 20, 7, 12),
#'   delay_cv = c(0.3, 0.3, 0.3, 0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
#' )
#'
#' # Define other parameters
#' n_per_group <- rep(10, max(delay_params$group))
#' error_params <- list(prop_missing_data = 0.2, prob_error = 0.05)
#' range_dates <- as.integer(as.Date(c("2025-03-01", "2025-09-01")))
#'
#' # Simulate data
#' set.seed(1)
#' sim_result <- simulate_data(
#'   n_per_group = n_per_group,
#'   delay_map = delay_map,
#'   delay_params = delay_params,
#'   error_params = error_params,
#'   range_dates = range_dates,
#'   simul_error = TRUE
#' )
#'
#' obs_dat <- sim_result$observed_data
#'
#' mcmc_init_settings <- list(quantile_range = c(0.01, 0.99))
#'
#' aug_dat <- initialise_augmented_data(
#'   observed_data = obs_dat,
#'   delay_map = delay_map,
#'   delay_params = delay_params,
#'   init_settings = mcmc_init_settings
#' )
#'
#' # Compare data to original
#' obs_dat
#' aug_dat$augmented_data
#' aug_dat$error_indicators
#'
initialise_augmented_data <- function(model, control) {
  
  observed_dates <- model$observed_dates
  groups <- model$groups
  delay_map <- model$delays
  delay_params <- model$delays
  delay_params$delay_mean <- 7
  delay_params$delay_cv <- 0.25
  init_settings <- list(quantile_range = c(control$lower_quantile,
                                           control$upper_quantile))
  
  
  delay_boundaries <- calculate_delay_boundaries(delay_params,
                                                 init_settings$quantile_range)

  # Initialise each individual row
  true_dates <- observed_dates
  true_dates$group <- groups
  true_dates$id <- seq_len(nrow(true_dates))
  true_dates <- true_dates %>%
    group_by(id) %>%
    group_modify(~ initialise_row(.x, delay_map, delay_boundaries))

  # Keep as tibble or covert to data frame?
  true_dates <- as.data.frame(true_dates)
  date_cols <- names(observed_dates)
  true_dates <- true_dates[, date_cols]

  # Create the error indicators
  error_indicators <- as.data.frame(observed_dates != floor(true_dates))
  
  model$true_dates <- true_dates
  model$error_indicators <- error_indicators
  
  model
}
