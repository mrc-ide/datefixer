# Initialise data for the MCMC
#' @importFrom igraph graph_from_data_frame distances
calculate_transitive_steps <- function(delay_map) {

  all_events <- unique(c(delay_map$from, delay_map$to))
  event_graph <- graph_from_data_frame(delay_map[, c("from", "to")],
                                       directed = TRUE, vertices = all_events)
  # Shortest path between pairs of events
  distances_matrix <- distances(event_graph, mode = "out")

  distances_df <- as.data.frame(as.table(distances_matrix))
  names(distances_df) <- c("from", "to", "steps")
  distances_df$from <- as.character(distances_df$from)
  distances_df$to <- as.character(distances_df$to)
  
  # remove 0 steps and dates with no connection
  distances_df <- subset(distances_df, steps != 0 & !is.infinite(steps))
  row.names(distances_df) <- NULL

  return(distances_df)
}


# Calculate the lower and upper delay boundaries based on quantiles
#' @importFrom dplyr %>% mutate select
#' @importFrom stats qgamma
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


# Initialise the augmented data for an individual (row)
#' @importFrom igraph graph_from_data_frame all_shortest_paths
#' @importFrom dplyr inner_join
#' @importFrom stats median
#' @importFrom utils head tail
initialise_row <- function(individual_data, delay_map, delay_boundaries, rng) {
  
  current_group <- individual_data$group

  group_delay_map <- delay_map[sapply(delay_map$group,
                                      function(g) current_group %in% g), ]
  group_delay_boundaries <- delay_boundaries[sapply(delay_boundaries$group,
                                                    function(g) current_group %in% g), ]
  group_dates <- unique(c(group_delay_map$from, group_delay_map$to))

  # Find all incompatible delays (direct and transitive)
  valid_paths <- calculate_transitive_steps(group_delay_map)
  incompatible_events <- list()

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

  individual_data[, group_dates] <- individual_data[, group_dates] + 
    monty::monty_random_n_real(length(group_dates), rng)
  
  individual_data
}

# Initialises augmented data based on observed data
#' @importFrom dplyr %>% group_by group_modify case_when
#' @importFrom generics setdiff
initialise_augmented_data <- function(model, control, rng) {
  
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
    group_modify(~ initialise_row(.x, delay_map, delay_boundaries, rng))

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
