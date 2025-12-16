# Set up simulated data for expected inputs

#' Simulate a dataset with true dates, observed dates and error indicators
#'
#' @param n_per_group Vector of number of individuals to simulate in each group.
#' @param delay_map A data frame that defines the delays between events. It must
#'   contain the columns `from` (character), `to` (character), and `group` (list
#'   of numeric group IDs).
#' @param delay_params A data frame containing the parameters (`delay_mean`,
#'  `delay_cv`) for each delay. Consider combining delay_map and delay_params?
#' @param error_params A list containing `prop_missing_data` and `prob_error`.
#' @param date_range A vector of two integer dates for the simulation range.
#' @param simul_error Boolean. If TRUE, simulates missing and erroneous data.
#'
#' @importFrom igraph topo_sort graph_from_data_frame degree
#' @import dplyr
#' @importFrom purrr map_dfr
#' @importFrom stats rgamma
#'
#' @return A list with three data frames: `true_data`, `observed_data`, and
#'   `error_indicators`, each including `id` and `group` columns.
#'
#' @export
#'
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
#' date_range <- as.integer(as.Date(c("2025-03-01", "2025-09-01")))
#'
#' # Run simulation
#' set.seed(1)
#' sim_result <- simulate_data(
#'   n_per_group = n_per_group,
#'   delay_map = delay_map,
#'   delay_params = delay_params,
#'   error_params = error_params,
#'   date_range = date_range,
#'   simul_error = TRUE
#' )
#'
#' sim_result$true_data
#' sim_result$observed_data
#' sim_result$error_indicators # true error indicators
#'
simulate_data <- function(n_per_group,
                          delay_map,
                          delay_params,
                          error_params,
                          date_range,
                          simul_error = FALSE,
                          true_data = NA) {
  
  if (is.na(true_data)) {
  true_data <- simulate_true_data(n_per_group, delay_map,
                                  delay_params, date_range)
  }
  
  # Simulate observation errors
  observed_data <- NULL
  error_indicators <- NULL

  if (simul_error) {
    error_results <- add_observation_errors(true_data,
                                            error_params,
                                            date_range)
    observed_data <- error_results$observed_data
    error_indicators <- error_results$error_indicators
  }

    if (!simul_error) {
      # If no error simulation, 'observed_data' is just the rounded 'true_data'
      observed_data <- true_data
      date_cols <- setdiff(names(observed_data), c("id", "group"))
      
      for (col in date_cols) {
        observed_data[[col]] <- as.Date(floor(true_data[[col]]),
                                        origin = "1970-01-01")
      }
    }

  return(list(
    true_data = true_data,
    observed_data = observed_data,
    error_indicators = error_indicators
  ))
}


#' Simulate true data
#' @description Simulate the true, unobserved dates for individuals
#' @inheritParams simulate_data
#' @importFrom stats rgamma
#' @importFrom igraph graph_from_data_frame topo_sort degree
#' @export
simulate_true_data <- function(n_per_group, delay_map, delay_params, date_range) {

  n_groups <- length(n_per_group)
  total_indiv <- sum(n_per_group)
  
  all_event_names <- unique(c(delay_map$from, delay_map$to))
  true_data <- data.frame(id = 1:total_indiv,
                          group = rep(1:n_groups, times = n_per_group))
  
  for (name in all_event_names) true_data[[name]] <- NA
  
  # Simulate the true dates for each individual
  for (i in seq_len(total_indiv)) {
    current_group <- true_data$group[i]
    
    # Filter the delay map to find rules applicable to the current group
    applicable_delays <-
      delay_map[sapply(delay_map$group, function(g) current_group %in% g), ]
    
    events_in_group <- unique(c(applicable_delays$from, applicable_delays$to))
    event_graph <- graph_from_data_frame(
      applicable_delays[, c("from", "to")],
      directed = TRUE,
      vertices = events_in_group
    )
    event_order <- names(topo_sort(event_graph))
    
    # Simulate root events ("from" events)
    root_events <- names(which(degree(event_graph, mode = "in") == 0))
    for (root in root_events) {
      true_data[i, root] <- sample(date_range[1]:date_range[2], 1) + runif(1)
    }
    
    for (to_event in setdiff(event_order, root_events)) {
      rule <- subset(applicable_delays, to == to_event)
      from_event <- rule$from
      params <- subset(
        delay_params,
        from == from_event & to == to_event & group == current_group
      )
      
      # Sample the delay
      shape <- (1 / params$delay_cv)^2
      rate <- shape / params$delay_mean
      delay <- rgamma(1, shape = shape, rate = rate)
      true_data[i, to_event] <- true_data[i, from_event] + delay
    }
  }
  return(true_data)
}


## Create error_indicators using error_params
add_observation_errors <- function(true_data, error_params, date_range) {

  observed_data <- true_data
  error_indicators <- true_data
  date_cols <- setdiff(names(true_data), c("id", "group"))
  
  error_indicators[date_cols] <- lapply(
    error_indicators[date_cols], function(x) rep(as.logical(NA), length(x))
  )

  # Convert true_data continuous dates to observed dates
  for (col in date_cols) {
    observed_data[[col]] <- as.Date(floor(true_data[[col]]),
                                    origin = "1970-01-01")
  }
  
  p_miss <- error_params$prop_missing_data
  p_error <- error_params$prob_error
  
  probs <- c(p_miss, (1 - p_miss) * p_error, (1 - p_miss) * (1 - p_error))

  n_indiv <- nrow(true_data)
  
  # Apply errors by individual
  for (i in seq_len(n_indiv)) {
    
    indiv_true_dates <- unlist(true_data[i, date_cols])
    exists_idx <- which(!is.na(indiv_true_dates))
    n_dates_to_observe <- length(exists_idx)
    
    error_type <- sample(c(NA, TRUE, FALSE),
                         n_dates_to_observe,
                         replace = TRUE,
                         prob = probs)
    
    # Handle situations where all dates for an individual are missing
    if (all(is.na(error_type))) {
      
      # Randomly select one index from existing dates to force as observed
      force_observed_idx <- sample(seq_len(n_dates_to_observe), 1)
      observed_probs <- probs[2:3] / sum(probs[2:3])
      error_type[force_observed_idx] <- sample(c(TRUE, FALSE), 1,
                                               prob = observed_probs)
      
    }
    
    error_indicators[i, date_cols[exists_idx]] <- error_type
    
    missing_idx <- which(is.na(error_type))
    error_idx   <- which(error_type)
    
    if (length(missing_idx) > 0) {
      observed_data[i, date_cols[exists_idx[missing_idx]]] <- NA
    }
    
    if (length(error_idx) > 0) {
      n_errors <- length(error_idx)
      col_indices_for_errors <- exists_idx[error_idx]
      true_dates_for_errors <- floor(indiv_true_dates[exists_idx[error_idx]])
      proposed_dates <- sample(date_range[1]:date_range[2], n_errors, replace = TRUE)
      
      # Find any resampled dates for errors which match the true date
      invalid_error <- which(proposed_dates == true_dates_for_errors)
      
      while(length(invalid_error) > 0) {
        new_sample <- sample(date_range[1]:date_range[2],
                             length(invalid_error), replace = TRUE)
        proposed_dates[invalid_error] <- new_sample
        invalid_error <- which(proposed_dates[invalid_error] ==
                                 true_dates_for_errors[invalid_error])
      }
      
      # Update the observed data for the columns that had errors
      cols_to_update <- date_cols[col_indices_for_errors]
      observed_data[i, cols_to_update] <- as.Date(proposed_dates,
                                                  origin = "1970-01-01")
    }
  }
  
  return(list(observed_data = observed_data,
              error_indicators = error_indicators))
}


