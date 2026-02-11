# Set up simulated data for expected inputs

#' Simulate a dataset with true dates, observed dates and error indicators
#'
#' @param n_per_group Vector of number of individuals to simulate in each group.
#' @param group_names A character or numeric vector of names for the groups
#'  being simulated.
#' @param delay_map A data frame that defines the delays between events. It must
#'   contain the columns `from` (character), `to` (character), and `group` (list
#'   of numeric or character group IDs).
#' @param delay_params A data frame containing the parameters (`mean_delay`,
#'  `cv_delay`) for each delay. Consider combining delay_map and delay_params?
#' @param error_params A list containing `prop_missing_data` and `prob_error`.
#' @param date_range A vector of two integer dates for the simulation range.
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
#'   group = I(list(
#'     c("community-alive", "community-dead",
#'       "hospitalised-alive", "hospitalised-dead"),
#'     "community-dead",
#'     "hospitalised-alive",
#'     "hospitalised-alive", 
#'     "hospitalised-dead",
#'     "hospitalised-dead"
#'   ))
#' )
#' 
#' # Define the delay parameters data frame
#' delay_params <- data.frame(
#'   group = c("community-alive", "community-dead", "hospitalised-alive",
#'             "hospitalised-dead", "community-dead", "hospitalised-alive",
#'             "hospitalised-alive", "hospitalised-dead", "hospitalised-dead"),
#'   from = c("onset", "onset", "onset", "onset", "onset", "onset",
#'            "hospitalisation", "onset", "hospitalisation"),
#'   to = c("report", "report", "report", "report", "death", "hospitalisation",
#'          "discharge", "hospitalisation", "death"),
#'   delay_mean = c(10, 10, 10, 10, 15, 7, 20, 7, 12),
#'   delay_cv = c(0.3, 0.3, 0.3, 0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
#' )
#'
#' # Define other parameters
#' n_per_group <- rep(10, length(unique(delay_params$group)))
#' group_names <- c("community-alive", "community-dead", "hospitalised-alive",
#'             "hospitalised-dead")
#' error_params <- list(prop_missing_data = 0.2, prob_error = 0.05)
#' date_range <- as.integer(as.Date(c("2025-03-01", "2025-09-01")))
#'
#' # Run simulation
#' set.seed(1)
#' sim_result <- simulate_data(
#'   n_per_group = n_per_group,
#'   group_names = group_names,
#'   delay_map = delay_map,
#'   delay_params = delay_params,
#'   error_params = error_params,
#'   date_range = date_range
#' )
#'
#' sim_result$true_data
#' sim_result$observed_data
#' sim_result$error_indicators # true error indicators
#'
simulate_data <- function(n_per_group,
                          group_names,
                          delay_map,
                          delay_params,
                          error_params,
                          date_range) {
  
  true_data <- simulate_true_data(n_per_group, group_names, delay_map,
                                  delay_params, date_range)
  
  add_observation_errors(true_data, error_params, date_range)
  
}


#' Simulate true data
#' @description Simulate the true, unobserved dates for individuals
#' @inheritParams simulate_data
#' @importFrom stats rgamma
#' @importFrom igraph graph_from_data_frame topo_sort degree
#' @export
simulate_true_data <- function(n_per_group, group_names,
                               delay_map, delay_params, date_range) {
  
  # Handle single n_per_group for all groups
  if (length(n_per_group) == 1) {
    n_per_group <- rep(n_per_group, length(group_names))
  }
  
  if (length(n_per_group) != length(group_names)) {
    cli::cli_abort(
      c("Lengths of 'n_per_group' and 'group_names' do not match",
        i = "length of 'n_per_group' is {squote(length(n_per_group))}",
        x = "length of 'group_names' is {squote(length(group_names))}"))
  }
  
  groups_delay_map <- sort(unique(unlist(delay_map$group)))
  is_same_groups <- length(group_names) == length(groups_delay_map) &&
    all(group_names == groups_delay_map)
  if (!is_same_groups) {
    cli::cli_abort(
      c("Groups in 'group_names' do not match those in 'delay_map'",
        i = "'data' has: {squote(group_names)}",
        x = "'delay_map' has: {squote(groups_delay_map)}"))
  }
  
  total_indiv <- sum(n_per_group)
  all_event_names <- unique(c(delay_map$from, delay_map$to))
  
  true_data <- data.frame(id = 1:total_indiv,
                          group = rep(group_names, times = n_per_group),
                          stringsAsFactors = FALSE)
  
  for (name in all_event_names) true_data[[name]] <- NA
  
  # Simulate the true dates for each individual
  for (i in seq_len(total_indiv)) {
    current_group <- true_data$group[i]
    
    # Filter the delay map to find rules applicable to the current group
    applicable_delays <-
      delay_map[sapply(delay_map$group, function(g) current_group %in% g), ]
    
    events_in_group <- unique(c(applicable_delays$from, applicable_delays$to))
    event_graph <- graph_from_data_frame(applicable_delays[, c("from", "to")],
                                         directed = TRUE,
                                         vertices = events_in_group)
    event_order <- names(topo_sort(event_graph))
    root_events <- names(which(degree(event_graph, mode = "in") == 0))
    
    valid_dates <- FALSE
    attempts <- 0
    max_attempts <- 100
    
    while (!valid_dates && attempts < max_attempts) {
      attempts <- attempts + 1
    
      event_cols <- unique(c(delay_map$from, delay_map$to))
      
      proposed_dates <- unlist(true_data[i, event_cols])

    # Simulate root events ("from" events)
    for (root in root_events) {
      proposed_dates[root] <- sample(date_range[1]:date_range[2], 1) + runif(1)
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
      proposed_dates[to_event] <- proposed_dates[from_event] + delay
    }
    
    # Check all simulated true dates for an individual are within date_range
    dates_to_check <- proposed_dates[events_in_group]
    valid_dates <- all(!is.na(dates_to_check) &
                         dates_to_check >= date_range[1] &
                         dates_to_check <= date_range[2])
    
    }
    
    if (attempts >= max_attempts) {
      stop("Could not generate valid true dates after 100 attempts.
           Consider widening date_range or reducing delays.")
    }
    
    true_data[i, events_in_group] <- proposed_dates[events_in_group]
    
  }
  
  true_data
}


#' Add observation errors
#' @description Simulate observed data incorporating observation error from true
#'    data
#' @inheritParams simulate_data
#' @export
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
  
  list(true_data = true_data,
       observed_data = observed_data,
       error_indicators = error_indicators)
}


