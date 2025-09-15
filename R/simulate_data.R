# Set up simulated data for expected inputs

#' Simulate a dataset with true dates, observed dates and error indicators
#'
#' @param n_per_group Vector of number of individuals to simulate in each group.
#' @param delay_map A data frame that defines the delays between events. It must
#'   contain the columns `from` (character), `to` (character), and `group` (list
#'   of numeric group IDs).
#' @param delay_params A data frame containing the parameters (`mean_delay`,
#'  `cv_delay`) for each delay. Consider combining delay_map and delay_params?
#' @param error_params A list containing `prop_missing_data` and `prob_error`.
#' @param range_dates A vector of two integer dates for the simulation range.
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
#'   mean_delay = c(10, 10, 10, 10, 15, 7, 20, 7, 12),
#'   cv_delay = c(0.3, 0.3, 0.3, 0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
#' )
#'
#' # Define other parameters
#' n_per_group <- rep(10, max(delay_params$group))
#' error_params <- list(prop_missing_data = 0.2, prob_error = 0.05)
#' range_dates <- as.integer(as.Date(c("2025-03-01", "2025-09-01")))
#'
#' # Run simulation
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
#' sim_result$true_data
#' sim_result$observed_data
#' sim_result$error_indicators # true error indicators
#'
simulate_data <- function(n_per_group,
                          delay_map,
                          delay_params,
                          error_params,
                          range_dates,
                          simul_error = FALSE) {

  # Simulate 20% more individuals per group than needed in case of all-NA rows
  n_to_sim <- n_per_group * 1.2

  n_groups <- length(n_to_sim)
  total_indiv <- sum(n_to_sim)

  # Initialise the data frame for true data
  all_event_names <- unique(c(delay_map$from, delay_map$to))
  true_data <- data.frame(id = 1:total_indiv,
                          group = rep(1:n_groups, times = n_to_sim))

  for (name in all_event_names) true_data[[name]] <- as.Date(NA)

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

    # Simulate root events ("from" events)
    root_events <- names(which(degree(event_graph, mode = "in") == 0))
    for (root in root_events) {
      true_data[i, root] <- as.Date(
        sample(range_dates[1]:range_dates[2], 1), origin = "1970-01-01"
      )
    }

    for (to_event in setdiff(event_order, root_events)) {
      rule <- subset(applicable_delays, to == to_event)
      from_event <- rule$from
      params <- subset(
        delay_params,
        from == from_event & to == to_event & group == current_group
      )

      # Sample the delay
      shape <- (1 / params$cv_delay)^2
      scale <- params$mean_delay / shape
      delay <- pmax(0, round(rgamma(1, shape = shape, scale = scale)))
      true_data[i, to_event] <- true_data[i, from_event] + delay
    }
  }

  # Simulate observation errors
  observed_data <- NULL
  error_indicators <- NULL

  if (simul_error) {
    error_results <- add_observation_errors(true_data,
                                            error_params,
                                            range_dates)
    observed_data <- error_results$observed_data
    error_indicators <- error_results$error_indicators

    # Remove individuals where entire row is NA
    date_cols <- setdiff(names(observed_data), c("id", "group"))
    rows_to_keep <- which(rowSums(!is.na(observed_data[, date_cols])) > 0)

    valid_rows <- observed_data[rows_to_keep, ]

    final_data <- valid_rows %>%
      group_by(group) %>%
      group_split() %>%
      map_dfr(~ slice_sample(.x, n = n_per_group[.x$group[1]]))

    final_ids <- final_data$id

    true_data <- true_data[true_data$id %in% final_ids, ]
    observed_data <- observed_data[observed_data$id %in% final_ids, ]
    error_indicators <- error_indicators[error_indicators$id %in% final_ids, ]

  } else {
    # Remove excess and update ids
    true_data <- true_data %>%
      group_by(group) %>%
      group_split() %>%
      map_dfr(~ slice_sample(.x, n = n_per_group[.x$group[1]]))
  }

  true_data$id <- seq_len(nrow(true_data))
  rownames(true_data) <- NULL
  if (!is.null(observed_data)) {
    observed_data$id <- seq_len(nrow(observed_data))
    error_indicators$id <- seq_len(nrow(error_indicators))
    rownames(observed_data) <- NULL
    rownames(error_indicators) <- NULL
  }

  return(list(
    true_data = true_data,
    observed_data = observed_data,
    error_indicators = error_indicators
  ))
}

## Create error_indicators using error_params
add_observation_errors <- function(true_data, error_params, range_dates) {

  observed_data <- true_data
  error_indicators <- true_data
  date_cols <- setdiff(names(true_data), c("id", "group"))
  error_indicators[date_cols] <- lapply(
    error_indicators[date_cols], function(x) rep(as.logical(NA), length(x))
  )

  probs <- c(
    error_params$prop_missing_data,
    (1 - error_params$prop_missing_data) * error_params$prob_error,
    (1 - error_params$prop_missing_data) * (1 - error_params$prob_error)
  )

  for (col in date_cols) {
    # Identify dates that are supposed to be recorded
    valid_idx <- which(!is.na(true_data[[col]]))
    if (length(valid_idx) == 0) next

    error_type <- sample(c(NA, TRUE, FALSE),
                         length(valid_idx), replace = TRUE, prob = probs)

    error_indicators[[col]][valid_idx] <- error_type

    missing_idx <- valid_idx[which(is.na(error_type))]
    error_idx   <- valid_idx[which(error_type)]

    if (length(missing_idx) > 0) {
      observed_data[missing_idx, col] <- NA
    }

    if (length(error_idx) > 0) {
      n_errors <- length(error_idx)
      true_dates_for_errors <- as.integer(true_data[error_idx, col])

      proposed_dates <- sample(
        range_dates[1]:range_dates[2], n_errors, replace = TRUE
        )

      # Find any resampled dates for errors which match the true date
      invalid_error <- which(proposed_dates == true_dates_for_errors)

      # Fix these invalid errors
      while(length(invalid_error) > 0) {
        new_sample <- sample(range_dates[1]:range_dates[2],
                             length(invalid_error), replace = TRUE)
        proposed_dates[invalid_error] <- new_sample
        invalid_error <- which(proposed_dates[invalid_error] ==
                                 true_dates_for_errors[invalid_error])
      }

      observed_data[error_idx, col] <- as.Date(proposed_dates,
                                               origin = "1970-01-01")
    }
  }

  return(list(observed_data = observed_data,
              error_indicators = error_indicators))
}
