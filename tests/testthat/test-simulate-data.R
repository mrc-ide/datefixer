# Define the delay_map data frame
delay_map <- data.frame(
  from = c("onset", "onset", "onset",
           "hospitalisation", "onset", "hospitalisation"),
  to = c("report", "death", "hospitalisation",
         "discharge", "hospitalisation", "death"),
  group = I(list(
    c("community-alive", "community-dead",
      "hospitalised-alive", "hospitalised-dead"),
    "community-dead",
    "hospitalised-alive",
    "hospitalised-alive", 
    "hospitalised-dead",
    "hospitalised-dead"
  ))
)

# Define the delay parameters data frame
delay_params <- data.frame(
  group = c("community-alive", "community-dead", "hospitalised-alive",
            "hospitalised-dead", "community-dead", "hospitalised-alive",
            "hospitalised-alive", "hospitalised-dead", "hospitalised-dead"),
  from = c("onset", "onset", "onset", "onset", "onset", "onset",
           "hospitalisation", "onset", "hospitalisation"),
  to = c("report", "report", "report", "report", "death", "hospitalisation",
         "discharge", "hospitalisation", "death"),
  delay_mean = c(10, 10, 10, 10, 15, 7, 20, 7, 12),
  delay_cv = c(0.3, 0.3, 0.3, 0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
)

# Define other parameters
n_per_group <- rep(10, length(unique(delay_params$group)))
error_params <- list(prop_missing_data = 0.2, prob_error = 0.05)
date_range <- as.integer(as.Date(c("2025-03-01", "2025-09-01")))


test_that("simulate_data returns correct structure and dimensions", {
  set.seed(1)
  sim_result <- simulate_data(
    n_per_group = n_per_group,
    delay_map = delay_map,
    delay_params = delay_params,
    error_params = error_params,
    date_range = date_range,
    simul_error = TRUE
  )

  expect_type(sim_result, "list")
  expect_named(sim_result, c("true_data", "observed_data", "error_indicators"))
  
  total_n <- sum(n_per_group)
  expect_equal(nrow(sim_result$true_data), total_n)
  expect_equal(nrow(sim_result$observed_data), total_n)
  expect_equal(nrow(sim_result$error_indicators), total_n)
  
  expected_cols <- c("id", "group", "onset", "report", "death",
                     "hospitalisation", "discharge")
  expect_true(all(expected_cols %in% names(sim_result$true_data)))
  expect_true(all(expected_cols %in% names(sim_result$observed_data)))
})


test_that("error_params as expected in simulated data", {
  n_per_group <- rep(100, length(unique(delay_params$group)))
  set.seed(1)
  sim_result <- simulate_data(
    n_per_group = n_per_group,
    delay_map = delay_map,
    delay_params = delay_params,
    error_params = error_params,
    date_range = date_range,
    simul_error = TRUE
  )
  
  # just the date columns from the true data and error indicators
  date_cols <- setdiff(names(sim_result$true_data), c("id", "group"))
  true_dates <- sim_result$true_data[, date_cols]
  error_indic <- sim_result$error_indicators[, date_cols]
  
  # identify date entries that should be observed in the groups
  eligible_dates <- !is.na(true_dates)
  n_eligible_points <- sum(eligible_dates)
  eligible_indicators <- error_indic[eligible_dates]
  
  # test the proportion of missing data
  observed_prop_missing <- sum(is.na(eligible_indicators)) / n_eligible_points
  diff_missing <- observed_prop_missing - error_params$prop_missing_data
  expect_equal(diff_missing, 0, tolerance = 0.05)
  
  # test the probability of an error
  non_missing_indicators <- na.omit(eligible_indicators)
  observed_prob_error <- sum(non_missing_indicators) /
    length(non_missing_indicators)
  diff_error <- observed_prob_error - error_params$prob_error
  expect_equal(diff_error, 0, tolerance = 0.05)
})


test_that("simulate_data handles numeric groups correctly", {
  # numeric groups
  unique_groups <- unique(delay_params$group)
  group_map <- setNames(seq_along(unique_groups), unique_groups)
  
  # convert delay_map groups to numeric
  numeric_delay_map <- delay_map
  numeric_delay_map$group <- I(lapply(delay_map$group, function(g) {
    as.numeric(group_map[g])
  }))
  
  # convert delay_params groups to numeric
  numeric_delay_params <- delay_params
  numeric_delay_params$group <- as.numeric(group_map[delay_params$group])
  
  set.seed(1)
  sim_result <- simulate_data(
    n_per_group = n_per_group,
    delay_map = numeric_delay_map,
    delay_params = numeric_delay_params,
    error_params = error_params,
    date_range = date_range,
    simul_error = TRUE
  )
  
  expect_type(sim_result$true_data$group, "double")
  expect_equal(sort(unique(sim_result$true_data$group)), unname(group_map))
  expect_equal(nrow(sim_result$true_data), sum(n_per_group))
  
  # just the date columns from the true data and error indicators
  date_cols <- setdiff(names(sim_result$true_data), c("id", "group"))
  true_dates <- sim_result$true_data[, date_cols]
  error_indic <- sim_result$error_indicators[, date_cols]
  
  # identify date entries that should be observed in the groups
  eligible_dates <- !is.na(true_dates)
  n_eligible_points <- sum(eligible_dates)
  eligible_indicators <- error_indic[eligible_dates]
  
  # test the proportion of missing data
  observed_prop_missing <- sum(is.na(eligible_indicators)) / n_eligible_points
  diff_missing <- observed_prop_missing - error_params$prop_missing_data
  expect_equal(diff_missing, 0, tolerance = 0.05)
  
  # test the probability of an error
  non_missing_indicators <- na.omit(eligible_indicators)
  observed_prob_error <- sum(non_missing_indicators) /
    length(non_missing_indicators)
  diff_error <- observed_prob_error - error_params$prob_error
  expect_equal(diff_error, 0, tolerance = 0.05)
})

