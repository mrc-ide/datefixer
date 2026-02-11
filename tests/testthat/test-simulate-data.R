
test_that("simulate_data returns correct structure and dimensions", {
  params <- toy_model_params()
  
  set.seed(1)
  sim_result <- simulate_data(
    n_per_group = params$n_per_group,
    group_names = params$group_names,
    delay_map = params$delay_map,
    delay_params = params$delay_params,
    error_params = params$error_params,
    date_range = params$date_range
  )

  expect_type(sim_result, "list")
  expect_named(sim_result, c("true_data", "observed_data", "error_indicators"))
  
  total_n <- sum(params$n_per_group)
  expect_equal(nrow(sim_result$true_data), total_n)
  expect_equal(nrow(sim_result$observed_data), total_n)
  expect_equal(nrow(sim_result$error_indicators), total_n)
  
  expected_cols <- c("id", "group", "onset", "report", "death",
                     "hospitalisation", "discharge")
  expect_true(all(expected_cols %in% names(sim_result$true_data)))
  expect_true(all(expected_cols %in% names(sim_result$observed_data)))
})


test_that("error_params as expected in simulated data", {
  params <- toy_model_params()
  n_per_group <- rep(100, length(params$group_names))
  
  set.seed(1)
  sim_result <- simulate_data(
    n_per_group = n_per_group,
    group_names = params$group_names,
    delay_map = params$delay_map,
    delay_params = params$delay_params,
    error_params = params$error_params,
    date_range = params$date_range
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
  diff_missing <- observed_prop_missing - params$error_params$prop_missing_data
  expect_equal(diff_missing, 0, tolerance = 0.05)
  
  # test the probability of an error
  non_missing_indicators <- na.omit(eligible_indicators)
  observed_prob_error <- sum(non_missing_indicators) /
    length(non_missing_indicators)
  diff_error <- observed_prob_error - params$error_params$prob_error
  expect_equal(diff_error, 0, tolerance = 0.05)
})


test_that("simulate_data handles numeric groups correctly", {
  params <- toy_model_params(named_groups = FALSE)
  
  set.seed(1)
  sim_result <- simulate_data(
    n_per_group = params$n_per_group,
    group_names = params$group_names,
    delay_map = params$delay_map,
    delay_params = params$delay_params,
    error_params = params$error_params,
    date_range = params$date_range
  )
  
  expect_type(sim_result$true_data$group, "integer")
  expect_equal(sort(unique(sim_result$true_data$group)), params$group_names)
  expect_equal(nrow(sim_result$true_data), sum(params$n_per_group))
  
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
  diff_missing <- observed_prop_missing - params$error_params$prop_missing_data
  expect_equal(diff_missing, 0, tolerance = 0.05)
  
  # test the probability of an error
  non_missing_indicators <- na.omit(eligible_indicators)
  observed_prob_error <- sum(non_missing_indicators) /
    length(non_missing_indicators)
  diff_error <- observed_prob_error - params$error_params$prob_error
  expect_equal(diff_error, 0, tolerance = 0.05)
})

