# test calculate_transitive_steps()

test_that("calculate_transitive_steps works correctly", {
  
  delay_map <- data.frame(
    from = c("onset", "onset", "onset", "hospitalisation", "onset",
             "hospitalisation"),
    to = c("report", "death", "hospitalisation", "discharge", "hospitalisation",
           "death"),
    group = I(list(1:4, 2, 3, 3, 4, 4))
  )
  
  expected_output <- data.frame(
    from = c("onset", "onset", "onset", "hospitalisation", "onset",
             "hospitalisation"),
    to = c("hospitalisation", "report", "death", "death", "discharge",
           "discharge"),
    steps = c(1, 1, 1, 1, 2, 1)
  )
  
  actual_output <- calculate_transitive_steps(delay_map)
  
  expect_equal(actual_output, expected_output)
  
})

# test calculate_delay_boundaries()

test_that("calculate_delay_boundaries estimates correct quantiles", {

delay_params <- data.frame(
  group = c(1:4, 2, 3, 3, 4, 4),
  from = c("onset", "onset", "onset", "onset", "onset", "onset",
           "hospitalisation", "onset", "hospitalisation"),
  to = c("report", "report", "report", "report", "death", "hospitalisation",
         "discharge", "hospitalisation", "death"),
  delay_mean = c(10, 10, 10, 10, 15, 7, 20, 7, 12),
  delay_cv = c(0.3, 0.3, 0.3, 0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
)

control <- mcmc_control()
quantile_range <- c(control$lower_quantile, control$upper_quantile)

shape <- (1 / delay_params$delay_cv)^2
scale <- delay_params$delay_mean / shape
exp_min <- qgamma(quantile_range[1], shape = shape, scale = scale)
exp_max <- qgamma(quantile_range[2], shape = shape, scale = scale)

actual <- calculate_delay_boundaries(delay_params, quantile_range)
act_min <- actual$min_delay
act_max <- actual$max_delay

expect_equal(exp_min, act_min)
expect_equal(exp_max, act_max)

})

# test initialise_row()

test_that("rows initialised correctly when there is 1 error and 1 missing", {

individual_data <- data.frame(
  id = 1, group = 3,
  onset = as.Date("2025-05-01"),
  hospitalisation = as.Date("2025-05-02"),
  discharge = as.Date(NA)
)

delay_map <- data.frame(
  from = c("onset", "hospitalisation"),
  to = c("hospitalisation", "discharge"),
  group = I(list(3, 3))
)

delay_params <- data.frame(
  group = 3,
  from = c("onset", "hospitalisation"),
  to = c("hospitalisation", "discharge"),
  delay_mean = 7,
  delay_cv = 0.25
)

control <- mcmc_control()
quantile_range <- c(control$lower_quantile, control$upper_quantile)

# min_delay = 3.579235, max_delay = 11.70001
# onset -> hospitalisation too short
delay_boundaries <- calculate_delay_boundaries(delay_params,
                                               quantile_range)

rng <- monty::monty_rng_create(1)

result <- initialise_row(individual_data, delay_map, delay_boundaries, rng)

# hospitalisation should be identified as incompatible, incompatible date and
# missing date of discharge should both be imputed
expect_true(!is.na(result$discharge))
expect_true(result$hospitalisation != individual_data$hospitalisation)
# need tolerance because dates in result are continuous - unclass(result$onset)
expect_equal(result$onset, individual_data$onset, tolerance = 0.99)

})

test_that("rows are initialised correctly in a complex example", {
  
  # onset -> report
  # onset -> hospitalisation -> death
  delay_map <- data.frame(
    from = c("onset", "onset", "hospitalisation"),
    to = c("report", "hospitalisation", "death"),
    group = I(list(4, 4, 4))
  )
  
  delay_params <- data.frame(
    group = 4,
    from = c("onset", "onset", "hospitalisation"),
    to = c("report", "hospitalisation", "death"),
    delay_mean = 7,
    delay_cv = 0.25
  )
  
  control <- mcmc_control()
  quantile_range <- c(control$lower_quantile, control$upper_quantile)
  
  # min_delay = 3.579235, max_delay = 11.70001
  delay_boundaries <- calculate_delay_boundaries(delay_params,
                                                 quantile_range)
  
  rng <- monty::monty_rng_create(1)
  
  # 1. onset date involved in 2 defined problematic delays
  # onset -> hospitalisation too long (12 days)
  # onset -> report too long (12 days)
  individual_data_1 <- data.frame(
    id = 34, group = 4,
    onset = as.Date("2025-07-03"),
    hospitalisation = as.Date("2025-07-15"),
    report = as.Date("2025-07-15"),
    death = as.Date("2025-07-25"),
    discharge = as.Date(NA)
  )
  
  # expect onset date to be changed
  res_1 <- initialise_row(individual_data_1, delay_map, delay_boundaries, rng)
  expect_true(res_1$onset != individual_data_1$onset)
  
  # 2. no problematic delays = no change
  individual_data_2 <- data.frame(
    id = 34, group = 4,
    onset = as.Date("2025-07-07"),
    hospitalisation = as.Date("2025-07-15"),
    report = as.Date("2025-07-15"),
    death = as.Date("2025-07-25"),
    discharge = as.Date(NA)
  )

  # expect no change - need tolerance to account for continuous dates
  res_2 <- initialise_row(individual_data_2, delay_map, delay_boundaries, rng)
  expect_equal(res_2, individual_data_2, tolerance = 0.99)
  
  # 3. two problematic delays: defined and transitive
  # hospitalisation -> death too short and
  # transitive delay between onset -> death = 2* 3.579235 = 7.16, so
  # onset -> death is also too short
  individual_data_3 <- data.frame(
    id = 34, group = 4,
    onset = as.Date("2025-07-04"),
    hospitalisation = as.Date("2025-07-10"),
    report = as.Date("2025-07-12"),
    death = as.Date("2025-07-11"),
    discharge = as.Date(NA)
  )
  
  res_3 <- initialise_row(individual_data_3, delay_map, delay_boundaries, rng)
  expect_true(res_3$death != individual_data_3$death)
  
  # 4. two unrelated problematic delays
  # onset -> report too long (13 days)
  # hospitalisation -> death too short (1 day)
  individual_data_4 <- data.frame(
    id = 34, group = 4,
    onset = as.Date("2025-07-03"),
    hospitalisation = as.Date("2025-07-10"),
    report = as.Date("2025-07-16"),
    death = as.Date("2025-07-11"),
    discharge = as.Date(NA)
  )
  
  res_4 <- initialise_row(individual_data_4, delay_map, delay_boundaries, rng)
  
  # identify most outlying for each problematic delay
  group_dates <- names(individual_data_4[, -c(1:2)])
  row_dates <- unlist(individual_data_4[1, group_dates])
  median_val <- median(row_dates, na.rm = TRUE)
  
  candidates_for_removal1 <- c("onset", "report")
  candidates_for_removal2 <- c("hospitalisation", "death")
  
  date_values1 <- unlist(individual_data_4[1, candidates_for_removal1])
  date_values2 <- unlist(individual_data_4[1, candidates_for_removal2])
  
  outlier1 <- names(which.max(abs(date_values1 - median_val)))
  outlier2 <- names(which.max(abs(date_values2 - median_val)))
  
  expect_true(res_4[[outlier1]] != individual_data_4[[outlier1]])
  expect_true(res_4[[outlier2]] != individual_data_4[[outlier2]])
  
  # 5. can handle a scenario where there is only one non-missing date
  individual_data_5 <- data.frame(
    id = 34, group = 4,
    onset = as.Date(NA),
    hospitalisation = as.Date("2025-07-10"),
    report = as.Date(NA),
    death = as.Date(NA),
    discharge = as.Date(NA)
  )
  
  res_5 <- initialise_row(individual_data_5, delay_map, delay_boundaries, rng)
  exp_imputed <- c("onset", "report", "death")
  expect_true(all(!is.na(res_5[, exp_imputed])))
  
  # 6. weird issue
  # onset to report too long
  # onset to death too long
  # hospitalisation missing...
  # onset now updated but hospitalisation date not being imputed
  individual_data_6 <- data.frame(
    id = 22, group = 4,
    onset = as.Date("2025-03-29"),
    hospitalisation = as.Date(NA),
    report = as.Date("2025-04-15"),
    death = as.Date("2025-04-27"),
    discharge = as.Date(NA)
  )
  
  res_6 <- initialise_row(individual_data_6, delay_map, delay_boundaries, rng)
  
}) 

# TODO: Update input format - converted to integers before calling initialise
# TODO: sort out issue above
  

## ---------------------------------------------------------------------------



# test initialise_augmented_data()

test_that("data is initialised correctly", {

  # simulate test dataset
  delay_map <- data.frame(
    from = c("onset", "onset", "onset", "hospitalisation", "onset",
             "hospitalisation"),
    to = c("report", "death", "hospitalisation", "discharge", "hospitalisation",
           "death"),
    group = I(list(1:4, 2, 3, 3, 4, 4))
  )

  delay_params <- data.frame(
    group = c(1:4, 2, 3, 3, 4, 4),
    from = c("onset", "onset", "onset", "onset", "onset", "onset",
           "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "report", "report", "report", "death", "hospitalisation",
         "discharge", "hospitalisation", "death"),
    mean_delay = c(10, 10, 10, 10, 15, 7, 20, 7, 12),
    cv_delay = c(0.3, 0.3, 0.3, 0.3, 0.4, 0.2, 0.5, 0.2, 0.3)
  )

n_per_group <- rep(10, max(delay_params$group))
error_params <- list(prop_missing_data = 0.2, prob_error = 0.05)
range_dates <- as.integer(as.Date(c("2025-03-01", "2025-09-01")))

set.seed(1)
sim_result <- simulate_data(
  n_per_group = n_per_group,
  delay_map = delay_map,
  delay_params = delay_params,
  error_params = error_params,
  range_dates = range_dates,
  simul_error = TRUE
)

observed_data <- sim_result$observed_data

# set up for monty
control <- mcmc_control()

n_delays <- nrow(delay_map)
delay_ids <- seq_len(n_delays)
parameters <- c("prob_error", paste0("mean_delay", delay_ids),
                paste0("cv_delay", delay_ids))

observer <- monty::monty_observer(
  function(model = NULL) {
    if (is.null(model)) {
      NULL
    } else {
      list(errors = data_frame_to_array(model$error_indicators),
           true_dates = data_frame_to_array(model$true_dates))
    }
  }
)

model <- monty::monty_model(list(
  parameters = parameters,
  density = function(pars) 0,
  observer = observer))

model$groups <- observed_data$group
model$observed_dates <- observed_dates_to_int(observed_data)
model$delays <- delay_map
model$hyperparameters <- mcmc_hyperparameters()
rng <- monty::monty_rng_create(1)

aug_dat <- initialise_augmented_data(model, control, rng)

# Compare data to original
observed_data
aug_dat$true_dates
aug_dat$error_indicators

})
