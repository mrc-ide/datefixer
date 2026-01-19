test_that("resampling order calculated correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  delay_info <- make_delay_info(delay_map, dates)
  
  ## Expected behaviour: the result should feature the indexes in the
  ## first argument to_resample, we resample non-errors (FALSE) first and then
  ## errors or missing (TRUE/NA) - order within these is determined by
  ## the order in to_resample and whether or not errors or missing dates have
  ## delay-connected dates to use for sampling
  expect_equal(
    calc_resampling_order(c(1, 3), c(FALSE, NA, TRUE, NA, NA),
                          delay_info$is_date_in_delay[, , 1]), c(1, 3))
  expect_equal(
    calc_resampling_order(c(1, 3), c(TRUE, NA, FALSE, NA, NA),
                          delay_info$is_date_in_delay[, , 1]), c(3, 1))
  expect_equal(
    calc_resampling_order(c(1, 4, 3), c(TRUE, NA, FALSE, FALSE, NA),
                          delay_info$is_date_in_delay[, , 2]), c(4, 3, 1))
  expect_equal(
    calc_resampling_order(c(1, 4, 3), c(FALSE, NA, TRUE, TRUE, NA),
                          delay_info$is_date_in_delay[, , 2]), c(1, 4, 3))
  expect_equal(
    calc_resampling_order(c(1, 4, 3), c(TRUE, NA, FALSE, NA, NA),
                          delay_info$is_date_in_delay[, , 2]), c(3, 1, 4))
  expect_equal(
    calc_resampling_order(c(5, 3, 2, 1), c(TRUE, NA, FALSE, NA, TRUE),
                          delay_info$is_date_in_delay[, , 3]), c(3, 1, 2, 5))
  expect_equal(
    calc_resampling_order(c(5, 3, 2, 1), c(FALSE, NA, TRUE, NA, FALSE),
                          delay_info$is_date_in_delay[, , 3]), c(5, 1, 3, 2))
  expect_equal(
    calc_resampling_order(c(5, 3, 2, 1), c(TRUE, FALSE, FALSE, NA, NA),
                          delay_info$is_date_in_delay[, , 3]), c(3, 2, 5, 1))
})


test_that("updating estimated dates skipped correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  delay_info <- make_delay_info(delay_map, dates)
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1) 
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  control <- mcmc_control(prob_update_estimated_dates = 1)
  
  ## group = 2, i = 2 - no hospitalisation for this group so no update
  group <- 2
  i <- 2
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    update_estimated_dates1(i, augmented_data, observed_dates, group,
                            prob_error, delay_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## group = 4, i = 5 - no discharge for this group so no update
  group <- 4
  i <- 5
  observed_dates <- c(10, 5, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  augmented_data_new <- 
    update_estimated_dates1(i, augmented_data, observed_dates, group,
                            prob_error, delay_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## Try to update i = 4 for group 4, but with prob_update_estimated_dates = 0
  ## there should be no update, and a single random draw
  i <- 4
  control <- mcmc_control(prob_update_estimated_dates = 0)
  augmented_data_new <- 
    update_estimated_dates1(i, augmented_data, observed_dates, group,
                            prob_error, delay_info, date_range, control, rng)
  ## augmented_data should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  ## rng should have gone through a single random_real draw so let's do the same
  ## to rng1 and check they match
  x <- monty::monty_random_real(rng1)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
})


test_that("updating error indicators skipped correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  delay_info <- make_delay_info(delay_map, dates)
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  control <- mcmc_control(prob_update_error_indicators = 1)
  
  ## group = 2, i = 2 - missing date so no update
  group <- 2
  i <- 2
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    update_error_indicators1(i, augmented_data, observed_dates, group,
                            prob_error, delay_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## group = 4, i = 4 - missing date so no update
  group <- 4
  i <- 4
  observed_dates <- c(10, 5, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  augmented_data_new <- 
    update_error_indicators1(i, augmented_data, observed_dates, group,
                             prob_error, delay_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## Try to update i = 3 for group 4, but with prob_update_error_indicators = 0
  ## there should be no update, and a single random draw
  i <- 3
  control <- mcmc_control(prob_update_error_indicators = 0)
  augmented_data_new <- 
    update_error_indicators1(i, augmented_data, observed_dates, group,
                             prob_error, delay_info, date_range, control, rng)
  ## augmented_data should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  ## rng should have gone through a single random_real draw so let's do the same
  ## to rng1 and check they match
  x <- monty::monty_random_real(rng1)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
})


test_that("swap error indicators skipped correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  delay_info <- make_delay_info(delay_map, dates)
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  control <- mcmc_control(prob_error_swap = 1)
  
  ## group = 2, both dates FALSE so no update
  group <- 2
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 68.1, NA),
                         error_indicators = c(NA, NA, FALSE, FALSE, NA))
  augmented_data_new <- 
    swap_error_indicators(augmented_data, observed_dates, group,
                          prob_error, delay_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## group = 4, no TRUE (only FALSE or missing) so no update
  group <- 4
  i <- 4
  observed_dates <- c(10, 15, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, FALSE, FALSE, NA, NA))
  augmented_data_new <- 
    swap_error_indicators(augmented_data, observed_dates, group,
                          prob_error, delay_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## Try to update individual with mixed errors, but with
  ## prob_swap_error_indicators = 0 there should be no update, and a single
  ## random draw
  augmented_data <- list(estimated_dates = c(10.3, 20.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  expect_true(has_mixed_errors(augmented_data$error_indicators))
  control <- mcmc_control(prob_error_swap = 0)
  augmented_data_new <- 
    swap_error_indicators(augmented_data, observed_dates, group,
                          prob_error, delay_info, date_range, control, rng)
  ## augmented_data should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  ## rng should have gone through a single random_real draw so let's do the same
  ## to rng1 and check they match
  x <- monty::monty_random_real(rng1)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
})


test_that("estimated dates proposed correctly", {
  
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  delay_info <- make_delay_info(delay_map, dates)
  
  delay_info$mean <- c(5, 8, 3, 4, 7, 10)
  delay_info$cv <- c(0.5, 0.3, 0.2, 0.7, 0.6, 0.9)
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  ## group 2, propose new (correct) report date
  group <- 2
  to_update <- 3
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    propose_estimated_dates(to_update, augmented_data, observed_dates, group,
                            delay_info, rng, FALSE)
  expect_equal(augmented_data$error_indicators,
               augmented_data_new$error_indicators)
  expect_equal(augmented_data$estimated_dates[-to_update],
               augmented_data_new$estimated_dates[-to_update])
  expect_equal(augmented_data_new$estimated_dates[to_update],
               observed_dates[to_update] + monty::monty_random_real(rng1))
  
  
  ## group 2, propose new (error) death date
  group <- 2
  to_update <- 4
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    propose_estimated_dates(to_update, augmented_data, observed_dates, group,
                            delay_info, rng, FALSE)
  expect_equal(augmented_data$error_indicators,
               augmented_data_new$error_indicators)
  expect_equal(augmented_data$estimated_dates[-to_update],
               augmented_data_new$estimated_dates[-to_update])
  expect_equal(augmented_data_new$estimated_dates[to_update],
               sample_from_delay(to_update, augmented_data_new$estimated_dates,
                                 group, delay_info, rng1))
  
  
  ## group 2, propose new (missing) onset date
  group <- 2
  to_update <- 1
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    propose_estimated_dates(to_update, augmented_data, observed_dates, group,
                            delay_info, rng, FALSE)
  expect_equal(augmented_data$error_indicators,
               augmented_data_new$error_indicators)
  expect_equal(augmented_data$estimated_dates[-to_update],
               augmented_data_new$estimated_dates[-to_update])
  expect_equal(augmented_data_new$estimated_dates[to_update],
               sample_from_delay(to_update, augmented_data_new$estimated_dates,
                                 group, delay_info, rng1))
  
  
  ## group 2, propose new report date, going from correct to error
  group <- 2
  to_update <- 3
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    propose_estimated_dates(to_update, augmented_data, observed_dates, group,
                            delay_info, rng, TRUE)
  expect_equal(augmented_data$error_indicators[-to_update],
               augmented_data_new$error_indicators[-to_update])
  expect_equal(augmented_data$error_indicators[to_update], FALSE)
  expect_equal(augmented_data$estimated_dates[-to_update],
               augmented_data_new$estimated_dates[-to_update])
  expect_equal(augmented_data_new$estimated_dates[to_update],
               sample_from_delay(to_update, augmented_data_new$estimated_dates,
                                 group, delay_info, rng1))
  
  
  ## group 2, propose new death date, going from error to correct
  group <- 2
  to_update <- 4
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    propose_estimated_dates(to_update, augmented_data, observed_dates, group,
                            delay_info, rng, TRUE)
  expect_equal(augmented_data$error_indicators[-to_update],
               augmented_data_new$error_indicators[-to_update])
  expect_equal(augmented_data$error_indicators[to_update], TRUE)
  expect_equal(augmented_data$estimated_dates[-to_update],
               augmented_data_new$estimated_dates[-to_update])
  expect_equal(augmented_data_new$estimated_dates[to_update],
               observed_dates[to_update] + monty::monty_random_real(rng1))
  
  
  ## group 2, propose all dates, swapping errors
  group <- 2
  to_update <- c(3, 1, 4)
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    propose_estimated_dates(to_update, augmented_data, observed_dates, group,
                            delay_info, rng, TRUE)
  expect_equal(augmented_data_new$error_indicators, 
               c(NA, NA, TRUE, FALSE, NA))
  estimated_dates <- rep(NA, 5)
  ## date 4 will be sampled based on observed date
  estimated_dates[4] <- observed_dates[4] + monty::monty_random_real(rng1)
  ## will then sample date 1 (connected to date 4) and then date 3
  estimated_dates[1] <- 
    sample_from_delay(1, estimated_dates, group, delay_info, rng1)
  estimated_dates[3] <- 
    sample_from_delay(3, estimated_dates, group, delay_info, rng1)
  expect_equal(augmented_data_new$estimated_dates, estimated_dates)
  
})

