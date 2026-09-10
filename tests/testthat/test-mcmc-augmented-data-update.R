test_that("sampling order calculated correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  ## Expected behaviour: the result should feature the indexes in the
  ## first argument to_resample, we resample non-errors (FALSE) first and then
  ## errors or missing (TRUE/NA) - order within these is determined by
  ## the order in to_resample and whether or not errors or missing dates have
  ## delay-connected dates to use for sampling
  expect_equal(
    calc_batch_sampling_order(c(1, 3), c(FALSE, NA, TRUE, NA, NA),
                              model_info$group_info[[1]]$is_date_connected),
    c(1, 3))
  expect_equal(
    calc_batch_sampling_order(c(1, 3), c(TRUE, NA, FALSE, NA, NA),
                              model_info$group_info[[1]]$is_date_connected),
    c(3, 1))
  expect_equal(
    calc_batch_sampling_order(c(1, 4, 3), c(TRUE, NA, FALSE, FALSE, NA),
                              model_info$group_info[[2]]$is_date_connected),
    c(4, 3, 1))
  expect_equal(
    calc_batch_sampling_order(c(1, 4, 3), c(FALSE, NA, TRUE, TRUE, NA),
                              model_info$group_info[[2]]$is_date_connected),
    c(1, 4, 3))
  expect_equal(
    calc_batch_sampling_order(c(1, 4, 3), c(TRUE, NA, FALSE, NA, NA),
                              model_info$group_info[[2]]$is_date_connected), 
    c(3, 1, 4))
  expect_equal(
    calc_batch_sampling_order(c(5, 3, 2, 1), c(TRUE, NA, FALSE, NA, TRUE),
                              model_info$group_info[[3]]$is_date_connected), 
    c(3, 1, 2, 5))
  expect_equal(
    calc_batch_sampling_order(c(5, 3, 2, 1), c(FALSE, NA, TRUE, NA, FALSE),
                              model_info$group_info[[3]]$is_date_connected),
    c(5, 1, 3, 2))
  expect_equal(
    calc_batch_sampling_order(c(5, 3, 2, 1), c(TRUE, FALSE, FALSE, NA, NA),
                              model_info$group_info[[3]]$is_date_connected), 
    c(3, 2, 5, 1))
})


test_that("cascade resampling order calculated correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  # group 2: onset(1) to report(3) and onset(1) to death(4)
  # 3 and 4 NOT directly connected
  
  group_info <- model_info$group_info[[2]]
  event_order <- group_info$event_order
  is_date_connected <- group_info$is_date_connected
  shortest_paths <- group_info$shortest_paths
  
  ## i = 3
  ## all correct, no cascade
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(FALSE, NA, FALSE, FALSE, NA),
                                is_date_connected, shortest_paths),
    3)
  
  ## i = 3
  ## 3 (correct) only connected to a correct date, no cascade
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(FALSE, NA, FALSE, NA, NA),
                                is_date_connected, shortest_paths),
    3)
  
  ## i = 3
  ## 3 (correct) --> 1 (erroneous) --> 4 (missing)
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(TRUE, NA, FALSE, NA, NA),
                                is_date_connected, shortest_paths),
    c(3, 1, 4))
  
  ## same as above but 1 is missing instead (makes no difference)
  ## i = 3
  ## 3 (correct) --> 1 (missing) --> 4 (missing)
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(NA, NA, FALSE, NA, NA),
                                is_date_connected, shortest_paths),
    c(3, 1, 4))
  
  ## i = 3
  ## 3 (correct) --> 1 (erroneous)
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(TRUE, NA, FALSE, FALSE, NA),
                                is_date_connected, shortest_paths),
    c(3, 1))
  
  ## i = 3
  ## no correct dates so cascade from 3
  ## 3 (erroneous) --> 1 (missing) --> 4 (missing)
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(NA, NA, TRUE, NA, NA),
                                is_date_connected, shortest_paths),
    c(3, 1, 4))
  
  ## i = 3
  ## 3 (erroneous) cannot cascade - only connected to 1 which is correct
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(FALSE, NA, TRUE, NA, NA),
                                is_date_connected, shortest_paths),
    3)
  
  ## i = 3
  ## anchor = 4 (nearest correct date to 3)
  ## 1 (missing) --> 3 (erroneous)
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(NA, NA, TRUE, FALSE, NA),
                                is_date_connected, shortest_paths),
    c(1, 3))
  
  
  ## i = 1
  ## 1 (correct) --> 3 (erroneous) and 1 --> 4 (missing)
  ## event order determines order in which we do those
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, NA, TRUE, NA, NA),
                                is_date_connected, shortest_paths),
    c(1, 3, 4))
  
  ## i = 1
  ## 1 (correct) --> 3 (erroneous)
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, NA, TRUE, FALSE, NA),
                                is_date_connected, shortest_paths),
    c(1, 3))
  
  ## i = 1
  ## 1 (erroneous), no cascade as all connected dates correct
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, NA, FALSE, FALSE, NA),
                                is_date_connected, shortest_paths),
    1)
  
  ## i = 1
  ## cascade from 1 as no correct dates
  ## 1 (erroneous) --> 3 (erroneous) --> 4 (missing)
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, NA, TRUE, NA, NA),
                                is_date_connected, shortest_paths),
    c(1, 3, 4))
  
  ## i = 1
  ## anchor = 3 (correct date connected to 1)
  ## 1 (erroneous) --> 4 (erroneous)
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, NA, FALSE, NA, NA),
                                is_date_connected, shortest_paths),
    c(1, 4))
  
  
  # group 4: onset(1) to report(3) and onset(1) to hospitalisation (2) then
  #          hospitalisation (2) to death (4)
  
  group_info <- model_info$group_info[[4]]
  event_order <- group_info$event_order
  is_date_connected <- group_info$is_date_connected
  shortest_paths <- group_info$shortest_paths
  
  ## i = 1
  ## all correct, no cascade
  expect_equal(
    calc_cascade_sampling_order(1, event_order, 
                                c(FALSE, FALSE, FALSE, FALSE, NA),
                                is_date_connected, shortest_paths),
    1)
  
  ## i = 1
  ## 1 (correct) only connected to correct dates, no cascade
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, FALSE, FALSE, NA, NA),
                                is_date_connected, shortest_paths),
    1)
  
  ## i = 1
  ## 1 (correct) --> 2 (erroneous) and 1 --> 3 (missing)
  ## then 2 --> 4 (missing)
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, TRUE, NA, NA, NA),
                                is_date_connected, shortest_paths),
    c(1, 2, 3, 4))
  
  ## i = 1
  ## 1 (correct) --> 2 (erroneous) --> 4 (missing)
  ## no cascade to 3 (correct) 
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, TRUE, FALSE, NA, NA),
                                is_date_connected, shortest_paths),
    c(1, 2, 4))
  
  ## anchor = 1
  ## 1 (correct) --> 3 (erroneous)
  ## no cascade to 2 (correct) 
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, FALSE, TRUE, NA, NA),
                                is_date_connected, shortest_paths),
    c(1, 3))
  
  ## i = 1
  ## anchor = 4 (nearest correct date to 1)
  ## 2 (missing) --> 1 (erroneous) --> 3 (erroneous)
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, NA, TRUE, FALSE, NA),
                                is_date_connected, shortest_paths),
    c(2, 1, 3))
  
  ## i = 1
  ## anchor = 3 (correct date connected to 1)
  ## 1 (erroneous) --> 2 (erroneous) --> 4 (missing)
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, NA, FALSE, TRUE, NA),
                                is_date_connected, shortest_paths),
    c(1, 2, 4))
  
  ## i = 1
  ## anchor = 2 (correct date connected to 1)
  ## 1 (erroneous) --> 3 (missing)
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, FALSE, NA, TRUE, NA),
                                is_date_connected, shortest_paths),
    c(1, 3))
  
  
  ## i = 2
  ## 2 (correct) --> 1 (erroneous) and 2 (correct) --> 4 (missing)
  ## then 1 (erroneous) --> 3 (missing)
  expect_equal(
    calc_cascade_sampling_order(2, event_order, c(TRUE, FALSE, NA, NA, NA),
                                is_date_connected, shortest_paths),
    c(2, 1, 4, 3))
  
  
  ## i = 2
  ## anchor = 4 (correct date connected to 4)
  ## 2 (erroneous) --> 1 (erroneous) --> 3 (missing)
  expect_equal(
    calc_cascade_sampling_order(2, event_order, c(TRUE, TRUE, NA, FALSE, NA),
                                is_date_connected, shortest_paths),
    c(2, 1, 3))
  
  ## i = 2
  ## anchor = 1 (correct date connected to 2)
  ## 2 (erroneous) --> 4 (erroneous)
  expect_equal(
    calc_cascade_sampling_order(2, event_order, c(FALSE, TRUE, NA, TRUE, NA),
                                is_date_connected, shortest_paths),
    c(2, 4))
  
  
  
  ## special case where there are no paths between some dates
  delay_map <- data.frame(
    from = c("onset", "hospitalisation"),
    to = c("report", "death"),
    group = c(1, 1),
    distribution = c("gamma", "gamma")
  )
  model_info <- 
    make_model_info(delay_map, c("onset", "hospitalisation", "report", "death"))
  
  group_info <- model_info$group_info[[1]]
  event_order <- group_info$event_order
  is_date_connected <- group_info$is_date_connected
  shortest_paths <- group_info$shortest_paths
  ## 3 is only possible anchor but no path to it
  expect_equal(
    calc_cascade_sampling_order(2, event_order, c(NA, TRUE, FALSE, NA),
                                is_date_connected, shortest_paths),
    c(2, 4))
})


test_that("updating estimated dates skipped correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  delay_info <- model_info$delay_info
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1) 
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  delay_pars <- list(list(mean = 8, shape = 4),
                     list(mean = 5, shape = 3),
                     list(mean = 3.2, shape = 1.5),
                     list(mean = 6.4, shape = 2.7),
                     list(meanlog = 2.5, precisionlog = 0.3),
                     list(meanlog = 1.8, precisionlog = 0.8))
  control <- chronofix_mcmc_control(prob_update_estimated_dates = 0)
  
  ## Try to update date = 4 for group 4, but with 
  ## prob_update_estimated_dates = 0 there should be no update,
  ## and a single random draw
  group <- 4
  group_info <- model_info$group_info[[group]]
  date <- 4
  i_group <- 1
  observed_dates <- rbind(c(10, 5, 30, NA, NA))
  augmented_data <- 
    list(estimated_dates = rbind(c(10.3, 15.4, 30.2, 40.1, NA)),
         error_indicators = rbind(c(FALSE, TRUE, FALSE, NA, NA)))
  augmented_data_new <- 
    update_estimated_dates1(date, i_group, augmented_data, observed_dates,
                            prob_error, delay_pars, delay_info, group_info,
                            date_range, control, rng)
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
  model_info <- make_model_info(delay_map, dates)
  delay_info <- model_info$delay_info
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  delay_pars <- list(list(mean = 8, shape = 4),
                     list(mean = 5, shape = 3),
                     list(mean = 3.2, shape = 1.5),
                     list(mean = 6.4, shape = 2.7),
                     list(meanlog = 2.5, precisionlog = 0.3),
                     list(meanlog = 1.8, precisionlog = 0.8))
  control <- chronofix_mcmc_control(prob_update_error_indicators = 1)
  
  ## group = 2, date = 2 - missing date so no update
  group <- 2
  group_info <- model_info$group_info[[group]]
  date <- 2
  i_group <- 1
  observed_dates <- rbind(c(NA, NA, 40, 68, NA))
  augmented_data <-
    list(rbind(estimated_dates = c(20.5, NA, 40.2, 50.1, NA)),
         error_indicators = rbind(c(NA, NA, FALSE, TRUE, NA)))
  augmented_data_new <- 
    update_error_indicators1(date, i_group, augmented_data, observed_dates,
                             prob_error, delay_pars, delay_info, group_info,
                             date_range, control, rng)
  ## augmented_data should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  ## rng should have gone through a single random_real draw so let's do the same
  ## to rng1 and check they match
  x <- monty::monty_random_real(rng1)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## group = 4, i = 4 - missing date so no update
  group <- 4
  group_info <- model_info$group_info[[group]]
  date <- 4
  i_group <- 1
  observed_dates <- rbind(c(10, 5, 30, NA, NA))
  augmented_data <-
    list(estimated_dates = rbind(c(10.3, 15.4, 30.2, 40.1, NA)),
         error_indicators = rbind(c(FALSE, TRUE, FALSE, NA, NA)))
  augmented_data_new <- 
    update_error_indicators1(date, i_group, augmented_data, observed_dates,
                             prob_error, delay_pars, delay_info, group_info,
                             date_range, control, rng)
  ## augmented_data should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  ## rng should have gone through a single random_real draw so let's do the same
  ## to rng1 and check they match
  x <- monty::monty_random_real(rng1)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## Try to update i = 3 for group 4, but with prob_update_error_indicators = 0
  ## there should be no update, and a single random draw
  date <- 3
  control <- chronofix_mcmc_control(prob_update_error_indicators = 0)
  augmented_data_new <- 
    update_error_indicators1(date, i_group, augmented_data, observed_dates,
                             prob_error, delay_pars, delay_info, group_info,
                             date_range, control, rng)
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
  model_info <- make_model_info(delay_map, dates)
  delay_info <- model_info$delay_info
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  delay_pars <- list(list(mean = 8, shape = 4),
                     list(mean = 5, shape = 3),
                     list(mean = 3.2, shape = 1.5),
                     list(mean = 6.4, shape = 2.7),
                     list(meanlog = 2.5, precisionlog = 0.3),
                     list(meanlog = 1.8, precisionlog = 0.8))
  control <- chronofix_mcmc_control(prob_error_swap = 1)
  
  ## group = 2, both dates FALSE so no update
  group <- 2
  group_info <- model_info$group_info[[group]]
  i_group <- 1
  observed_dates <- rbind(c(NA, NA, 40, 68, NA))
  augmented_data <-
    list(estimated_dates = rbind(c(20.5, NA, 40.2, 68.1, NA)),
         error_indicators = rbind(c(NA, NA, FALSE, FALSE, NA)))
  augmented_data_new <- 
    swap_error_indicators(i_group, augmented_data, observed_dates,
                          prob_error, delay_pars, delay_info, group_info,
                          date_range, control, rng)
  ## augmented_data should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  ## rng should have gone through a single random_real draw so let's do the same
  ## to rng1 and check they match
  x <- monty::monty_random_real(rng1)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## group = 4, no TRUE (only FALSE or missing) so no update
  group <- 4
  group_info <- model_info$group_info[[group]]
  i_group <- 1
  observed_dates <- rbind(c(10, 15, 30, NA, NA))
  augmented_data <-
    list(estimated_dates = rbind(c(10.3, 15.4, 30.2, 40.1, NA)),
         error_indicators = rbind(c(FALSE, FALSE, FALSE, NA, NA)))
  augmented_data_new <- 
    swap_error_indicators(i_group, augmented_data, observed_dates,
                          prob_error, delay_pars, delay_info, group_info,
                          date_range, control, rng)
  ## augmented_data should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  ## rng should have gone through a single random_real draw so let's do the same
  ## to rng1 and check they match
  x <- monty::monty_random_real(rng1)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## Try to update individual with mixed errors, but with
  ## prob_swap_error_indicators = 0 there should be no update, and a single
  ## random draw
  augmented_data <-
    list(estimated_dates = rbind(c(10.3, 20.4, 30.2, 40.1, NA)),
         error_indicators = rbind(c(FALSE, TRUE, FALSE, NA, NA)))
  expect_true(has_mixed_errors(augmented_data$error_indicators))
  control <- chronofix_mcmc_control(prob_error_swap = 0)
  augmented_data_new <- 
    swap_error_indicators(i_group, augmented_data, observed_dates,
                          prob_error, delay_pars, delay_info, group_info,
                          date_range, control, rng)
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
  model_info <- make_model_info(delay_map, dates)
  delay_info <- model_info$delay_info
  date_range <- toy_model()$date_range
  
  delay_pars <- list(list(mean = 8, shape = 4),
                     list(mean = 5, shape = 3),
                     list(mean = 3.2, shape = 1.5),
                     list(mean = 6.4, shape = 2.7),
                     list(meanlog = 2.5, precisionlog = 0.3),
                     list(meanlog = 1.8, precisionlog = 0.8))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  ## group 2, propose new (correct) report date
  group <- 2
  group_info <- model_info$group_info[[group]]
  to_update <- 3
  observed_dates <- c(NA, NA, 40, 68, NA)
  estimated_dates <- c(20.5, NA, 40.2, 50.1, NA)
  error_indicators <- c(NA, NA, FALSE, TRUE, NA)
  estimated_dates_new <- 
    propose_estimated_dates(to_update, estimated_dates, error_indicators,
                            observed_dates,  delay_pars, delay_info, group_info,
                            date_range, rng)
  expect_equal(estimated_dates[-to_update], estimated_dates_new[-to_update])
  expect_equal(estimated_dates_new[to_update],
               observed_dates[to_update] + monty::monty_random_real(rng1))
  
  
  ## group 2, propose new (error) death date
  ## based on delay 2, onset (date 1) to death (date 4)
  group <- 2
  group_info <- model_info$group_info[[group]]
  to_update <- 4
  observed_dates <- c(NA, NA, 40, 68, NA)
  estimated_dates <- c(20.5, NA, 40.2, 50.1, NA)
  error_indicators <- c(NA, NA, FALSE, TRUE, NA)
  estimated_dates_new <- 
    propose_estimated_dates(to_update, estimated_dates, error_indicators,
                            observed_dates,  delay_pars, delay_info, group_info,
                            date_range, rng)
  expect_equal(estimated_dates[-to_update], estimated_dates_new[-to_update])
  expect_equal(estimated_dates_new[to_update],
               sample_from_delay(to_update, estimated_dates, error_indicators,
                                 delay_pars, delay_info, group_info, 
                                 date_range, rng1))
  
  
  ## group 2, propose all dates
  group <- 2
  group_info <- model_info$group_info[[group]]
  
  observed_dates <- c(NA, NA, 40, 68, NA)
  estimated_dates <- c(20.5, NA, 40.2, 50.1, NA)
  error_indicators <- c(NA, NA, TRUE, FALSE, NA)
  sampling_order <-
    calc_batch_sampling_order(group_info$event_order, error_indicators,
                              group_info$is_date_connected)
  estimated_dates_new <- 
    propose_estimated_dates(sampling_order, estimated_dates, error_indicators,
                            observed_dates,  delay_pars, delay_info, group_info,
                            date_range, rng)
  cmp <- rep(NA, 5)
  ## date 4 will be sampled based on observed date
  cmp[4] <- observed_dates[4] + monty::monty_random_real(rng1)
  ## will then sample date 1 (connected to date 4) and then date 3
  cmp[1] <- sample_from_delay(1, cmp, error_indicators, delay_pars, 
                              delay_info, group_info, date_range, rng1)
  cmp[3] <- sample_from_delay(3, cmp, error_indicators, delay_pars, 
                              delay_info, group_info, date_range, rng1)
  expect_equal(estimated_dates_new, cmp)
  
})


test_that("proposal density calculated correctly", {
  
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  delay_info <- model_info$delay_info
  date_range <- toy_model()$date_range
  
  delay_pars <- list(list(mean = 8, shape = 4),
                     list(mean = 5, shape = 3),
                     list(mean = 3.2, shape = 1.5),
                     list(mean = 6.4, shape = 2.7),
                     list(meanlog = 2.5, precisionlog = 0.3),
                     list(meanlog = 1.8, precisionlog = 0.8))
  
  # group 2, 3 dates
  group <- 2
  group_info <- model_info$group_info[[group]]
  estimated_dates <- c(20.5, NA, 40.2, 50.1, NA)
  error_indicators <- c(NA, NA, FALSE, TRUE, NA)
  
  ll_delays <- chronofix_log_likelihood_delays1(
    estimated_dates, delay_pars, delay_info, group_info$is_delay_in_group)
  
  ## group 2, updated correct report date
  ## proposal log-density should be zero
  updated <- 3
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), 0)
  
  ## group 2, updated error death date
  ## proposal based on delay 2 (gamma) onset (date 1) to death (date 4)
  updated <- 4
  d <- dgamma(estimated_dates[4] - estimated_dates[1],
              shape = delay_pars[[2]]$shape, 
              rate = delay_pars[[2]]$shape / delay_pars[[2]]$mean, log = TRUE)
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
  
  ## group 2, updated missing onset date 
  ## based on delay 1 (gamma), onset (date 1) to report (date 3)
  ## and delay 2 (gamma), onset (date 1) to death (date 4)
  ## the two delays are equally likely to be used
  updated <- 1
  d <- log(sum(
    dgamma(estimated_dates[c(3, 4)] - estimated_dates[1],
           shape = c(delay_pars[[1]]$shape, delay_pars[[2]]$shape), 
           rate = c(delay_pars[[1]]$shape, delay_pars[[2]]$shape) /
             c(delay_pars[[1]]$mean, delay_pars[[2]]$mean)))) - log(2)
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
  
  ## group 2, updated all dates
  ## report (date 3) is correct so has no impact for proposing this
  ## then onset is proposed based on delay 1 (gamma), 
  ##               onset (date 1) to report (date 3)
  ## then death is proposed based on delay 2 (gamma),
  ##               onset (date 1) to death (date 4)
  updated <-
    calc_batch_sampling_order(model_info$group_info[[group]]$event_order,
                              error_indicators,
                              model_info$group_info[[group]]$is_date_connected)
  d <- sum(dgamma(estimated_dates[c(3, 4)] - estimated_dates[1],
                  shape = c(delay_pars[[1]]$shape, delay_pars[[2]]$shape), 
                  rate = c(delay_pars[[1]]$shape, delay_pars[[2]]$shape) /
                    c(delay_pars[[1]]$mean, delay_pars[[2]]$mean), log = TRUE))
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
    
  
  ## group 2, updated missing onset date 
  ## based on delay 1 (gamma), onset (date 1) to report (date 3)
  ## and delay 2 (gamma), onset (date 1) to death (date 4)
  ## the two delays are equally likely to be used
  estimated_dates <- c(20.5, NA, 40.2, 50.1, NA)
  error_indicators <- c(NA, NA, FALSE, FALSE, NA)
  updated <- 1
  ll_delays <- chronofix_log_likelihood_delays1(
    estimated_dates, delay_pars, delay_info, group_info$is_delay_in_group)
  d <- log(sum(
    dgamma(estimated_dates[c(3, 4)] - estimated_dates[1],
           shape = c(delay_pars[[1]]$shape, delay_pars[[2]]$shape), 
           rate = c(delay_pars[[1]]$shape, delay_pars[[2]]$shape) /
             c(delay_pars[[1]]$mean, delay_pars[[2]]$mean)))) - log(2)
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
  
  ## group 2, updated missing onset date 
  ## based on delay 1 (gamma), onset (date 1) to report (date 3)
  ## and delay 2 (gamma), onset (date 1) to death (date 4)
  ## the two delays are equally likely to be used
  estimated_dates <- c(20.5, NA, 40.2, 50.1, NA)
  error_indicators <- c(NA, NA, FALSE, FALSE, NA)
  updated <- 1
  ll_delays <- chronofix_log_likelihood_delays1(
    estimated_dates, delay_pars, delay_info, group_info$is_delay_in_group)
  d <- log(sum(
    dgamma(estimated_dates[c(3, 4)] - estimated_dates[1],
           shape = c(delay_pars[[1]]$shape, delay_pars[[2]]$shape), 
           rate = c(delay_pars[[1]]$shape, delay_pars[[2]]$shape) /
             c(delay_pars[[1]]$mean, delay_pars[[2]]$mean)))) - log(2)
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
  
  
  ## group 2, updated all dates via cascade
  ## no correct dates
  ## start with report (date 3), no delay available so date proposed at random
  ## then onset is proposed based on delay 1 (gamma), 
  ##               onset (date 1) to report (date 3)
  ## then death is proposed based on delay 2 (gamma),
  ##               onset (date 1) to death (date 4)
  estimated_dates <- c(20.5, NA, 40.2, 50.1, NA)
  error_indicators <- c(NA, NA, TRUE, TRUE, NA)
  updated <-
    calc_cascade_sampling_order(3, group_info$event_order, error_indicators,
                                group_info$is_date_connected,
                                group_info$shortest_paths)
  ll_delays <- chronofix_log_likelihood_delays1(
    estimated_dates, delay_pars, delay_info, group_info$is_delay_in_group)
  d <- -log(date_range[2] - date_range[1]) +
    sum(dgamma(estimated_dates[c(3, 4)] - estimated_dates[1],
                  shape = c(delay_pars[[1]]$shape, delay_pars[[2]]$shape), 
                  rate = c(delay_pars[[1]]$shape, delay_pars[[2]]$shape) /
                    c(delay_pars[[1]]$mean, delay_pars[[2]]$mean), log = TRUE))
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
  
  
  # group 4, 3 dates
  group <- 4
  group_info <- model_info$group_info[[group]]
  estimated_dates <- c(10.3, 15.4, 30.2, 40.1, NA)
  error_indicators <- c(FALSE, TRUE, FALSE, NA, NA)
  ll_delays <- chronofix_log_likelihood_delays1(
    estimated_dates, delay_pars, delay_info, group_info$is_delay_in_group)
  
  ## group 4, updated correct onset date
  ## proposal log-density should be zero
  updated <- 1
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), 0)
  
  ## group 4, updated correct report date
  ## proposal log-density should be zero
  updated <- 3
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), 0)
  
  ## group 4, updated error hospitalisation date 
  ## based on delay 5 (log-normal), onset (date 1) to hospitalisation (date 2)
  ## and delay 6 (log-normal), hospitalisation (date 2) to death (date 4)
  ## the two delays are equally likely to be used
  updated <- 2
  d <- log(sum(
    dlnorm(estimated_dates[c(2, 4)] - estimated_dates[c(1, 2)],
           meanlog = c(delay_pars[[5]]$meanlog, delay_pars[[6]]$meanlog), 
           sdlog = 1 / sqrt(c(delay_pars[[5]]$precisionlog, 
                              delay_pars[[6]]$precisionlog))))) - log(2)
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
  
  ## group 4, updated missing death date 
  ## based on delay 6 (log-normal), hospitalisation (date 2) to death (date 4)
  updated <- 4
  d <- dlnorm(estimated_dates[4] - estimated_dates[2],
              meanlog = delay_pars[[6]]$meanlog, 
              sdlog = 1 / sqrt(delay_pars[[6]]$precisionlog), log = TRUE)
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
  
  ## group 4, updated all dates
  ## onset (date 1) and report (date 3) correct so no impact for proposing
  ## then hospitalisation is proposed based on delay 5, onset (date 1) to
  ##    hospitalisation (date 2)
  ## then death is proposed based on delay 6 (log-normal), hospitalisation
  ##    (date 2) to death (date 4)
  updated <- c(1, 2, 3, 4)
  d <- sum(dlnorm(estimated_dates[c(2, 4)] - estimated_dates[c(1, 2)],
                  meanlog = c(delay_pars[[5]]$meanlog, delay_pars[[6]]$meanlog), 
                  sdlog = 1 / sqrt(c(delay_pars[[5]]$precisionlog, 
                                     delay_pars[[6]]$precisionlog)),
                  log = TRUE))
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
  
  
  ## group 4, updated error hospitalisation date 
  ## based on delay 5 (log-normal), onset (date 1) to hospitalisation (date 2)
  ## and delay 6 (log-normal), hospitalisation (date 2) to death (date 4)
  ## the two delays are equally likely to be used
  updated <- 2
  estimated_dates <- c(10.3, 15.4, 30.2, 40.1, NA)
  error_indicators <- c(FALSE, TRUE, FALSE, FALSE, NA)
  ll_delays <- chronofix_log_likelihood_delays1(
    estimated_dates, delay_pars, delay_info, group_info$is_delay_in_group)
  d <- log(sum(
    dlnorm(estimated_dates[c(2, 4)] - estimated_dates[c(1, 2)],
           meanlog = c(delay_pars[[5]]$meanlog, delay_pars[[6]]$meanlog), 
           sdlog = 1 / sqrt(c(delay_pars[[5]]$precisionlog, 
                              delay_pars[[6]]$precisionlog))))) - log(2)
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
  
  
  ## group 4, updated error hospitalisation date 
  ## based on delay 5 (log-normal), onset (date 1) to hospitalisation (date 2)
  ## and delay 6 (log-normal), hospitalisation (date 2) to death (date 4)
  ## the two delays are equally likely to be used
  updated <- 2
  estimated_dates <- c(10.3, 15.4, 30.2, 40.1, NA)
  error_indicators <- c(TRUE, TRUE, FALSE, NA, NA)
  ll_delays <- chronofix_log_likelihood_delays1(
    estimated_dates, delay_pars, delay_info, group_info$is_delay_in_group)
  d <- log(sum(
    dlnorm(estimated_dates[c(2, 4)] - estimated_dates[c(1, 2)],
           meanlog = c(delay_pars[[5]]$meanlog, delay_pars[[6]]$meanlog), 
           sdlog = 1 / sqrt(c(delay_pars[[5]]$precisionlog, 
                              delay_pars[[6]]$precisionlog))))) - log(2)
  expect_equal(calc_proposal_density(
    updated, error_indicators, ll_delays, group_info, date_range), d)
})


test_that("acceptance probability calculated correctly", {
  
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  delay_info <- model_info$delay_info
  
  prob_error <- 0.05
  delay_pars <- list(list(mean = 8, shape = 4),
                     list(mean = 5, shape = 3),
                     list(mean = 3.2, shape = 1.5),
                     list(mean = 6.4, shape = 2.7),
                     list(meanlog = 2.5, precisionlog = 0.3),
                     list(meanlog = 1.8, precisionlog = 0.8))
  
  date_range <- c(0, 101)
  
  ## separate function for calculating acceptance probability
  calc_accept <- function(sampling_order, sampling_order_reverse,
                          estimated_dates_new, estimated_dates,
                          error_indicators_new, error_indicators,
                          group_info) {
    ll_delays_current <- 
      chronofix_log_likelihood_delays1(estimated_dates, delay_pars, 
                                       delay_info, group_info$is_delay_in_group)
    ll_delays_new <- 
      chronofix_log_likelihood_delays1(estimated_dates_new, delay_pars, 
                                       delay_info, group_info$is_delay_in_group)
    
    ll_errors_current <-
      chronofix_log_likelihood_errors(prob_error, error_indicators, date_range)
    ll_errors_new <-
      chronofix_log_likelihood_errors(prob_error, error_indicators_new,
                                      date_range)
    
    ratio_ll_delays <- sum(ll_delays_new) - sum(ll_delays_current)
    ratio_ll_errors <- ll_errors_new - ll_errors_current
    ratio_post <- ratio_ll_delays + ratio_ll_errors
    
    prop_current <- 
      calc_proposal_density(sampling_order_reverse, error_indicators,
                            ll_delays_current, group_info, date_range)
    prop_new <- calc_proposal_density(sampling_order, error_indicators_new,
                                      ll_delays_new, group_info, date_range)
      
    ratio_prop <- prop_current - prop_new
    
    ratio_post + ratio_prop
  }
  
  # group 2
  group <- 2
  group_info <- model_info$group_info[[group]]
  observed_dates <- rbind(c(NA, NA, 40, 68, NA))
  estimated_dates <- rbind(c(20.5, NA, 40.2, 50.1, NA))
  error_indicators <- rbind(c(NA, NA, FALSE, TRUE, NA))
  
  ## updating error death (date 4) but matching observed date so auto-reject
  sampling_order <- 4
  sampling_order_reverse <- 4
  estimated_dates_new <- rbind(c(20.5, NA, 40.2, 68.1, NA))
  error_indicators_new <- rbind(c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(list(sampling_order), list(sampling_order_reverse),
                     estimated_dates_new, estimated_dates, 
                     error_indicators_new, error_indicators, observed_dates, 
                     prob_error, delay_pars, delay_info, 
                     group_info, date_range),
    -Inf)
  
  ## updating death (date 4) but outside date range so auto-reject
  sampling_order <- 4
  sampling_order_reverse <- 4
  estimated_dates_new <- rbind(c(20.5, NA, 40.2, 150.1, NA))
  error_indicators_new <- rbind(c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(list(sampling_order), list(sampling_order_reverse),
                     estimated_dates_new, estimated_dates, 
                     error_indicators_new, error_indicators, observed_dates, 
                     prob_error, delay_pars, delay_info, 
                     group_info, date_range),
    -Inf)
  
  
  ## updating onset (date 1) but outside date range so auto-reject
  sampling_order <- 1
  sampling_order_reverse <- 1
  estimated_dates_new <- rbind(c(-20.5, NA, 40.2, 50.1, NA))
  error_indicators_new <- rbind(c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(list(sampling_order), list(sampling_order_reverse),
                     estimated_dates_new, estimated_dates, 
                     error_indicators_new, error_indicators, observed_dates, 
                     prob_error, delay_pars, delay_info, 
                     group_info, date_range),
    -Inf)
  
  ## updating onset (date 1) but negative delay resulting so auto-reject
  sampling_order <- 1
  sampling_order_reverse <- 1
  estimated_dates_new <- rbind(c(43.5, NA, 40.2, 50.1, NA))
  error_indicators_new <- rbind(c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(list(sampling_order), list(sampling_order_reverse),
                     estimated_dates_new, estimated_dates, 
                     error_indicators_new, error_indicators, observed_dates, 
                     prob_error, delay_pars, delay_info, 
                     group_info, date_range),
    -Inf)
  
  ## updating onset (date 1)
  sampling_order <- 1
  sampling_order_reverse <- 1
  estimated_dates_new <- rbind(c(10.5, NA, 40.2, 50.1, NA))
  error_indicators_new <- rbind(c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(list(sampling_order), list(sampling_order_reverse),
                     estimated_dates_new, estimated_dates, 
                     error_indicators_new, error_indicators, observed_dates, 
                     prob_error, delay_pars, delay_info, 
                     group_info, date_range),
    calc_accept(sampling_order, sampling_order_reverse, 
                estimated_dates_new, estimated_dates,
                error_indicators_new, error_indicators, group_info))
  
  
  ## updating death (date 4), switching to correct
  sampling_order <- 4
  sampling_order_reverse <- 4
  estimated_dates_new <- rbind(c(20.5, NA, 40.2, 68.1, NA))
  error_indicators_new <- rbind(c(NA, NA, FALSE, FALSE, NA))
  expect_equal(
    calc_accept_prob(list(sampling_order), list(sampling_order_reverse),
                     estimated_dates_new, estimated_dates, 
                     error_indicators_new, error_indicators, observed_dates, 
                     prob_error, delay_pars, delay_info, 
                     group_info, date_range),
    calc_accept(sampling_order, sampling_order_reverse, 
                estimated_dates_new, estimated_dates,
                error_indicators_new, error_indicators, group_info))
  
  ## updating all dates, swapping errors
  updated <- c(1, 3, 4)
  estimated_dates_new <- rbind(c(10.5, NA, 50.2, 68.1, NA))
  error_indicators_new <- rbind(c(NA, NA, TRUE, FALSE, NA))
  sampling_order <-
    calc_batch_sampling_order(group_info$event_order, error_indicators_new,
                              group_info$is_date_connected)
  sampling_order_reverse <-
    calc_batch_sampling_order(group_info$event_order, error_indicators,
                              group_info$is_date_connected)
  expect_equal(
    calc_accept_prob(list(sampling_order), list(sampling_order_reverse),
                     estimated_dates_new, estimated_dates, 
                     error_indicators_new, error_indicators, observed_dates, 
                     prob_error, delay_pars, delay_info, 
                     group_info, date_range),
    calc_accept(sampling_order, sampling_order_reverse, 
                estimated_dates_new, estimated_dates,
                error_indicators_new, error_indicators, group_info))
  
  
  ## group 4
  group <- 4
  group_info <- model_info$group_info[group]
  observed_dates <- rbind(c(10, 5, 30, NA, NA))
  estimated_dates <- rbind(c(10.3, 15.4, 30.2, 40.1, NA))
  error_indicators <- rbind(c(FALSE, TRUE, FALSE, NA, NA))
  
  ## updating all dates, swapping errors
  estimated_dates_new <- rbind(c(2.3, 5.2, 33.2, 40.1, NA))
  error_indicators_new <- rbind(c(TRUE, FALSE, TRUE, NA, NA))
  sampling_order <-
    calc_batch_sampling_order(group_info$event_order, error_indicators_new,
                              group_info$is_date_connected)
  sampling_order_reverse <-
    calc_batch_sampling_order(group_info$event_order, error_indicators,
                              group_info$is_date_connected)
  expect_equal(
    calc_accept_prob(list(sampling_order), list(sampling_order_reverse),
                     estimated_dates_new, estimated_dates, 
                     error_indicators_new, error_indicators, observed_dates, 
                     prob_error, delay_pars, delay_info, 
                     group_info, date_range),
    calc_accept(sampling_order, sampling_order_reverse, 
                estimated_dates_new, estimated_dates,
                error_indicators_new, error_indicators, group_info))
})

test_that("acceptance probability is positive for a better fitting proposed state", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  delay_info <- model_info$delay_info
  
  ## Set tight delay distributions so that moving report closer to the
  ## expected onset -> report delay gives a clearly better fit.
  delay_pars <- list(list(mean = 10, shape = 10),
                     list(mean = 10, shape = 10),
                     list(mean = 10, shape = 10),
                     list(mean = 10, shape = 10),
                     list(meanlog = log(10), precisionlog = 10),
                     list(meanlog = log(10), precisionlog = 10))
  
  prob_error <- 0.05
  date_range <- c(0, 1000)
  group <- 2
  group_info <- model_info$group_info[[group]]
  
  observed_dates <- rbind(c(NA, NA, 102, 110, NA))
  
  ## Current: onset -> report delay is 2 days
  estimated_dates <- rbind(c(100.0, NA, 102.0, 110.0, NA))
  error_indicators <- rbind(c(NA, NA, FALSE, FALSE, NA))
  
  ## Proposed: onset -> report delay is 10 days
  estimated_dates_new <- rbind(c(100.0, NA, 110.0, 110.0, NA))
  error_indicators_new <- rbind(c(NA, NA, FALSE, FALSE, NA))
  
  sampling_order <- list(3)
  sampling_order_reverse <- list(3)
  
  log_accept_ratio <- calc_accept_prob(
    sampling_order,
    sampling_order_reverse,
    estimated_dates_new,
    estimated_dates,
    error_indicators_new,
    error_indicators,
    observed_dates,
    prob_error,
    delay_pars,
    delay_info,
    group_info,
    date_range
  )
  
  expect_gt(log_accept_ratio, 0)
})

test_that("has_mixed_errors returns TRUE only when both TRUE and FALSE 
          present", {
  # Only non-errors
  expect_false(has_mixed_errors(c(FALSE, FALSE)))
  expect_false(has_mixed_errors(c(FALSE, FALSE, FALSE)))
  
  # Only errors
  expect_false(has_mixed_errors(c(TRUE, TRUE)))
  
  # Only missing
  expect_false(has_mixed_errors(c(NA, NA)))
  
  # Non-errors and missing (no TRUE)
  expect_false(has_mixed_errors(c(FALSE, NA)))
  expect_false(has_mixed_errors(c(FALSE, FALSE, NA)))
  
  # Errors and missing (no FALSE)
  expect_false(has_mixed_errors(c(TRUE, NA)))
  expect_false(has_mixed_errors(c(TRUE, TRUE, NA)))
  
  # Mixed: both FALSE and TRUE present (NA irrelevant)
  expect_true(has_mixed_errors(c(FALSE, TRUE)))
  expect_true(has_mixed_errors(c(FALSE, TRUE, NA)))
  expect_true(has_mixed_errors(c(TRUE, FALSE, FALSE)))
  expect_true(has_mixed_errors(c(NA, FALSE, TRUE, NA)))
})
