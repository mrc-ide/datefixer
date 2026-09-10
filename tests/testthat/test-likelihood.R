
test_that("error log-likelihood calculated correctly", {
  
  date_range <- c(500, 650)
  prob_error <- 0.2
  
  error_indicators <- rbind(c(TRUE, FALSE, NA, FALSE),
                            c(FALSE, TRUE, FALSE, TRUE),
                            c(FALSE, NA, NA, FALSE))
  n_non_errors <- rowSums(!is.na(error_indicators) & error_indicators == FALSE)
  n_errors <- rowSums(!is.na(error_indicators) & error_indicators == TRUE)
  
  ## prob_error of each error and 1 - prob_error of each non-error
  ## missing dates have no impact here!
  ## for each error the date is then drawn at random from the range of dates
  ## excluding the observed date
  ll_expected <- n_errors * log(prob_error) + 
    n_non_errors * log(1 - prob_error) -
    n_errors * log(diff(date_range) - 1)
  
  expect_equal(
    chronofix_log_likelihood_errors(prob_error, error_indicators, date_range),
    ll_expected)
})


test_that("log density delay calculated correctly", {
  ## gamma distribution
  pars <- list(mean = 3,
               shape = 2)
  rate <- pars$shape / pars$mean

  expect_equal(log_density_delay(8, pars, "gamma"),
               dgamma(8, pars$shape, rate = rate, log = TRUE))
  
  ## log-normal distribution
  pars <- list(meanlog = log(3),
               precisionlog = 1 / 2)
  sdlog <- 1 / sqrt(pars$precisionlog)
  expect_equal(log_density_delay(8, pars, "log-normal"),
               dlnorm(8, pars$meanlog, sdlog, log = TRUE))
})


test_that("individual delay log-likelihood calculated correctly", {
  
  delay_map <- data.frame(
    from = c("onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = I(list(1:4, 2, 3, 3, 4, 4)),
    distribution = c("gamma", "gamma", "gamma", "gamma", "log-normal",
                     "log-normal")
  )
  
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  
  model_info <- make_model_info(delay_map, dates)
  delay_info <- model_info$delay_info
  group_info <- model_info$group_info
  
  delay_pars <- list(list(mean = 8, shape = 4),
                     list(mean = 5, shape = 3),
                     list(mean = 3.2, shape = 1.5),
                     list(mean = 6.4, shape = 2.7),
                     list(meanlog = 2.5, precisionlog = 0.3),
                     list(meanlog = 1.8, precisionlog = 0.8))
  
  calc_ll_expected <- function(d, group) {
    k <- group_info[[group]]$is_delay_in_group
    ll_expected <- rep(0, length(delay_pars))
    delay_values <- d[delay_info$to] - d[delay_info$from]
    ll_expected[k] <- mapply(log_density_delay, delay_values[k],
                             delay_pars[k], delay_info$distribution[k])
    ll_expected
  }
  
  ## group 1, onset & report
  estimated_dates <- c(3.5, NA, 6.5, NA, NA)
  ll <- chronofix_log_likelihood_delays1(estimated_dates, delay_pars, 
                                         delay_info,
                                         group_info[[1]]$is_delay_in_group)
  expect_equal(ll, calc_ll_expected(estimated_dates, 1))
  
  ## group 1, onset & report, negative delay
  estimated_dates <- c(3.5, NA, 2.5, NA, NA)
  ll <- chronofix_log_likelihood_delays1(estimated_dates, delay_pars,
                                         delay_info,
                                         group_info[[1]]$is_delay_in_group)
  expect_equal(sum(ll), -Inf)
  
  
  ## group 2, onset, report & death
  estimated_dates <- c(3.5, NA, 6.5, 7, NA)
  ll <- chronofix_log_likelihood_delays1(estimated_dates, delay_pars,
                                         delay_info,
                                         group_info[[2]]$is_delay_in_group)
  expect_equal(ll, calc_ll_expected(estimated_dates, 2))
  
  ## group 2, onset, report & death, negative delay
  estimated_dates <- c(3.5, NA, 6.5, 1, NA)
  ll <- chronofix_log_likelihood_delays1(estimated_dates, delay_pars,
                                         delay_info,
                                         group_info[[2]]$is_delay_in_group)
  expect_equal(sum(ll), -Inf)
  
  
  ## group 3, onset, report, hospitalisation & discharge
  estimated_dates <- c(3.5, 8.4, 6.5, NA, 12.1)
  ll <- chronofix_log_likelihood_delays1(estimated_dates, delay_pars,
                                         delay_info,
                                         group_info[[3]]$is_delay_in_group)
  expect_equal(ll, calc_ll_expected(estimated_dates, 3))
  
  ## group 3, onset, report, hospitalisation & discharge, negative delay
  estimated_dates <- c(3.5, 8.4, 6.5, NA, 7.3)
  ll <- chronofix_log_likelihood_delays1(estimated_dates, delay_pars,
                                         delay_info,
                                         group_info[[3]]$is_delay_in_group)
  expect_equal(sum(ll), -Inf)
  
  
  ## group 4, onset, report, hospitalisation & death
  estimated_dates <- c(3.5, 8.4, 6.5, 12.1, NA)
  ll <- chronofix_log_likelihood_delays1(estimated_dates, delay_pars,
                                         delay_info,
                                         group_info[[4]]$is_delay_in_group)
  expect_equal(ll, calc_ll_expected(estimated_dates, 4))
  
  ## group 3, onset, report, hospitalisation & discharge, negative delay
  estimated_dates <- c(3.5, 8.4, 6.5, 7.3, NA)
  ll <- chronofix_log_likelihood_delays1(estimated_dates, delay_pars,
                                         delay_info,
                                         group_info[[4]]$is_delay_in_group)
  expect_equal(sum(ll), -Inf)
})


test_that("log-likelihood aggregates correctly", {
  control <- chronofix_mcmc_control()
  toy <- toy_model(control = control)
  model <- toy$model
  data <- toy$data
  delay_map <- toy$delay_map
  
  ## split the model into the prior and likelihood
  model_split <- monty::monty_model_split(model)
  has_augmented_data <- 
    unlist(lapply(model_split, function (m) m$properties$has_augmented_data))
  model_likelihood <- model_split[[which(has_augmented_data)]]
  model_prior <- model_split[[which(!has_augmented_data)]]
  
  prob_error <- 0.08
  delay_pars <- list(list(mean = 8, shape = 4),
                     list(mean = 5, shape = 3),
                     list(mean = 3.2, shape = 1.5),
                     list(mean = 6.4, shape = 2.7),
                     list(meanlog = 2.5, precisionlog = 0.3),
                     list(meanlog = 1.8, precisionlog = 0.8))
  
  delay_pars_flat <- unlist(delay_pars)
  names(delay_pars_flat) <- 
    paste0("delay", rep(seq_len(6), each = 2), "_", names(delay_pars_flat))
  
  pars <- c(prob_error = prob_error, delay_pars_flat)
  pars <- unname(pars[model$parameters])
  
  ## use true data and error indicators from simulated data as
  ## estimated dates and error indicators in augmented data respectively
  ## remove first two columns (id and group)
  estimated_dates <- as.matrix(data$true_data[, -c(1, 2)])
  error_indicators <- as.matrix(data$error_indicators[, -c(1, 2)])
  
  augmented_data <- 
    model$data_packer$pack(list(estimated_dates = estimated_dates,
                                error_indicators = error_indicators))
  attr(pars, "data") <- augmented_data
  ll_aggregated <- model_likelihood$density(pars)
  
  
  ## now calculate all ll parts
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  date_range <- 
    calc_date_range(observed_dates_to_int(data$observed_data), control)
  
  ## error log-likelihood by row
  ll_errors <- 
    chronofix_log_likelihood_errors(prob_error, error_indicators, date_range)
  
  ## delay log-likelihood by row (and delay)
  calc_ll_delay1 <- function(i) {
    group <- which(model_info$groups == data$true_data$group[i])
    chronofix_log_likelihood_delays1(
      estimated_dates[i, ], delay_pars, model_info$delay_info,
      model_info$group_info[[group]]$is_delay_in_group)
  }
  ll_delays <- vapply(seq_len(nrow(estimated_dates)), 
                      calc_ll_delay1, numeric(length(delay_pars)))
  
  ## sum over delay and error log-likelihoods, check it equals aggregated
  expect_equal(ll_aggregated, sum(ll_errors) + sum(ll_delays))
  
  
  ## check overall density is prior density + likelihood density
  prior <- model_prior$density(pars)
  expect_equal(model$density(pars), ll_aggregated + prior)
})
