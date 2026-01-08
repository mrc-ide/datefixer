
test_that("error log-likelihood calculated correctly", {
  
  date_range <- c(500, 650)
  prob_error <- 0.2
  
  n_errors <- 3
  n_non_errors <- 4
  n_missing <- 2
  error_indicators <- c(rep(TRUE, n_errors),
                        rep(FALSE, n_non_errors),
                        rep(NA, n_missing))
  
  ## prob_error of each error and 1 - prob_error of each non-error
  ## missing dates have no impact here!
  ## for each error the date is then drawn at random from the range of dates
  ## excluding the observed date
  ll_expected <- n_errors * log(prob_error) + 
    n_non_errors * log(1 - prob_error) -
    n_errors * log(diff(date_range) - 1)
  
  expect_equal(
    datefixer_log_likelihood_errors(prob_error, error_indicators, date_range),
    ll_expected)
})

test_that("individual delay log-likelihood calculated correctly", {
  
  delay_map <- data.frame(
    from = c("onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = I(list(1:4, 2, 3, 3, 4, 4))
  )
  
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  
  delay_info <- make_delay_info(delay_map, dates)
  delay_from <- delay_info$from
  delay_to <- delay_info$to
  is_delay_in_group <- delay_info$is_delay_in_group
  
  mean_delays <- c(8, 5, 3.2, 6.4, 13, 10.7)
  cv_delays <- c(0.2, 0.8, 0.1, 0.4, 0.5, 0.3)
  shape_delays <- 1 / cv_delays^2
  rate_delays <- shape_delays / mean_delays
  
  calc_ll_expected <- function(d, group) {
    ifelse(is_delay_in_group[, group],
           dgamma(d[delay_to] - d[delay_from], shape = shape_delays,
                  rate = rate_delays, log = TRUE),
           0)
  }
  
  ## group 1, onset & report
  estimated_dates <- c(3.5, NA, 6.5, NA, NA)
  ll <- datefixer_log_likelihood_delays1(estimated_dates, mean_delays,
                                         cv_delays, delay_from, delay_to,
                                         is_delay_in_group[, 1])
  expect_equal(ll, calc_ll_expected(estimated_dates, 1))
  
  ## group 1, onset & report, negative delay
  estimated_dates <- c(3.5, NA, 2.5, NA, NA)
  ll <- datefixer_log_likelihood_delays1(estimated_dates, mean_delays,
                                         cv_delays, delay_from, delay_to,
                                         is_delay_in_group[, 1])
  expect_equal(sum(ll), -Inf)
  
  
  ## group 2, onset, report & death
  estimated_dates <- c(3.5, NA, 6.5, 7, NA)
  ll <- datefixer_log_likelihood_delays1(estimated_dates, mean_delays,
                                         cv_delays, delay_from, delay_to,
                                         is_delay_in_group[, 2])
  expect_equal(ll, calc_ll_expected(estimated_dates, 2))
  
  ## group 2, onset, report & death, negative delay
  estimated_dates <- c(3.5, NA, 6.5, 1, NA)
  ll <- datefixer_log_likelihood_delays1(estimated_dates, mean_delays,
                                         cv_delays, delay_from, delay_to,
                                         is_delay_in_group[, 2])
  expect_equal(sum(ll), -Inf)
  
  
  ## group 3, onset, report, hospitalisation & discharge
  estimated_dates <- c(3.5, 8.4, 6.5, NA, 12.1)
  ll <- datefixer_log_likelihood_delays1(estimated_dates, mean_delays,
                                         cv_delays, delay_from, delay_to,
                                         is_delay_in_group[, 3])
  expect_equal(ll, calc_ll_expected(estimated_dates, 3))
  
  ## group 3, onset, report, hospitalisation & discharge, negative delay
  estimated_dates <- c(3.5, 8.4, 6.5, NA, 7.3)
  ll <- datefixer_log_likelihood_delays1(estimated_dates, mean_delays,
                                         cv_delays, delay_from, delay_to,
                                         is_delay_in_group[, 3])
  expect_equal(sum(ll), -Inf)
  
  
  ## group 4, onset, report, hospitalisation & death
  estimated_dates <- c(3.5, 8.4, 6.5, 12.1, NA)
  ll <- datefixer_log_likelihood_delays1(estimated_dates, mean_delays,
                                         cv_delays, delay_from, delay_to,
                                         is_delay_in_group[, 4])
  expect_equal(ll, calc_ll_expected(estimated_dates, 4))
  
  ## group 3, onset, report, hospitalisation & discharge, negative delay
  estimated_dates <- c(3.5, 8.4, 6.5, 7.3, NA)
  ll <- datefixer_log_likelihood_delays1(estimated_dates, mean_delays,
                                         cv_delays, delay_from, delay_to,
                                         is_delay_in_group[, 4])
  expect_equal(sum(ll), -Inf)
})

test_that("log-likelihood aggregates correctly", {
  control <- mcmc_control()
  toy <- toy_model(control)
  model <- toy$model
  data <- toy$data
  delay_map <- toy$delay_map
  
  m <- model$model$data$parts[[1]]
  expect_true(m$properties$has_augmented_data)
  
  prob_error <- 0.08
  mean_delays <- c(8, 5, 3.2, 6.4, 13, 10.7)
  cv_delays <- c(0.2, 0.8, 0.1, 0.4, 0.5, 0.3)
  
  pars <- c(prob_error, mean_delays, cv_delays)
  
  ## use true data and error indicators from simulated data as
  ## estimated dates and error indicators in augmented data respectively
  ## remove first two columns (id and group)
  estimated_dates <- as.matrix(data$true_data[, -c(1, 2)])
  error_indicators <- as.matrix(data$error_indicators[, -c(1, 2)])
  
  augmented_data <- 
    model$data_packer$pack(list(estimated_dates = estimated_dates,
                                error_indicators = error_indicators))
  attr(pars, "data") <- augmented_data
  ll_aggregated <- m$density(pars)
  
  
  ## now calculate all ll parts
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  delay_info <- make_delay_info(delay_map, dates)
  date_range <- 
    calc_date_range(observed_dates_to_int(data$observed_data), control)
  
  ## error log-likelihood by row
  ll_errors <- 
    apply(error_indicators, 1, 
          function(x) datefixer_log_likelihood_errors(prob_error, x, date_range)
    )
  
  ## delay log-likelihood by row (and delay)
  calc_ll_delay1 <- function(i) {
    group <- 
    datefixer_log_likelihood_delays1(
      estimated_dates[i, ], mean_delays, cv_delays, delay_info$from,
      delay_info$to, delay_info$is_delay_in_group[, data$true_data$group[i]])
  }
  ll_delays <- vapply(seq_len(nrow(estimated_dates)), 
                      calc_ll_delay1, numeric(length(mean_delays)))
  
  expect_equal(ll_aggregated, sum(ll_errors) + sum(ll_delays))
  
  
  ## check overall density is prior density + likelihood density
  prior <- model$model$data$parts[[2]]$density(pars)
  expect_equal(model$density(pars), ll_aggregated + prior)
})
