

test_that("delay info is setup correctly", {
  delay_map <- data.frame(
    from = c("onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = I(list(1:4, 2, 3, 3, 4, 4))
  )
  
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  
  delay_info <- make_delay_info(delay_map, dates)
  
  delay_from <- c(1, 1, 1, 2, 1, 2)
  delay_to <- c(3, 4, 2, 5, 2, 4)
  expect_equal(delay_info$from, delay_from)
  expect_equal(delay_info$to, delay_to)
  expect_equal(dim(delay_info$is_delay_in_group), c(6, 4))
  expect_equal(delay_info$is_delay_in_group,
               rbind(c(TRUE, TRUE, TRUE, TRUE),
                     c(FALSE, TRUE, FALSE, FALSE),
                     c(FALSE, FALSE, TRUE, FALSE),
                     c(FALSE, FALSE, TRUE, FALSE),
                     c(FALSE, FALSE, FALSE, TRUE),
                     c(FALSE, FALSE, FALSE, TRUE)))
  
  expect_equal(dim(delay_info$is_date_in_delay), c(5, 6, 4))
  ## easier to test this by checking the number of TRUE/FALSE is correct,
  ## then checked the TRUEs, we do this by delay
  for (i in seq_len(nrow(delay_map))) {
    ## 2 dates per group related to the delay
    groups <- unlist(delay_map$group[i])
    n_true <- 2 * length(groups)
    expect_equal(sum(delay_info$is_date_in_delay[, i, ]), n_true)
    n_false <- 20 - n_true
    expect_equal(sum(!delay_info$is_date_in_delay[, i, ]), n_false)
    expect_true(all(
      delay_info$is_date_in_delay[c(delay_from[i], delay_to[i]), i, groups]))
  }
  
})

test_that("date range is calculated correctly", {
  data <- toy_model()$data
  
  observed_dates <- observed_dates_to_int(data$observed_data)
  min_date <- min(observed_dates, na.rm = TRUE)
  max_date <- max(observed_dates, na.rm = TRUE)
  
  ## Note there is always  +1 on the right hand side to include the one-day
  ## period of the latest possible date
  control <- mcmc_control()
  ## default date buffer is 30 days
  expect_equal(calc_date_range(observed_dates, control), 
               c(min_date - 30, max_date + 30 + 1)) 

  control <- mcmc_control(date_buffer = 45)
  expect_equal(calc_date_range(observed_dates, control), 
               c(min_date - 45, max_date + 45 + 1))
  
  control <- mcmc_control(earliest_possible_date = "2025-02-01")
  expect_equal(calc_date_range(observed_dates, control), 
               c(date_to_int("2025-02-01"), max_date + 30 + 1))
  
  control <- mcmc_control(latest_possible_date = "2025-10-01")
  expect_equal(calc_date_range(observed_dates, control), 
               c(min_date - 30, date_to_int("2025-10-01") + 1))
  
  control <- mcmc_control(earliest_possible_date = "2025-02-01",
                          latest_possible_date = "2025-10-01")
  expect_equal(calc_date_range(observed_dates, control), 
               c(date_to_int("2025-02-01"), date_to_int("2025-10-01") + 1))
})

