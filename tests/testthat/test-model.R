
test_that("model info is setup correctly", {
  delay_map <- data.frame(
    from = c("onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = I(list(1:4, 2, 3, 3, 4, 4)),
    distribution = c("gamma", "gamma", "gamma", "gamma",
                     "log-normal", "log-normal")
  )
  
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  
  model_info <- make_model_info(delay_map, dates)
  
  expect_equal(names(model_info),
               c("delay_info", "group_info", "groups"))
  
  delay_from <- c(1, 1, 1, 2, 1, 2)
  delay_to <- c(3, 4, 2, 5, 2, 4)
  delay_distribution <- c("gamma", "gamma", "gamma", "gamma", 
                          "log-normal", "log-normal")
  expect_equal(model_info$delay_info$from, delay_from)
  expect_equal(model_info$delay_info$to, delay_to)
  expect_equal(model_info$delay_info$distribution, delay_distribution)
  expect_equal(model_info$groups, c(1, 2, 3, 4))
  
  expect_equal(model_info$group_info[[1]]$is_delay_in_group,
               c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(model_info$group_info[[2]]$is_delay_in_group,
               c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(model_info$group_info[[3]]$is_delay_in_group,
               c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(model_info$group_info[[4]]$is_delay_in_group,
               c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
               
  
  is_date_in_delay <- array(FALSE, c(5, 6))
  ## onset to report for all groups
  is_date_in_delay[c(1, 3), 1] <- TRUE
  expect_equal(model_info$group_info[[1]]$is_date_in_delay, is_date_in_delay)
  
  is_date_in_delay2 <- is_date_in_delay
  ## onset to death for group 2
  is_date_in_delay2[c(1, 4), 2] <- TRUE
  expect_equal(model_info$group_info[[2]]$is_date_in_delay, is_date_in_delay2)
  
  is_date_in_delay3 <- is_date_in_delay
  ## onset to hospitalisation and hospitalisation to discharge for group 3
  is_date_in_delay3[c(1, 2), 3] <- TRUE
  is_date_in_delay3[c(2, 5), 4] <- TRUE
  expect_equal(model_info$group_info[[3]]$is_date_in_delay, is_date_in_delay3)
  
  is_date_in_delay4 <- is_date_in_delay
  ## onset to hospitalisation and hospitalisation to death for group 4
  is_date_in_delay4[c(1, 2), 5] <- TRUE
  is_date_in_delay4[c(2, 4), 6] <- TRUE
  expect_equal(model_info$group_info[[4]]$is_date_in_delay, is_date_in_delay4)
  
  
  expect_equal(model_info$group_info[[1]]$is_date_in_group, 
               c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(model_info$group_info[[2]]$is_date_in_group, 
               c(TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(model_info$group_info[[3]]$is_date_in_group, 
               c(TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(model_info$group_info[[4]]$is_date_in_group, 
               c(TRUE, TRUE, TRUE, TRUE, FALSE))
  
  
  is_date_connected <- array(FALSE, c(5, 5))
  ## onset to report for all groups
  is_date_connected[1, 3] <- TRUE
  is_date_connected[3, 1] <- TRUE
  expect_equal(model_info$group_info[[1]]$is_date_connected, is_date_connected)
  
  is_date_connected2 <- is_date_connected
  ## onset to death for group 2
  is_date_connected2[1, 4] <- TRUE
  is_date_connected2[4, 1] <- TRUE
  expect_equal(model_info$group_info[[2]]$is_date_connected, is_date_connected2)
  
  ## onset to hospitalisation for groups 3/4
  is_date_connected[1, 2] <- TRUE
  is_date_connected[2, 1] <- TRUE
  
  is_date_connected3 <- is_date_connected
  ## hospitalisation to discharge for group 3
  is_date_connected3[2, 5] <- TRUE
  is_date_connected3[5, 2] <- TRUE
  expect_equal(model_info$group_info[[3]]$is_date_connected, is_date_connected3)
  
  is_date_connected4 <- is_date_connected
  ## hospitalisation to death for group 4
  is_date_connected4[2, 4] <- TRUE
  is_date_connected4[4, 2] <- TRUE
  expect_equal(model_info$group_info[[4]]$is_date_connected, is_date_connected4)
  
  
  expect_equal(model_info$group_info[[1]]$event_order, c(1, 3))
  expect_equal(model_info$group_info[[2]]$event_order, c(1, 3, 4))
  expect_equal(model_info$group_info[[3]]$event_order, c(1, 2, 3, 5))
  expect_equal(model_info$group_info[[4]]$event_order, c(1, 2, 3, 4))
  
  shortest_paths <- rep(list(NULL), 5)
  # group 1, onset/report
  shortest_paths1 <- shortest_paths
  shortest_paths1[[1]] <- list(NULL, NULL, c(1, 3), NULL, NULL)
  shortest_paths1[[3]] <- list(c(3, 1), NULL, NULL, NULL, NULL)
  expect_equal(model_info$group_info[[1]]$shortest_paths, shortest_paths1)
  # group 2, onset/death/report
  shortest_paths2 <- shortest_paths
  shortest_paths2[[1]] <- list(NULL, NULL, c(1, 3), c(1, 4), NULL)
  shortest_paths2[[3]] <- list(c(3, 1), NULL, NULL, c(3, 1, 4), NULL)
  shortest_paths2[[4]] <- list(c(4, 1), NULL, c(4, 1, 3), NULL, NULL)
  expect_equal(model_info$group_info[[2]]$shortest_paths, shortest_paths2)
  # group 3, onset/hospitalisation/discharge/report
  shortest_paths3 <- shortest_paths
  shortest_paths3[[1]] <- list(NULL, c(1, 2), c(1, 3), NULL, c(1, 2, 5))
  shortest_paths3[[2]] <- list(c(2, 1), NULL, c(2, 1, 3), NULL, c(2, 5))
  shortest_paths3[[3]] <- 
    list(c(3, 1), c(3, 1, 2), NULL, NULL, c(3, 1, 2, 5))
  shortest_paths3[[5]] <- 
    list(c(5, 2, 1), c(5, 2), c(5, 2, 1, 3), NULL, NULL)
  expect_equal(model_info$group_info[[3]]$shortest_paths, shortest_paths3)
  # group 4, onset/hospitalisation/death/report
  shortest_paths4 <- shortest_paths
  shortest_paths4[[1]] <- list(NULL, c(1, 2), c(1, 3), c(1, 2, 4), NULL)
  shortest_paths4[[2]] <- list(c(2, 1), NULL, c(2, 1, 3), c(2, 4), NULL)
  shortest_paths4[[3]] <- 
    list(c(3, 1), c(3, 1, 2), NULL, c(3, 1, 2, 4), NULL)
  shortest_paths4[[4]] <- 
    list(c(4, 2, 1), c(4, 2), c(4, 2, 1, 3), NULL, NULL)
  expect_equal(model_info$group_info[[4]]$shortest_paths, shortest_paths4)
  
  ## now setup with named groups
  groups <- c("community_alive", "community_dead", "hospitalised_alive",
              "hospitalised_dead")
  delay_map2 <- data.frame(
    from = c("onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = I(list(groups[1:4], groups[2], groups[3], groups[3],
                   groups[4], groups[4])),
    distribution = c("gamma", "gamma", "gamma", "gamma",
                     "log-normal", "log-normal")
  )
  
  model_info2 <- make_model_info(delay_map2, dates)
  ## everything should be the same except for groups
  expect_identical(model_info2[names(model_info2) != "groups"],
                   model_info[names(model_info) != "groups"])
  expect_equal(model_info2$groups, groups)
  
  
  
  ## named groups, but names are not in alphabetical order
  groups <- c("alive_community", "dead_community", "alive_hospitalised",
              "dead_hospitalised")
  delay_map3 <- data.frame(
    from = c("onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = I(list(groups[1:4], groups[2], groups[3], groups[3],
                   groups[4], groups[4])),
    distribution = c("gamma", "gamma", "gamma", "gamma",
                     "log-normal", "log-normal")
  )
  
  model_info3 <- make_model_info(delay_map3, dates)
  ## groups would be sorted into alphabetical order
  expect_equal(model_info3$groups, sort(groups))
  ## elements without group dimension should remain unchanged
  unchanged <- c("delay_from", "delay_to", "delay_distribution")
  expect_identical(model_info3[unchanged], model_info[unchanged])
  ## elements with a group dimension we expect order to have changed
  g <- match(groups, model_info3$groups)
  expect_identical(model_info3$group_info[g], model_info$group_info)
})


test_that("data and delays are validated correctly", {
  toy <- toy_model()
  data <- toy$data$observed_data
  delay_map <- toy$delay_map
  
  x <- validate_data_and_delays(data, delay_map)
  
  dates <- setdiff(names(data), c("id", "group"))
  model_info <- make_model_info(delay_map, dates)
  expect_identical(x$model_info, model_info)
  expect_equal(x$observed_dates, observed_dates_to_int(data))
  expect_equal(x$groups, match(data$group, model_info$groups))
  
})


test_that("data and delays are validated correctly without groups", {
  toy <- toy_model(named_groups = FALSE)
  data <- toy$data$observed_data
  delay_map <- toy$delay_map
  
  ## only use group 3
  data <- data[data$group == 3, ]
  data$group <- NULL
  data$death <- NULL
  delays_to_keep <- unlist(lapply(delay_map$group, function(x) 3 %in% x))
  delay_map <- delay_map[delays_to_keep, ]
  delay_map$group <- NULL
  
  x <- validate_data_and_delays(data, delay_map)
  
  dates <- setdiff(names(data), c("id", "group"))
  delay_map_with_group <- delay_map
  delay_map$group <- 1
  model_info <- make_model_info(delay_map, dates)
  expect_identical(x$model_info, model_info)
  expect_equal(model_info$delay_info$from, match(delay_map$from, dates))
  expect_equal(model_info$delay_info$to, match(delay_map$to, dates))
  
  expect_equal(length(model_info$group_info), 1)
  group_info <- model_info$group_info[[1]]
  expect_equal(group_info$is_delay_in_group, rep(TRUE, 3))
  expect_equal(group_info$is_date_in_delay,
               rbind(c(TRUE, TRUE, FALSE),
                     c(FALSE, TRUE, TRUE),
                     c(TRUE, FALSE, FALSE),
                     c(FALSE, FALSE, TRUE)))
  expect_equal(group_info$is_date_in_group, rep(TRUE, 4))
  expect_equal(group_info$is_date_connected,
               rbind(c(FALSE, TRUE, TRUE, FALSE),
                     c(TRUE, FALSE, FALSE, TRUE),
                     c(TRUE, FALSE, FALSE, FALSE),
                     c(FALSE, TRUE, FALSE, FALSE)))
  expect_equal(group_info$event_order, c(1, 2, 3, 4))
  expect_equal(model_info$groups, 1)
  expect_equal(x$observed_dates, observed_dates_to_int(data))
  expect_equal(x$groups, rep(1, nrow(data)))
})


test_that("Error when data and delay_map have different groups", {
  toy <- toy_model(named_groups = FALSE)
  data <- toy$data$observed_data
  delay_map <- toy$delay_map

  ## No group column in data
  data_no_group <- data[, names(data) != "group"]
  expect_error(validate_data_and_delays(data_no_group, delay_map),
               "Expected 'group' column in 'data'")
  
  ## No group column in delay_map
  delay_map_no_group <- delay_map[, names(delay_map) != "group"]
  expect_error(validate_data_and_delays(data, delay_map_no_group),
               "Expected 'group' column in 'delay_map'")
  
  ## data missing group 4
  data_no_group_4 <- data[data$group != 4, ]
  expect_error(validate_data_and_delays(data_no_group_4, delay_map),
               "Groups in 'data'")
  
  ## delay_map missing group 4
  is_not_group_4 <- 
    unlist(lapply(delay_map$group, function(x) !identical(x, 4)))
  delay_map_no_group_4 <- delay_map[is_not_group_4, ]
  delay_map_no_group_4$group <- lapply(delay_map_no_group_4$group,
                                       function (x) setdiff(x, 4))
  expect_error(validate_data_and_delays(data, delay_map_no_group_4),
               "Groups in 'data'")
  
  ## data has named groups, but numbered groups in delay_map
  data_named_groups <- data
  group_names <- c("a", "b", "c", "d")
  data_named_groups$group <- group_names[data_named_groups$group]
  expect_error(validate_data_and_delays(data_named_groups, delay_map),
               "Groups in 'data'")
})

test_that("validate_events correctly flags missing columns and date errors", {
  toy <- toy_model()
  data <- toy$data$observed_data
  delay_map <- toy$delay_map
  
  # missing id column in data
  data_no_id <- data
  data_no_id$id <- NULL
  expect_error(
    validate_data_and_delays(data_no_id, delay_map),
    "must contain an `id` column"
  )
  
  # NA ids
  data_na_id <- data
  data_na_id$id[3] <- NA
  
  expect_error(
    validate_data_and_delays(data_na_id, delay_map),
    "cannot contain missing values"
  )
  
  # duplicated ids
  data_duplicate_id <- data
  data_duplicate_id$id[2] <- data_duplicate_id$id[1]
  
  expect_error(
    validate_data_and_delays(data_duplicate_id, delay_map),
    "must contain unique values"
  )
  
  # missing column in data
  data_missing_col <- data
  data_missing_col$report <- NULL 
  expect_error(
    validate_data_and_delays(data_missing_col, delay_map),
    "must exist as columns in `data`"
  )
  
  # unmapped event column in data
  data_extra_col <- data
  data_extra_col$symptom_resolution <- Sys.Date()
  expect_error(
    validate_data_and_delays(data_extra_col, delay_map),
    "must be mapped in `delay_map`"
  )
  
  # individual has all NA dates
  data_all_na <- data
  event_cols <- setdiff(names(data), c("id", "group"))
  data_all_na[1, event_cols] <- NA # row 1 dates set to all NA
  expect_error(
    validate_data_and_delays(data_all_na, delay_map),
    "cannot have `NA` for all event dates"
  )
  
  # invalid date for an individual's group
  data_invalid_date <- data
  # give a 'community-alive' person an invalid 'discharge' date
  comm_idx <- which(data_invalid_date$group == "community-alive")[1]
  data_invalid_date$discharge[comm_idx] <- as.Date("2026-01-01") 
  expect_error(
    validate_data_and_delays(data_invalid_date, delay_map),
    "events not associated with their group"
  )
})

test_that("date range is calculated correctly", {
  data <- toy_model()$data
  
  observed_dates <- observed_dates_to_int(data$observed_data)
  min_date <- min(observed_dates, na.rm = TRUE)
  max_date <- max(observed_dates, na.rm = TRUE)
  
  ## Note there is always  +1 on the right hand side to include the one-day
  ## period of the latest possible date
  control <- chronofix_mcmc_control()
  ## default date buffer is 30 days
  expect_equal(calc_date_range(observed_dates, control), 
               c(min_date - 30, max_date + 30 + 1)) 

  control <- chronofix_mcmc_control(date_buffer = 45)
  expect_equal(calc_date_range(observed_dates, control), 
               c(min_date - 45, max_date + 45 + 1))
  
  control <- chronofix_mcmc_control(earliest_possible_date = "2025-02-01")
  expect_equal(calc_date_range(observed_dates, control), 
               c(date_to_int("2025-02-01"), max_date + 30 + 1))
  
  control <- chronofix_mcmc_control(latest_possible_date = "2025-10-01")
  expect_equal(calc_date_range(observed_dates, control), 
               c(min_date - 30, date_to_int("2025-10-01") + 1))
  
  control <- chronofix_mcmc_control(earliest_possible_date = "2025-02-01",
                          latest_possible_date = "2025-10-01")
  expect_equal(calc_date_range(observed_dates, control), 
               c(date_to_int("2025-02-01"), date_to_int("2025-10-01") + 1))
})


test_that("convert_to_distribution_params converts correctly", {
  
  mean <- 3
  cv <- 2
  
  ## gamma distribution
  params <- convert_to_distribution_params(mean, cv, "gamma")
  expect_equal(params$shape, 1 / cv^2)
  expect_equal(params$rate, 1 / (mean * cv^2))
  expect_equal(params$shape / params$rate, mean)
  expect_equal(1 / sqrt(params$shape), cv)
  
  ## log-normal distribution
  params <- convert_to_distribution_params(mean, cv, "log-normal")
  expect_equal(params$sdlog, sqrt(log(cv^2 + 1)))
  expect_equal(params$meanlog, log(mean) - log(cv^2 + 1) / 2)
  expect_equal(exp(params$meanlog + params$sdlog^2 / 2), mean)
  expect_equal(sqrt(exp(params$sdlog^2) - 1), cv)
  
  ## unsupported distribution
  expect_error(convert_to_distribution_params(mean, cv, "normal"),
               'Distribution "normal" is not supported')
})


test_that("dinvgamma calculates correctly", {
  shape <- 5
  scale <- 3
  x <- 10
  
  d <- dgamma(1 / x, shape, rate = scale, log = TRUE) - 2 * log(x)
  
  expect_equal(dinvgamma(x, shape, scale, log = TRUE), d)
  expect_equal(dinvgamma(x, shape, scale, log = FALSE), exp(d))
})
