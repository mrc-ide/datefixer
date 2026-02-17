
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
               c("delay_from", "delay_to", "delay_distribution",
                 "is_delay_in_group", "is_date_in_delay", "is_date_in_group",
                 "is_date_connected", "event_order", "groups"))
  
  delay_from <- c(1, 1, 1, 2, 1, 2)
  delay_to <- c(3, 4, 2, 5, 2, 4)
  delay_distribution <- c("gamma", "gamma", "gamma", "gamma", 
                          "log-normal", "log-normal")
  expect_equal(model_info$delay_from, delay_from)
  expect_equal(model_info$delay_to, delay_to)
  expect_equal(model_info$delay_distribution, delay_distribution)
  expect_equal(model_info$groups, c(1, 2, 3, 4))
  
  expect_equal(dim(model_info$is_delay_in_group), c(6, 4))
  expect_equal(model_info$is_delay_in_group,
               rbind(c(TRUE, TRUE, TRUE, TRUE),
                     c(FALSE, TRUE, FALSE, FALSE),
                     c(FALSE, FALSE, TRUE, FALSE),
                     c(FALSE, FALSE, TRUE, FALSE),
                     c(FALSE, FALSE, FALSE, TRUE),
                     c(FALSE, FALSE, FALSE, TRUE)))
  
  expect_equal(dim(model_info$is_date_in_delay), c(5, 6, 4))
  ## easier to test this by checking the number of TRUE/FALSE is correct,
  ## then checked the TRUEs, we do this by delay
  for (i in seq_len(nrow(delay_map))) {
    ## 2 dates per group related to the delay
    groups <- unlist(delay_map$group[i])
    n_true <- 2 * length(groups)
    expect_equal(sum(model_info$is_date_in_delay[, i, ]), n_true)
    n_false <- 20 - n_true
    expect_equal(sum(!model_info$is_date_in_delay[, i, ]), n_false)
    expect_true(all(
      model_info$is_date_in_delay[c(delay_from[i], delay_to[i]), i, groups]))
  }
  
  expect_equal(dim(model_info$is_date_in_group), c(5, 4))
  expect_equal(model_info$is_date_in_group,
               rbind(c(TRUE, TRUE, TRUE, TRUE),
                     c(FALSE, FALSE, TRUE, TRUE),
                     c(TRUE, TRUE, TRUE, TRUE),
                     c(FALSE, TRUE, FALSE, TRUE),
                     c(FALSE, FALSE, TRUE, FALSE)))
  
  expect_equal(dim(model_info$is_date_connected), c(5, 5, 4))
  is_date_connected <- array(FALSE, c(5, 5, 4))
  ## onset to report for all groups
  is_date_connected[1, 3, c(1, 2, 3, 4)] <- TRUE
  is_date_connected[3, 1, c(1, 2, 3, 4)] <- TRUE
  ## onset to death for group 2
  is_date_connected[1, 4, 2] <- TRUE
  is_date_connected[4, 1, 2] <- TRUE
  ## onset to hospitalisation for groups 3/4
  is_date_connected[1, 2, c(3, 4)] <- TRUE
  is_date_connected[2, 1, c(3, 4)] <- TRUE
  ## hospitalisation to discharge for group 3
  is_date_connected[2, 5, 3] <- TRUE
  is_date_connected[5, 2, 3] <- TRUE
  ## hospitalisation to death for group 4
  is_date_connected[2, 4, 4] <- TRUE
  is_date_connected[4, 2, 4] <- TRUE
  
  expect_equal(model_info$is_date_connected, is_date_connected)
  
  expect_equal(length(model_info$event_order), 4)
  expect_equal(model_info$event_order,
               list(c(1, 3),
                    c(1, 3, 4),
                    c(1, 2, 3, 5),
                    c(1, 2, 3, 4)))
  
  
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
  expect_equal(model_info3$is_delay_in_group[, g], model_info$is_delay_in_group)
  expect_equal(model_info3$is_date_in_delay[, , g], model_info$is_date_in_delay)
  expect_equal(model_info3$is_date_in_group[, g], model_info$is_date_in_group)
  expect_equal(model_info3$is_date_connected[, , g],
               model_info$is_date_connected)
  expect_equal(model_info3$event_order[g], model_info$event_order)
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
  expect_equal(model_info$delay_from, match(delay_map$from, dates))
  expect_equal(model_info$delay_to, match(delay_map$to, dates))
  expect_equal(model_info$is_delay_in_group, array(TRUE, c(3, 1)))
  expect_equal(dim(model_info$is_date_in_delay), c(4, 3, 1))
  expect_equal(model_info$is_date_in_delay[, , 1],
               rbind(c(TRUE, TRUE, FALSE),
                     c(FALSE, TRUE, TRUE),
                     c(TRUE, FALSE, FALSE),
                     c(FALSE, FALSE, TRUE)))
  expect_equal(model_info$is_date_in_group, array(TRUE, c(4, 1)))
  expect_equal(dim(model_info$is_date_connected), c(4, 4, 1))
  expect_equal(model_info$is_date_connected[, , 1],
               rbind(c(FALSE, TRUE, TRUE, FALSE),
                     c(TRUE, FALSE, FALSE, TRUE),
                     c(TRUE, FALSE, FALSE, FALSE),
                     c(FALSE, TRUE, FALSE, FALSE)))
  expect_equal(model_info$event_order, list(c(1, 2, 3, 4)))
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

