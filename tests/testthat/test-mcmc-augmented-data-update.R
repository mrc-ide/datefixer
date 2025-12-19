test_that("resampling order calculated correctly", {
  delay_map <- data.frame(
    from = c("onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = I(list(1:4, 2, 3, 3, 4, 4))
  )
  
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
