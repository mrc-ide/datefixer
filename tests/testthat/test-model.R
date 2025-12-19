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
