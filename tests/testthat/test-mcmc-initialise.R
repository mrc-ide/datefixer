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

quantile_range <- c(0.01, 0.99)

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
