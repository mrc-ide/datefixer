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
