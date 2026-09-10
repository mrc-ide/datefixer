test_that("chronofix_get_delays securely calls validation helper", {
  expect_error(chronofix_get_delays(), "is missing")
})

test_that("chronofix_get_delays returns posterior delay summaries", {
  mock <- make_delay_summary_mock()
  
  result <- chronofix_get_delays(
    mcmc_output = mock$mcmc_output,
    delay_map = mock$delay_map
  )
  
  expect_s3_class(result, "data.frame")
  # 2 delays * 4 parameters each = 8 rows total
  expect_equal(nrow(result), 8)
  
  expect_equal(
    names(result),
    c("Group",
      "Delay",
      "Distribution",
      "Parameter",
      "Posterior_Mean",
      "Posterior_Median",
      "Lower_95_CrI",
      "Upper_95_CrI")
  )
  
  expect_equal(result$Group[1], "Community Alive")
  expect_equal(result$Group[5], "Hospitalised Alive, Hospitalised Dead")
  
  expect_equal(result$Delay[1], "Onset to Report")
  expect_equal(result$Delay[5], "Hospitalisation to Discharge")
  
  expect_equal(result$Distribution, c(rep("Gamma", 4), rep("Log-Normal", 4)))
  
  mean_rows <- subset(result, Parameter == "Mean")
  cv_rows <- subset(result, Parameter == "CV")
  
  # calculated expected from mock data
  expected_gamma_cv <- round(1 / sqrt(6), 3) # median of 1/sqrt(c(2, 4, 6, 8, 10))
  expected_ln_mean <- round(exp(3 + (1 / 3) / 2), 3)
  expected_ln_cv <- round(sqrt(exp(1 / 3) - 1), 3)
  
  expect_equal(mean_rows$Posterior_Median, c(3, expected_ln_mean))
  expect_equal(cv_rows$Posterior_Median, c(expected_gamma_cv, expected_ln_cv))
})

test_that("chronofix_get_delays calculates rounded 95% credible intervals", {
  mock <- make_delay_summary_mock()
  
  result <- chronofix_get_delays(
    mcmc_output = mock$mcmc_output,
    delay_map = mock$delay_map
  )
  
  expected_mean1 <- round(
    stats::quantile(c(1, 2, 3, 4, 5),
                    probs = c(0.025, 0.5, 0.975),
                    names = FALSE),
    3
  )
  expected_cv1 <- round(
    stats::quantile(1 / sqrt(c(2, 4, 6, 8, 10)),
                    probs = c(0.025, 0.5, 0.975),
                    names = FALSE),
    3
  )
  expected_mean1_post_mean <- round(mean(c(1, 2, 3, 4, 5)), 3)
  
  expect_equal(result$Lower_95_CrI[1], expected_mean1[1])
  expect_equal(result$Posterior_Median[1], expected_mean1[2])
  expect_equal(result$Upper_95_CrI[1], expected_mean1[3])
  expect_equal(result$Posterior_Mean[1], expected_mean1_post_mean)
  
  expect_equal(result$Lower_95_CrI[2], expected_cv1[1])
  expect_equal(result$Posterior_Median[2], expected_cv1[2])
  expect_equal(result$Upper_95_CrI[2], expected_cv1[3])
})

test_that("chronofix_get_delays handles NAs in MCMC output safely", {
  mock <- make_delay_summary_mock()
  mock$mcmc_output$pars["delay1_mean", 1] <- NA
  
  result <- chronofix_get_delays(mock$mcmc_output, mock$delay_map)
  
  mean_row_1 <- subset(result, Delay == "Onset to Report" & Parameter == "Mean")
  
  expect_false(is.na(mean_row_1$Posterior_Median))
  expect_false(is.na(mean_row_1$Posterior_Mean))
  # median and mean of c(NA, 2, 3, 4, 5) should be 3.5
  expect_equal(mean_row_1$Posterior_Median, 3.5)
  expect_equal(mean_row_1$Posterior_Mean, 3.5)
})
