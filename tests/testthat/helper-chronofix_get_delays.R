make_delay_summary_mock <- function() {
  delay_map <- data.frame(
    group = I(list("community-alive",
                   c("hospitalised-alive", "hospitalised-dead"))),
    from = c("onset", "hospitalisation"),
    to = c("report", "discharge"),
    distribution = c("gamma", "lognormal"),
    stringsAsFactors = FALSE
  )
  
  param_names <- c("delay1_mean", "delay1_shape",
                   "delay2_meanlog", "delay2_precisionlog")
  
  pars <- matrix(
    data = NA,
    nrow = 4,
    ncol = 5,
    dimnames = list(param_names, NULL)
  )
  
  pars["delay1_mean", ] <- c(1, 2, 3, 4, 5)
  pars["delay1_shape", ] <- c(2, 4, 6, 8, 10)
  pars["delay2_meanlog", ] <- c(1, 2, 3, 4, 5)
  pars["delay2_precisionlog", ] <- c(3, 3, 3, 3, 3)
  
  list(
    mcmc_output = list(pars = pars),
    delay_map = delay_map
  )
}
