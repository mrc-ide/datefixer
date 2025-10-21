test_that("Can run mcmc", {
  set.seed(1)
  control <- mcmc_control(n_steps = 50, n_chains = 3)
  
  model <- toy_model(control)
  
  sampler <- datefixer_sampler(control)
  samples <- mcmc_run(model, sampler, control)
  
  expect_equal(dim(samples$pars), c(length(model$parameters), 50, 3))
})
