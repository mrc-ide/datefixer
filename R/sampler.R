datefixer_sampler <- function(control) {
  
  monty::monty_sampler(
    "datefixer sampler",
    "datefixer sampler",
    control,
    datefixer_sampler_initialise,
    datefixer_sampler_step)
}

datefixer_sampler_initialise <- function(state_chain, control, model, rng) {
  return(NULL)
}

datefixer_sampler_step <- function(state_chain, state_sampler, control, 
                                   model, rng) {
  state_chain <- update_prob_error(state_chain, model, rng)
  
  state_chain 
}
