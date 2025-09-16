datefixer_sampler <- function(control) {
  
  monty::monty_sampler(
    "datefixer sampler",
    "datefixer sampler",
    control,
    datefixer_sampler_initialise,
    datefixer_sampler_step)
}

datefixer_sampler_initialise <- function(state_chain, control, model, rng) {
  
}

datefixer_sampler_step <- function(state_chain, state_sampler, control, 
                                   model, rng) {

}