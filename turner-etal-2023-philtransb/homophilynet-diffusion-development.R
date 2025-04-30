sample_ingroup <- function(homophily) {
  # Probability of within-group interaction.
  ingroup_prob <- 0.5 * (1 + homophily)
  # Return if random draw from uniform distribution less than ingroup prob.
  return (runif(1) < ingroup_prob)  
}

# Define partner selection for structured mixing.
structmix_successbiased_partner_selection <- function(focal_agent, model) {
  # Extract focal agent's group.
  grp <- focal_agent$get_attribute("Group")
  # Get homophily attribute from the focal agent.
  h <- focal_agent$get_attribute("Homophily")
  
  # Use group lookup table to get either in-group or out-group prospective teachers.
  grp_lookup <- model$get_parameter("group_lookup")
  grp_names <- names(grp_lookup)
  
  # Prospective teachers stored as vector; from ingroup (if) or an outgroup (else)
  prospective_teachers <- 
    purrr::list_c(ifelse(
      sample_ingroup(h), grp_lookup[grp], grp_lookup[setdiff(grp_names, grp)]
    ))
  
  # Select an agent from the prospective teachers weighted by fitness
  prob_weights <- purrr::map_vec(prospective_teachers, \(pt) pt$get_fitness())
  return (sample(prospective_teachers, 1, prob = prob_weights)[[1]])
}

# Use our custom structured mixing 
structmix_successbiased_strategy <- socmod::LearningStrategy$new(
  partner_selection = structmix_successbiased_partner_selection,
  interaction = socmod::success_bias_interact,
  model_step = socmod::iterate_learning_model,
  label = "Structured mixing, success-biased"
)

#' Makes vars into key-value pairs for passing as arguments to ... 
#'  position in make_model_parameters for free parameters. 
#' 
#' ex: to_keyval(param1, param2) effectively turns 
#' this in to param1 = param1, param2 = param2
to_keyval <- \(vars) !!!rlang::list2(vars)

make_minmaj_structmix <- function(n_agents = 100, minority_fraction = 0.05,
                                  homophily = c(0.0), start_group = "Minority",
                                  social_learning_strategy = 
                                    structmix_successbiased_strategy,
                                  adaptive_fitness = 1.4, legacy_fitness = 1.0) {
  
  # Set up agents here!
  n_minority <- round(n_agents * minority_fraction)
  n_majority <- n_agents - n_minority
  agents_groups <- c(rep("Minority", n_minority), rep("Majority", n_majority))
  assertthat::are_equal(length(agents_groups), n_agents)
  agents <- purrr::imap(
    agents_groups, \(group, a_idx) {
      socmod::Agent$new(id = a_idx, name = paste0("a", a_idx), 
                        behavior = "Legacy", fitness = legacy_fitness)
    }
  )
  # Build parameters named list, where list2 automagically does key-valuing
  # params <- rlang::list2(
  #   learning_strategy = social_learning_strategy,
  #   graph = igraph::make_empty_graph(n_agents), n_agents,
  #   homophily, start_group, adaptive_fitness, legacy_fitness
  # )
  # # Create new ABM using model parameters succinctly defined above via do.call
  # abm <- socmod::make_abm(
  #   model_parameters = do.call(socmod::make_model_parameters, params),
  #   agents = agents
  # )
  # 
  # return (abm)
    # learning_strategy = social_learning_strategy, 
    #   graph = igraph::make_empty_graph(n_agents),
    #   n_agents = n_agents,
    #   homophily = homophily,
    #   start_group = start_group,
    #   adaptive_fitness = adaptive_fitness,
    #   legacy_fitness = legacy_fitness
    # ),
  #   agents = agents
  # )
  # 
  abm <- socmod::make_abm(
    model_parameters = socmod::make_model_parameters(
      learning_strategy = social_learning_strategy,
      graph = igraph::make_empty_graph(n_agents),
      n_agents = n_agents,
      homophily = homophily,
      start_group = start_group,
      adaptive_fitness = adaptive_fitness,
      legacy_fitness = legacy_fitness
    ),
    agents = agents
  )

  return (abm)
}
      

abm <- make_minmaj_structmix()