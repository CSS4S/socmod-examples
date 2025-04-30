sample_ingroup <- function(homophily) {
  
  # Probability of within-group interaction.
  ingroup_prob <- 0.5 * (1 + homophily)
  
  # Return if random draw from uniform distribution less than ingroup prob.
  ret <- runif(1) < ingroup_prob

  return (ret)  
}

# Define partner selection for structured mixing.
structmix_successbiased_partner_selection <- function(focal_agent, model) {
  # Extract focal agent's group.
  group <- focal_agent$get_attribute("Group")
  # Get homophily attribute from the focal agent.
  h <- model$get_parameter("homophily")
  
  # Use group lookup table to get either in-group or out-group prospective teachers.
  group_lookup <- model$get_parameter("group_lookup")
  group_names <- names(group_lookup)
  # Prospective teachers stored as vector; from ingroup (if) or an outgroup (else)
  prospective_teachers <- 
    purrr::list_c(ifelse(
      sample_ingroup(h), 
      group_lookup[group], 
      group_lookup[setdiff(group_names, group)]
    ))
  
  # Select an agent from the prospective teachers weighted by fitness
  prob_weights <- purrr::map_vec(
    prospective_teachers, 
    \(pt) pt$get_fitness()
  )
  return (sample(prospective_teachers, 1, prob = prob_weights)[[1]])
}

# Use our custom structured mixing 
structmix_successbiased_strategy <- socmod::LearningStrategy$new(
  partner_selection = structmix_successbiased_partner_selection,
  interaction = socmod::success_bias_interact,
  model_step = socmod::iterate_learning_model,
  label = "Structured mixing, success-biased"
)

make_group_lookup <- function(agents, groups = c("Minority", "Majority")) {
  return (
    purrr::map(
      purrr::set_names(groups), 
      \(g) { 
        purrr::keep(agents, \(a) a$get_attribute("Group") == g)
      }
    )
  )
}

make_minmaj_structmix <- function(n_agents = 100, 
                                  minority_fraction = 0.05,
                                  homophily = c(0.0), 
                                  start_group = "Minority",
                                  n_to_start = 2,
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
      a <- socmod::Agent$new(
        id = a_idx, 
        name = paste0("a", a_idx), 
        behavior = "Legacy", 
        fitness = legacy_fitness
      )

      # Currently this returns nothing, so need to set after creation.
      a$set_attribute("Group", group)
      
      return (a)
    }
  )
  
  group_lookup <- make_group_lookup(agents)
  assertthat::are_equal(group_lookup$Minority, n_minority)
  assertthat::are_equal(group_lookup$Majority, n_majority)
  
  # Initialize one or two agents with the adaptive behavior.
  if (start_group == "Both") {
    
    min_seed <- sample(group_lookup$Minority, 1)[[1]]
    maj_seed <- sample(group_lookup$Majority, 1)[[1]]
    
    min_seed$set_behavior("Adaptive")
    maj_seed$set_behavior("Adaptive")
    min_seed$set_fitness(adaptive_fitness)
    maj_seed$set_fitness(adaptive_fitness)
    
  } else if (start_group %in% c("Minority", "Majority")) {
    
    seed <- sample(group_lookup[[start_group]], 1)[[1]]
    seed$set_behavior("Adaptive")
    seed$set_fitness(adaptive_fitness)
    
  } else {
    
    stop("start_group must be 'Minority', 'Majority', or 'Both'")
  }
  
  abm <- socmod::make_abm(
    model_parameters = socmod::make_model_parameters(
      learning_strategy = social_learning_strategy,
      graph = igraph::make_empty_graph(n_agents),
      n_agents = n_agents,
      homophily = homophily,
      start_group = start_group,
      adaptive_fitness = adaptive_fitness,
      legacy_fitness = legacy_fitness,
      group_lookup = group_lookup
    ),
    agents = agents
  )

  return (abm)
}

# Initialize structured mixing ABM.
abm <- make_minmaj_structmix(adaptive_fitness = 10.0, homophily = 0.7)

trial <- socmod::run_trial(abm, stop = socmod::fixated)

p <- plot_adoption(trial, tracked_behaviors = c("Legacy", "Adaptive"))