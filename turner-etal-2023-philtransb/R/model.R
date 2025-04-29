#' Make a demo minority-majority symmetric homophily model.
#' 
#' @param n_agents Number of agents in the model
#' @param minority_fraction Fraction of agents who are in the minority group
#' @param mean_degree Mean degree of homophily network
#' @param homophily Symmetric homophily, the difference in probability of within- versus between-group social connections
#' @param social_learning_strategy How social learning works
#' @param adaptive_fitness Fitness value of the adaptive behavior
#' @param start_group Group that begins with the adaptive behavior
#' @returns AgentBasedModel initialized with specified parameters
make_minmaj_model <- function(n_agents = 100, minority_fraction = 0.05,
                              mean_degree = 4, homophily = c(0.0),
                              social_learning_strategy = 
                                socmod::success_bias_learning_strategy,
                              adaptive_fitness = 1.4, 
                              start_group = "Minority") {
  
  # Minority groups are defined by the population size in this model.
  minority_size <- ceiling(n_agents * minority_fraction)
  majority_size <- n_agents - minority_size
  
  graph <- socmod::make_homophily_network(
    group_sizes = c(
      minority_size,
      majority_size
    ),
    mean_degree = mean_degree,
    homophily = homophily,
    add_to_complete = TRUE
  )
  
  # Initialize ABM.
  abm <- socmod::make_abm(socmod::make_model_parameters(graph = graph))
  
  # It might have already synced the group information as a parameter, let's see.
  # If not I'll have to write that.
  return (abm)
}
