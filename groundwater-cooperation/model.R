#' Create a spatial Prisoner's Dilemma model for resource conservation
#'
#' Agents choose between "Conserve" and "Overuse" behaviors.
#' Payoffs follow a standard Prisoner's Dilemma structure.
#'
#' @param width Number of agents per row (grid will be width x width)
#' @param initial_n_conserving Number of agents initially assigned "Conserve"
#' @param overuse_temptation Payoff for overusing when neighbors conserve
#' @param conservation_reward Payoff for mutual conservation
#' @param overuse_punishment Payoff for mutual overuse
#' @param one_sided_conservation Payoff for conserving when neighbors overuse
#'
#' @return A configured AgentBasedModel instance
#' @export
make_groundwater_pd_model <- function(
  width = 10,
  initial_n_conserving = NULL,
  overuse_temptation = 5,
  conservation_reward = 3,
  overuse_punishment = 1,
  one_sided_conservation = 0
) {
  stopifnot(width > 1)
  n <- width * width

  # Create 2D lattice (no torus)
  g <- igraph::make_lattice(c(width, width), nei = 1, circular = FALSE)

  # Create behaviors: start with all "Overuse"
  behaviors <- rep("Overuse", n)
  if (is.null(initial_n_conserving)) {
    initial_n_conserving <- floor(n / 2)
  }
  conserve_indices <- sample(seq_len(n), size = initial_n_conserving)
  behaviors[conserve_indices] <- "Conserve"

  # Create agents
  agents <- purrr::map2(
    behaviors,
    seq_len(n),
    function(b, i) socmod::Agent$new(id = i, name = paste0("a", i), behavior = b)
  )

  # Build model using standard socmod API
  model <- socmod::make_abm(
    socmod::make_model_parameters(
      learning_strategy = success_biased_pd_strategy,
      graph = g,
      overuse_temptation = overuse_temptation,
      conservation_reward = conservation_reward,
      overuse_punishment = overuse_punishment,
      one_sided_conservation = one_sided_conservation
    ),
    agents = agents
  )

  return (model)
}

#' Calculate Prisoner's Dilemma payoff for a behavior pair
#'
#' Payoff structure matches resource conservation framing of the Prisoner's Dilemma.
#'
#' @param focal_behavior Behavior of the focal agent
#' @param partner_behavior Behavior of the partner agent
#' @param model The AgentBasedModel
#'
#' @return Payoff to focal agent
#' @export
groundwater_pd_payoff <- function(focal_behavior, partner_behavior, model) {
  payoff_table <- list(
    "Conserve" = list(
      "Conserve" = model$get_parameter("conservation_reward"),
      "Overuse"  = model$get_parameter("one_sided_conservation")
    ),
    "Overuse" = list(
      "Conserve" = model$get_parameter("overuse_temptation"),
      "Overuse"  = model$get_parameter("overuse_punishment")
    )
  )

  if (!focal_behavior %in% names(payoff_table)) {
    stop("Invalid focal behavior in groundwater_pd_payoff.")
  }
  if (!partner_behavior %in% names(payoff_table[[focal_behavior]])) {
    stop("Invalid partner behavior in groundwater_pd_payoff.")
  }

  return (payoff_table[[focal_behavior]][[partner_behavior]])
}

#' Prisoner's Dilemma interaction function (pairwise)
#'
#' Each agent plays a Prisoner's Dilemma game with a neighbor.
#'
#' @param focal_agent The focal agent
#' @param partner The partner agent
#' @param model An AgentBasedModel instance
#'
#' @return NULL
#' @export
groundwater_pd_interaction <- function(focal_agent, partner, model) {
  payoff <- groundwater_pd_payoff(
    focal_behavior = focal_agent$get_behavior(),
    partner_behavior = partner$get_behavior(),
    model = model
  )

  new_fitness <- focal_agent$get_next_fitness() + payoff
  focal_agent$set_next_fitness(new_fitness)

  return (invisible(NULL))
}

#' Random neighbor selection for spatial games
#'
#' @param focal_agent The focal agent
#' @param model An AgentBasedModel instance
#'
#' @return A randomly selected neighbor
#' @export
random_neighbor_selection <- function(focal_agent, model) {
  partner <- focal_agent$get_neighbors()$sample(1)
  return (partner)
}

#' Success-biased learning strategy for spatial Prisoner's Dilemma
#'
#' @export
success_biased_pd_strategy <- socmod::LearningStrategy$new(
  partner_selection = random_neighbor_selection,
  interaction = groundwater_pd_interaction,
  model_step = socmod::iterate_learning_model,
  label = "Success-biased Prisoner's Dilemma"
)


print(success_biased_pd_strategy$get_interaction())
