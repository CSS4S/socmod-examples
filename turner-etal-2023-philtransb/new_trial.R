

run_trial_agg <- function(model, stop = 5, ...) {
  
  trial <- socmod::run_trial(model, stop)
  trial$observations <- update_observations
  
  return (trial)
}


make_example_abm <- function(n_agents = 10, adaptive_fitness = 1.4, 
                             legacy_fitness = 1.0, n_adaptive_init = 1) {

  abm <- socmod::make_abm(socmod::make_model_parameters(n_agents = n_agents))
  purrr::walk(abm$agents, \(a) a$set_fitness(legacy_fitness))
  
  seed_agents <- sample(abm$agents, n_adaptive_init)
  for (seed_agent in seed_agents) {
    seed_agent$set_behavior("Adaptive")
    seed_agent$set_fitness(adaptive_fitness) 
  }

  return (abm)
}



abm <- make_example_abm()

aggfunc_diffusion_default <- function(allagents_observations, 
                                      tracked_behaviors = 
                                        c("Legacy", "Adaptive")) {

  if (!purrr::is_empty(allagents_observations)) {
    all_obs <- allagents_observations
    n_agents <- length(unique(allagents_observations$agent))

    # Set Behavior to be one factor value among all tracked_behaviors so we can 
    # use `tidyr::complete` in the command below.
    all_obs <- dplyr::mutate(
      all_obs, 
      Behavior = factor(Behavior, levels = tracked_behaviors)
    )

    ret <- 
      dplyr::group_by(all_obs, Step, Behavior) %>%
        dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
        tidyr::complete(Step, Behavior, fill = list(Count = 0)) %>%
        dplyr::mutate(Prevalence = Count / n_agents)

    return (ret)
  } else {
    return (tibble::tibble())
  }
}
# load_all()
# abm <- make_example_abm(n_agents = 100, n_adaptive_init = 5, adaptive_fitness = 2.0); 

n_adaptive_init <- 5; adaptive_fitness <- 1.5; legacy_fitness <- 1.0;

model_params <- make_model_parameters(graph = make_small_world(100, 10, 0.5))
abm <- make_abm(model_params)
purrr::walk(abm$agents, \(a) a$set_fitness(legacy_fitness))
seed_agents <- sample(abm$agents, n_adaptive_init)
for (seed_agent in seed_agents) {
  seed_agent$set_behavior("Adaptive")
  seed_agent$set_fitness(adaptive_fitness) 
}

aggobs <- run_trial(abm, stop = fixated, aggfunc = aggfunc_diffusion_default); 

p <- ggplot(aggobs, aes(x = Step, y = Prevalence, color = Behavior)) +
  geom_line() + geom_point()

print(p)


## -----------BENCHMARKING NOTES------------
## 
## 
library(microbenchmark)

run_sw_model <- function(n, n_adaptive_init = 5) {
  adaptive_fitness <- 1.5; legacy_fitness <- 1.0;
  
  # model_params <- make_model_parameters(graph = make_small_world(n, 10, 0.5))
  model_params <- make_model_parameters(graph = igraph::sample_smallworld(dim = 1, size = n, nei = 5, p = 0.1))
  
  abm <- make_abm(model_params)
  purrr::walk(abm$agents, \(a) a$set_fitness(legacy_fitness))
  seed_agents <- sample(abm$agents, n_adaptive_init)
  for (seed_agent in seed_agents) {
    seed_agent$set_behavior("Adaptive")
    seed_agent$set_fitness(adaptive_fitness) 
  }
  
  aggobs <- run_trial(abm, stop = fixated);
  rm("aggobs")
}

# Benchmark across different N values
# Ns <- c(50, 100, 200, 500, 1000, 2000)
Ns <- c(50, 100, 200, 500, 1000)

# Store results
benchmark_results <- purrr::map_dfr(Ns, function(n) {
  bm <- microbenchmark(run_sw_model(n), times = 2)  # 5 replicates per N
  # convert nanoseconds to ms, s, and minutes
  tibble(N = n, time_ms = bm$time / 1e6, time_sec = bm$time / 1e9, 
         time_min = bm$time / (1e9 * 60))  
})

mean_bm_times <- 
  group_by(benchmark_results, N) %>%
  summarise(mean_time_ms = mean(time_ms), 
            mean_time_sec = mean(time_sec),
            mean_time_min = mean(time_min))
  
glimpse(mean_bm_times)
