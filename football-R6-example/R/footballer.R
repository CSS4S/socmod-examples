library(R6)

Footballer <- R6Class("Footballer",
                        
  public = list(
    # Listing attributes as fields and 
    # setting to zero for their definition.
    name = "",
    speed = 0.0,  # units of max km/h
    accuracy = 0.0, # probability of scoring on a shot
    market_value = 0.0, # 
    aggressiveness = 0.0, # units of penalties per match
    team = "",
    
    initialize = function(name = "",
                          speed = 15, 
                          accuracy = 0.2, market_value = 1e6, 
                          aggressiveness = 0.5, 
                          team = "Free agent") {
      self$name = name
      self$speed = speed
      self$accuracy = accuracy
      self$market_value = market_value
      self$aggressiveness = aggressiveness
      self$team = team
    },
    
    # Stub two SoccerPlayer class methods...
    # ...one for scoring a goal in a game...
    scored_goal = function() {
      return (ifelse(runif(1) < self$accuracy, 1, 0))
    },
    # ...and one for getting a penalty in a game.
    get_penalty_on_play = function() {
      return (runif(1) < self$aggressiveness)
    }
  )
)
