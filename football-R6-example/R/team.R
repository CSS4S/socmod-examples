Team <- R6Class("Team",
                
  public = list(
    name = "",
    players = list(),
    wins = 0,
    ties = 0,
    payroll = 0,
    games_played = 0,
    
    # Team name is required, with players optionally specified.
    initialize = function(name, players = list()) {

      self$name = name

      # Initialize players and payroll.
      for (player in players) {
        self$payroll <- self$payroll + player$market_value
        player$team <- name
      }

      self$players <- players
    },
    
    # Add a player to the roster.
    sign_player = function(player) {
      # Add the player to the team.
      self$players <- c(players, player)
      # Update payroll.
      self$payroll <- self$payroll + player$market_value
      # Update the player's team to be this team's name.
      player$team <- self$name
    }
  )
)
