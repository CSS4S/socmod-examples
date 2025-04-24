# Define a function that models a football match.
play_match <- function(team1, team2) {
  
  # Walk over each player to see how many scores they get, 
  # summing to get the total team score. 
  team1_score <- sum(
    purrr::map_vec(
      team1$players,
      \(player) {
      # Calculate the total goals scored by the current player.
        sum(purrr::map_vec(1:10, \(.) player$scored_goal()))
      }
    ) 
  )
  
  team2_score <- sum(
    purrr::map_vec(
      team2$players,
      \(player) {
      # Calculate the total goals scored by the current player.
        sum(purrr::map_vec(1:10, \(.) player$scored_goal()))
      }
    ) 
  )
  
  if (team1_score == team2_score) {
    team1$ties <- team1$ties + 1
    team2$ties <- team2$ties + 1
    cat("Tie game!\n")
  } else if (team1_score > team2_score) {
    cat(team1$name, "wins!!!\n")
    team1$wins <- team1$wins + 1
  } else {
    cat(team2$name, "wins!!!\n")
    team2$wins <- team2$wins + 1
  }
  
  cat(team1$name, ": ", team1_score, "   ",  
      team2$name, ": ", team2_score, "\n")
  
  team1$games_played <- team1$games_played + 1
  team2$games_played <- team2$games_played + 1
}
