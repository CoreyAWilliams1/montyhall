#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title
#' Player randomly selects a door from the previously created game.
#'
#' @description
#' `select_door()` picks a door selection from those created in the current game
#' by `create_game()`.
#'
#' @details
#' `select_door()` randomly selects one of a three door selection from the game
#' previously created by `create_game()`.
#' Neither any Car nor Goat location is known to the Player.
#'
#' @param
#' ... No arguments are used by the function.
#'
#' @return
#' Function returns a  vector of length 1 containing the number 1, 2, or 3 to
#' indicate the location of the selected door.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Game Host opens a confirmed Goat Door.
#'
#' @description
#' `open_goat_door()` picks a second door known to have a goat behind it
#' from the remaining two doors not selected previously from `select_door()`.
#'
#' @details
#' This function has the game Host reveal a door with a goat behind it that the
#' contestant has not chosen.This door is a game loosing door and provokes the
#' player to next make a decision of the winning door with this new knowledge of
#' knowing a game loosing door choice. The game Contestant now has a chosen
#' mysterious door, a known loosing door, and a third mysterious door that they
#' may soon choose instead.
#'
#' @param
#' Parameter game is the current game vector selected by 'create_game()'.
#' Parameter a.pick is integer that is the contestant's original choice of a
#' door from 'select_door()'.
#'
#' @return
#' Returns length 1 vector of a number 1, 2, or 3 representing a door with a
#' goat that is not a.pick.
#'
#' @examples
#' open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#' Contestant choses a final selection: Stay with or Switch door selection.
#'
#' @description
#' `change_door()` selects one of the initial door selected from `select_door()`
#' or the third remaining door, but not the door known to have a goat from
#' `open_goat_door()`.
#'
#' @details
#' The Current game Contestant has selected a mysterious door which may have a
#' car or goat behind it, a known goat door has been revealed, and there is a
#' third door that is also mysterious goat or car. The Contestant can choose to
#' stay with their original door selection a.pick, or select the remaining
#' mysterious door, given the knowledge of a known goat door that was revealed
#' by the Host.
#'
#' @param
#' Parameter stay is logical: stay=T indicating the contestant not switching and
#' staying with their original choice of door and stay=F for switching to the
#' third mystery door neither disclosed or chosen. Parameter opened.door is the
#' number of the known goat door, disclosed by the Host. Parameter a.pick is the
#' integer representing the door originally selected by the contestant of the
#' current game.
#'
#' @return
#' Returns length 1 vector or 1, 2, or 3 representing the position of the stay
#' or switch door selection, the final player door selection.
#'
#' @examples
#' change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }
  
  return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if the Contestant has Won or Lost the game
#'
#' @description
#' `determine_winner()` judges the contestant as having won the car or lost the
#'  game and then returns the game results.
#'
#' @details
#' If a car is behind the final contestant chosen door, the player won.
#' Otherwise, the payer has lost.
#'
#' @param
#' Parameter final.pick is vector length 1 of integer 1, 2, or 3, representing
#' the final door chosen by the contestant via 'change_door()'. This is compared
#' to the current game created by 'create_game()'.
#'
#'
#' @return
#' Returns character string being one of "Win" or "Lose" if the final door
#' chosen is the car door (Win) established in the current game or not (Lose).
#'
#' @examples
#' determine_winner ()
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#' Play a round of the Monty Hall Problem Game
#'
#' @description
#' `play_game()` executes an entire round of the game, executing the steps in
#' order as one function made of the functions combined.
#'
#' @details
#' Game is created. Three doors: one with a car and two with goats behind them.
#' The Contestant select an initial door choice without knowing its occupant
#' goat or car. The Host discloses a goat door to the contestant that is neither
#' a door with a car or the previously chosen door. Contestant now decides to
#' stay with their original choice of door or to switch o the reaming door. If
#' this final choice of door by the contestant has a car behind it they win;
#' if there is a goat behind it, they lose. This game is played once.
#'
#' @param
#' ... no arguments are used by the function; but the other functions are used:
#' create_game(), select_door(), open_goat_door(), change_door(), and
#' determine_winner().
#'
#' @return
#' Returns the final outcome of the game instance being a "Win" or a "Lose".
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play 100 Games to Determine Win Probability, given Stay or Switch Strategy.
#`
#' @description
#' `play_n_games()` plays n complete game iterations, n being 100, and create a
#' random sample of game results from staying and switching strategies in order
#' to determine the probability of each Win and Lose, given having either
#' switched or stayed in a given game.
#'
#' @details
#' Loops the game n=100 times to accumulate a sample from which the average
#' proportion of Win if the contestant chooses to stay compared to switch.
#'
#' @param
#' Parameter n=100 is the input for the number of games to run. The Rule of
#' Large Numbers implies that the greater n is, the nearer to the actual
#' probability will the outcome approach.
#'
#' @return
#' Creates a dataframe of the number of Win and Lose game.outcome in a combined
#' results.list, based on contestant's strategy of Stay or Switch.
#' Returns a table showing the proportion of Win and Lose based on the
#' contestant's choice of stay with the original door choice or to switch to the
#' remaining mystery door.
#'
#' @examples
#' play_n_games()
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()
  
  return( results.df )
  
}
