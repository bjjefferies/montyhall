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
#'  Randomly select one of three doors. This is the first door pick in
#'  the Monty Hall Problem game.
#'
#' @description
#'  `select_door()` returns a random integer between 1 and 3 and serves
#'  as the first door selected while playing the Monty Hall Problem
#'  game.
#'
#' @details
#'  This is the first step in playing the Monty Hall Problem. One of
#'  three doors is randomly selected. Since no other information is known
#'  by the contestant at the time of the first door choice, a random
#'  simulation is appropriate.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#'  The function returns an integer vector of length 1 indicating the
#'  index value of the door first chosen by the contestant.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Opens one of the two un-chosen doors that has a goat behind it.
#'
#' @description
#'  `open_goat_door()` After selecting one of the three doors in the
#'  game, the host reveals a goat behind one of the two un-chosen doors.
#'
#' @details
#'  This function generates the index value of the un-chosen door
#'  behind which lies a goat. It will not reveal the door with
#'  the car behind it. In the case the initial door value was the car,
#'  the function randomly chooses between the other two doors to
#'  reveal. If the initial door had a goat behind it, then the
#'  function automatically chooses the only remaining door with
#'  a goat.
#'
#' @param game vector of length three containing strings "goat" and "car"
#' @param a.pick a positive integer between 1 and 3
#'
#' @return an integer between 1 and 3
#'
#' @examples
#'   open_goat_door( c("goat", "car", "goat") , 3 )
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
#'  Allows user to select strategy for game: "stay" or "switch"
#'
#' @description
#'  After the host reveals one door with a goat behind it, the
#'  contestant has two choices, stay with the first choice
#'  or switch to the other door.
#'
#' @details
#'  `change_door()` requires the contestant to choose their
#'  strategy for the game and therefore their final choice
#'  of doors. The function requires a logical argument for
#'  strategy "stay" (T/F), the integer value of the host opened
#'  door and the door initially picked by the contestant.
#'
#' @param stay logical indicating strategy, TRUE = stay, FALSE
#'  = switch
#' @param opened.door positive integer value of the door opened by the host
#' @param a.pick postiive integer value of the first door chosen by
#'  the contestant
#'
#' @return a positive integer value between 1 and 3 indicating
#'  the final door selection for the contestant.
#'
#' @examples
#'   change_door( stay=F, 2, 1)
#'   change_door( stay=T, 3, 2)
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
#'  Reveals whether the final door selection was a winner or loser.
#'
#' @description
#'  After the contestant makes their final door selection, the
#'  host unveils whether the door held a goat (lose) or a car
#'  (win).
#'
#' @details
#'  `determine_winner()` compares the final door selection with
#'  the original game vector. If the final door's value as an
#'  index in the game vector is "car" the contestant wins and
#'  will return "WIN". Otherwise, if the final door's value as an
#'  index in the game vector is a "goat" the contestant loses.
#'  Requires the final door selection and the initial game vector.
#'
#' @param final.pick Integer value representing the final door choice
#' @param game Vector of length 3 containing 2 "goat" and 1 "car"
#'
#' @return a character string. "WIN" or "LOSE"
#'
#' @examples
#'   determine_winner( 2 , c("goat", "car", "goat"))  #Returns WIN
#'   determine_winner( 1 , c("goat", "car", "goat"))  #Returns LOSE
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
#'  Runs one full iteration of the Monty Hall Problem.
#'
#' @description
#'  The Monty Hall Problem is a famous mathematical probability
#'  problem that replicates a game show. A contestant is shown
#'  three doors, two of which veil a goat and one of which holds
#'  a car. The contestant first chooses one door, after which the
#'  host unveils one of the other two doors which has a goat behind
#'  it. The contestant then chooses to stay with their first door
#'  or switch to the other. This function evaluates the outcomes
#'  of both strategies in a single, random round of the Monty Hall
#'  Problem game.
#'
#' @details
#'  `play_game()` requires no inputs. It first creates a random vector
#'  of three items: 2 goats and 1 car. A random value between 1 and 3
#'  is generated as the contestants first choice and one of the two
#'  remaining values is revealed to hold a goat. The function then
#'  reports the results of both strategies the contestant might
#'  choose, "stay" or "switch"
#'
#' @param ... no arguments are used by the function
#'
#' @return
#'  data.frame of dimentions 2 by 2. The first column displays
#'  the strategies "stay" and "switch". The second column reports
#'  the outcome of each strategy "win" or "lose".
#'
#' @examples
#'   play_game()
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
#'  Run n iterations of the Monty Hall Problem and return
#'  winning proportions of both strategies.
#'
#' @description
#'  The Monty Hall Problem is a famous mathematical probability
#'  problem. This function allows one to simulate the problem n
#'  times in order to explore the empirical probability of both
#'  strategies for the game "stay" and "switch".
#'
#' @details
#'  `play_n_games()` runs the `play_game()` function from this
#'  package n times and stores the results of each strategy in
#'  a data frame. This results are then summarized and reported
#'  in a proportion table showing the winning and losing percentage
#'  of each strategy.
#'
#' @param n A positive integer. Number of simulations to run.
#'  Defaults to 100
#'
#' @return a proportion table of the observed winning and losing
#'  percentages of each strategy.
#'
#' @examples
#'   play_n_games(1000)
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

  results.dt <- table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 )

  return( results.dt )

}
