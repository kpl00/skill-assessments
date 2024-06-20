#Global variables
active_board <- blank_board
gamephase <-1
player_x_o <- ""
player_number <- 0
computer_number <- 0
pause_time <- 1

# Builds a new board
blank_board <- matrix (
  c(1,1,1,1,1,1,1,1,1),
  ncol=3,
  nrow=3
)

# Function: Allows player to choose X or O
player_select <- function() {
  
  
  while (player_x_o != "X" && player_x_o != "O") {
    player_x_o <<- readline(prompt="X or O?")
    
    if(player_x_o == "X" || player_x_o == "O") {
      break
    }
    
    print("invalid selection, please choose X or O")
  }
}

# Player is set a assigned number, based on X or O selection, which is used for scoring purposes
player_number_assignment <- function () {
  if (player_x_o == "X") {
    player_number <<- 3
    computer_number <<- 9
  } else if (player_x_o == "O") {
    player_number <<- 9
    computer_number <<- 3
  }
}

# Function for player's move 
player_move <- function() {
  cat("Player turn:")
  invalid_player_entry <-1
  
  while (invalid_player_entry ==1) {
    
    boardrow <- readline(prompt="Enter your row")
    boardrow <- as.integer(boardrow)
    boardcol <- readline(prompt="Enter your column")
    boardcol <- as.integer(boardcol)
    
    if (boardrow < 4 && boardcol <4 && active_board[boardrow,boardcol]==1){
      invalid_player_entry <-0
    }  
    
    if (invalid_player_entry == 1) {
      print("Placement is not valid, please enter valid coordinates")
    }
  }
  cat(paste("You moved to row ",boardrow,"and column ",boardcol,"\n"))
  cat ("\n")
  cat("Your move:\n")
  active_board[boardrow,boardcol] <<- player_number
  show_o_x(active_board)
}

# Function for computer's move
computer_move <- function() {
  boardrow <- sample (3,1)
  boardcol <- sample (3,1)
  if(active_board[boardrow,boardcol] == 1) {
    active_board[boardrow,boardcol] <<- computer_number
    cat("Computer's move:\n")
    show_o_x(active_board)
  } else {
    computer_move()
  }
}

# Function to show the board in the format of O and X
show_o_x <- function(reference_board) {
  
  cat ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat ("\n")
  visible_board <- reference_board
  visible_board[visible_board==1] <- " "
  visible_board[visible_board==3] <- "X"
  visible_board[visible_board==9] <- "O"  
  print((visible_board))
  cat ("\n")
  cat ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat ("\n")
}

# Scoring Function
score <- function() {
  
  row1 <- active_board[1,1]+active_board[1,2]+active_board[1,3]
  row2 <- active_board[2,1]+active_board[2,2]+active_board[2,3]
  row3 <- active_board[3,1]+active_board[3,2]+active_board[3,3]
  col1 <- active_board[1,1]+active_board[2,1]+active_board[3,1]
  col2 <- active_board[1,2]+active_board[2,2]+active_board[3,2]
  col3 <- active_board[1,3]+active_board[2,3]+active_board[3,3]
  diag1 <- active_board[1,1]+active_board[2,2]+active_board[3,3]
  diag2 <- active_board[1,3]+active_board[2,2]+active_board[3,1]
  
  scored_patterns <<- c(row1,row2,row3,col1,col2, col3, diag1, diag2)
  
  computer_3 <- computer_number*3
  player_3 <- player_number*3
  
  if (computer_3 %in% scored_patterns) {
    cat("The computer wins. ")
    gamephase <<- 2
    
  }
  
  if (player_3 %in% scored_patterns) {
    cat("You win! ")
  gamephase <<- 2
  }
  
  if (!( 1 %in% active_board) ) {
    cat("Noone wins. ")
    gamephase <<- 2
  }

}

# Function to print the round number
round_print <- function(round_number) {
  
  cat("########################\n")
  cat(paste("####### Round ",round_number,"#######\n"))
  cat("########################\n")
  cat ("\n")
}

# Function to run the entire game once
game <- function() {
  
  round_timer <- 1
  gamephase <<- 1
  active_board <- blank_board
  player_select()
  player_number_assignment()
  
  cat("Welcome to this game of Tic Tac Toe!\n")
  cat("Current Board:\n")
  show_o_x(active_board)
  cat("\n")
  Sys.sleep(pause_time)
  
  
  while (gamephase == 1 && player_x_o == "X") {
    
    round_print(round_timer)
    Sys.sleep(pause_time)
    
    player_move()
    score()
    
    if(gamephase==2){
      break
    }
    
    Sys.sleep(pause_time)
    
    computer_move()
    Sys.sleep(pause_time)
    score()
    Sys.sleep(pause_time)
    round_timer <- round_timer + 1
    
  }
  
  while (gamephase == 1 && player_x_o == "O") {
    
    round_print(round_timer)
    Sys.sleep(pause_time)
    computer_move()
    score()
    
    if(gamephase==2){
      break
    }
    
    Sys.sleep(pause_time)
    
    player_move()
    Sys.sleep(pause_time)
    score()
    Sys.sleep(pause_time)
    round_timer <- round_timer + 1
    
  }
  
  cat("The game is complete")
  
  
}

game()

