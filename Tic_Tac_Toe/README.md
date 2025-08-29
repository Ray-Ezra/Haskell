TIC TAC TOE HASKELL

Planned approach
1. Figure out the requirements for making it using Haskell
    1. The Game logic:
        1. This should have a dedicated module that enforces the rules of the game, EG, turn alteration, preventing moves on occupied or out-of-bounds positions and managing game state transitions.
        2. Create two modules that control. The game
            - [ ] The Game module: This module will determine whether the game begins with an X or  O.
            - [ ] The referee module: This will check the game-over conditions, determining if a player has won, lost, or drawn.
        3. Defining the playing board: This can be modeled as a list defining each list to have a square like a room with tiles divided into 9 with equal sizes.
2. Define all Functions
    1. Defined a board(emptyBoard)
        1. Clearly defined the dimensions of the board by dividing it into 9 equal spaces/squares.
        2. Created the core functionalities of the game 
            1. makeMove: This is responsible for ensuring the player’s move is registered and printed to the required index of the board.
            2. Clearly defined the winning combos using(winingCombos): Here I’ve listed the combinations of specific sequences that determine if the player has won.
            3. checkWin: This function is responsible for checking whether the player has won a game by running all the players’ moves through the winning combos. If one of the players' combinations matches one of the winning combos, it registers as won. If not, then all nine positions are not empty; their registers are drawn.
            4. Finally, there is a gameLoop function. This function is responsible for the switching of the players back and forth to ensure smooth gameplay between Each opponent.

EEY HAVE FUN
