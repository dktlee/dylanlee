##
## Dylan Lee
## Introduction to Computer Science (2015)
##

## check_diagonal (queen_pos, king_pos) produces true if the queen is in
## position to diagonally attack the kind, otherwise produces false
def check_diagonal (queen_pos, king_pos):
    return (abs(ord(queen_pos[:1]) -  ord(king_pos[:1])) == 
            abs(int(queen_pos[1:]) - int(king_pos[1:])))
        

## check_by_queen(queen_pos, king_pos) prints "Check!!!" if the queen, in
## queen_pos, is in position to attack the king, in king_pos, on a chess
## board, assuming there are no other pieces on the board. Prints nothing if
## the king is not under attack
## requires: queen_pos, king_pos is made up (anyof "A", "B", ... "H")
##           and (anyof "1", "2", ... "8")
    
def check_by_queen(queen_pos, king_pos):
    if queen_pos[:1] == king_pos[:1]:
        print("Check!!!")
    elif queen_pos[1:] == king_pos[1:]:
        print("Check!!!")
    elif check_diagonal(queen_pos, king_pos):
        print("Check!!!")        
    else:
        return