##
## Dylan Lee
## Introduction to Computer Science (2015)
##

# find_all(string,sub) produces a list of all the positions sub appears in 
# string

def find_all(string, sub):
    start=0
    lst=[]
    for x in range(string.count(sub)):
        start=string.find(sub, start)
        if start == -1:
            return None
        lst.append(start)
        start += len(sub)
    return lst

# hangman(word,guesses) consumes a word which is the word the user has to 
# guess as well as guesses which is the number of guesses the user gets. The
# user gets to guess until guesses runs out or they guess the entire word.
# if the user guesses a letter they've already guessed before, their number
# of guesses remains unchanged. Users may only enter guesses that are
# single, alphabetic lower-case letters or 'quit' if they choose to quit 
# the game.
# effects: hangman will print a welcome message, what the word looks like
# with correct guessed letters revealed(i.e --ll- if the word is 'hello'
# and the user guessed 'l'). 
#          hangman will prompt the user for an input which is their guess for 
#          a letter. If the user guesses incorrectly, a try again message
#          will be displayed with the number of guesses remaining
#          if the user enters 'quit' a goodbye message will be displayed
#          if the user guesses the word correctly, a congratulations message
#          will be displayed
#          if the user does not guess the word correctly, a game over message
#          will be displayed and the correct word will be shown
#          if the user guesses a letter they already guessed, a message will
#          be displayed saying they already guessed that letter
# hangman: Str Nat -> None
# requires: word is a non-empty, alphabetic, non-whitespace, lowercase word

def hangman(word, guesses):
    x = '-'*len(word)
    guessed_letters=[]
    print("Welcome to hangman.\nYour word is",x)
    
    while guesses>0:
        
        user_guess= input("Please guess a letter or type quit: ")
        
        while guessed_letters.count(user_guess) > 0:
            print("You already guessed", user_guess +".","Guesses remaining:", guesses)
            print("\nYour word is",x)
            user_guess= input("Please guess a letter or type quit ")
            
        if user_guess == 'quit':
            print("Goodbye!")
            return None
       
        elif user_guess in word and x.count('-')==1:
            print("Congratulations the word is", word) 
            return None

        elif user_guess in word:
            positions= find_all(word,user_guess)
            
            for y in range(len(positions)):
                x = x[:positions[y]]+user_guess+x[positions[y]+1:]
            
            print("\nYour word is", x)
            
        elif user_guess not in word and guesses > 1:
            guesses=guesses-1
            print("Sorry, try again. Guesses remaining: ", guesses)
            print("\nYour word is", x)
        
        else:
            print("Game over. The correct word is", word+".")
            return None   
              
        guessed_letters.append(user_guess)