##
## Dylan Lee
## Introduction to Computer Science (2015)
##

# Q3 - word_count

# text2list(text,string,lst) produces a list of strings, by converting all 
# the words in text into a string, and then making a list of those strings,
# where string is the word that will go into lst (words are seperated by
# blank spaces, commas, and periods

def text2list(text, string, lst):
    if len(text) == 0:
        return list(filter(lambda x: x!="",lst + [string]))
    elif text[0] == " ":
        return text2list(text[1:],"", lst + [string])
    elif text[0] == ".":
        return text2list(text[1:],"", lst + [string])
    elif text[0] == ",":
        return text2list(text[1:],"", lst + [string])
    else:
        return text2list(text[1:], string + text[0], lst)
# realized I could have used str.replace() to remove all commas and periods
# and replace them with spaces " " and then use the str.split() to convert the text to a 
# list of individual words after I wrote this function
    
# total_word_count(text_list) consumes text_list which is a list of strings
# and prints the total number of words in the text_list 
# effects: prints a natural number on one line 
# total_word_count: (listof Str) -> None

def total_word_count(text_list):
    total_num_of_words = len(text_list)
    print ("There are a total of", total_num_of_words, "words.")
   
# word_count_printer(text_list, curr_word, counter) consumes a list of strings
# text_list, and prints the number of times each individual word, curr_word
# occurs, which is the total of the counter

def word_count_printer(text_list, curr_word, counter):
    if text_list == []:
        print(curr_word, "-", counter, "time(s).")
    elif text_list[0] != curr_word:
        print (curr_word, "-", counter, "time(s).")
        word_count_printer(text_list[1:], text_list[0], 1)
    else:
        word_count_printer(text_list[1:], curr_word, counter+1)

# word_count (text) consumes a string, text, and prints the total word count
# for it, as well as how many times each individual word occurs in 
# alphabetical order
# Example:
# calling word_count("") prints on one line: "There are a total of 0 words."
# calling word_count("Up,up,up,to the sky, ... 3 ups.") prints on seperate
#         lines : “There are a total of 8 words.
#                  3 - 1 times.
#                  Up - 1 times.
#                  sky - 1 times.
#                  the - 1 times.
#                  to - 1 times.
#                  up - 2 times.
#                  ups - 1 times.”
# calling word_count("mixed-with-symbols!!! hi2342648 @#$%^&*") prints on
#         seperate lines: "There are a total of 3 words.
#                  @#$%^&* - 1 times.
#                  hi2342648 - 1 times.
#                  mixed-with-symbols!!! - 1 times."

def word_count(text):
    text_list= sorted(text2list(text,"", []))    
    if text_list == []:
        total_word_count(text_list)
    else:
        total_word_count(text_list)
        word_count_printer(text_list, text_list[0], 0)