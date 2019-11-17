##
## Dylan Lee
## Introduction to Computer Science (2015)
##

# bar_label_length(data, max_label_length, index) produces the length 
# of the largest bar label that will be created from data, which is 
# max_bar_length, by checking to see if the label in data at index is bigger
# than the max_bar_length so far
# requires: the format of data will always have an even number of elements
#           where a bar label string is followed by a natural number for the
#           bar length
#           first call of index is 0

def bar_label_length(data, max_label_length, index):
    if index >= len(data):
        return max_label_length
    elif len(data[index]) > max_label_length:
        return bar_label_length(data, len(data[index]), index+2)
    else:
        return bar_label_length(data, max_label_length, index+2)
    
# chart_acc(data, label_index, length_index, max_bar_label) prints
# out a bar chart horizontally with bar labels named from the string in data
# at label_index with a bar length of the integer at length_index in data
# formated so that the bar label is right alinged according to the 
# max_bar_label
# requires: the format of data will always have an even number of elements
#           where a bar label string is followed by a natural number for the
#           bar length
#           first call of label_index is 0
#           first call of length_index is 1

def chart_acc(data, label_index, length_index, max_bar_label):
    if not(label_index >= len(data)):
        label = "|" + " "*(max_bar_label - len(data[label_index])-1) \
                    + data[label_index] + " |"
        print(label + "#"*data[length_index])
        chart_acc(data, label_index + 2, length_index+2, max_bar_label)

# chart(data) consumes a list called data and prints out a bar chart
# horizontally using text and symbols to draw the display
# effects: prints the data labels from data along with #'s representing 
# the bars bars
# requires: the format of data will always have an even number of elements
#           where a bar label string is followed by a natural number for the
#           bar length
# Examples
# calling chart(["VW", 20, "Ford", 18, "Subaru", 3, 
#                "Mercedes", 7, "Fiat", 2]) prints:
#|       VW |####################
#|     Ford |##################
#|   Subaru |###
#| Mercedes |#######
#|     Fiat |##
# calling chart(["Rose", 1, "Lebron", 6, "Lowry", 7, "Kobe", 24, "Durant", 35]) prints:
#|    Rose |#
#|  Lebron |######
#|   Lowry |#######
#|    Kobe |########################
#|  Durant |###################################
 
def chart(data):
    max_bar_label = bar_label_length(data, len(data[0]), 0) + 2
    chart_acc(data, 0, 1, max_bar_label)