##
## Dylan Lee
## Introduction to Computer Science (2015)
##

# calculator (question, answer) produces whether answer is equal to the 
# arithmetic question when it is evaluated 

def calculator (question,answer):
    if question[question.find(" ")+1] == '+':
        if int(question[:question.find(" ")]) + \
           int(question[question.find(" ", question.find(" ") +1)+1:]) == \
            answer:
            return 'Correct'
        else: 
            return 'Wrong'
    elif question[question.find(" ")+1] == '-':
        if int(question[:question.find(" ")]) - \
           int(question[question.find(" ", question.find(" ") +1)+1:]) == \
            answer:
            return 'Correct'
        else: 
            return 'Wrong'
    else:
        if int(question[:question.find(" ")]) * \
           int(question[question.find(" ", question.find(" ") +1)+1:]) == \
            answer:
            return 'Correct'
        else: 
            return 'Wrong'        
        

# math_drill_acc(questions, num_correct, index) produces the number of correct
# answers, num_correct, that the user answers to the question at index
# requires: question must be a string with two operands that surround one 
#           operator like "31 * 3"
#                   the operands must be integers, the operator is +,- or *
#                   there is one space between the operator and operands
#           first call of num_correct is 0
#           first call of index is 0

def math_drill_acc(questions, num_correct, index):
    if index == len(questions):
        return num_correct
    else:
        answer = int(input("What is " +questions[index] + "? "))
        print(calculator(questions[index], answer))
        if calculator(questions[index], answer) == 'Correct':
            return math_drill_acc(questions, num_correct+1, index+1)
        else:
            return math_drill_acc(questions, num_correct, index+1)


# math_drill (questions) produces the number of correct 
# answers the user gets when answer all the questions in questions
# requires: question must be a string with two operands that surround one 
#           operator like "31 * 3"
#                   the operands must be integers, the operator is +,- or *
#                   there is one space between the operator and operands
# Examples
# If the user enters 2, 20, and 999 when prompted to be the 3 answers to
#     the 3 questions, Correct, Wrong, and Correct are printed on 3 different
#     lines and math_drill(["1 + 1", "3 * 5", "1000 - 1"]) => 2
# If the user enters 50, and 191 when prompted to be the 2 answers to the 
#     2 questions, then Wrong and Wrong are printed on 2 different lines and
#     math_drill(["100 * 5", "222 - 32"]) => 0

def math_drill(questions):
    return math_drill_acc(questions, 0, 0)