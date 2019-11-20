##
## Dylan Lee
## Introduction to Computer Science (2015)
##

# bubble_sort_acc(lst, first_index, second_index) produces a list like lst
# but in sorted descending order by swapping the numbers located at the 
# first_index of lst with the number at the second_index of lst if the first
# is smaller than the second, moving all bigger numbers to the front and 
# smaller numbers to the back of the list

def bubble_sort_acc(lst, first_index, second_index):
    if second_index>=len(lst):
        return bubble_sort(lst[:-1]) + [lst[second_index - 1]]    
    elif lst[first_index] < lst[second_index]:
        lst[first_index],lst[second_index]=lst[second_index],lst[first_index]
        return bubble_sort_acc(lst, first_index + 1, second_index + 1)
    else:
        return bubble_sort_acc(lst, first_index + 1, second_index + 1)
    
# bubble_sort(lst): produces the same list of of items in lst but in 
# descending order
# Examples
# bubble_sort([1,9,2,8,3,7]) => [9,8,7,3,2,1]
# bubble_sort([-33, -1, 4, 2, 23]) => [23,4,2,-1,-33]
# bubble_sort([1,2,3,4,5,6,7,8]) => [8,7,6,5,4,3,2,1]

def bubble_sort(lst):
    if lst == [] or len(lst) == 1:
        return lst
    else:
        return bubble_sort_acc (lst[:], 0, 1)