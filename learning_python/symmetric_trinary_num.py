##
## Dylan Lee
## Introduction to Computer Science (2015)
##

# sym_tri_num_acc(old_list, adder, new_list) produces a list, new_list, which
# is just the list of symmetric trinary numbers with 2 numbers added to the 
# front and back of each symmetric trinary number of length 2 less than 
# currently desired. adder is the digit we have to add at the beginning and
# end of the old_list of trinary numbers to get new_list of trinary numbers
# sym_tri_num_acc: (listof Str) Nat (listof Str) -> (listof Str)
# Examples
# notice that if we want a list of all symmetric trinary numbers of length
# 3 we just have to add 0's, 1's, and 2's respectively, to the beginning
# and end of the list of symmetric trianary numbers of length 1 
# this pattern is true for all n
# requires: we require the first call of old_list is the list of all 
# symmetric trinary numbers of length n-2 if we want the new_list to be
# the list of symmetric trinary numbers to be n
# also, we require the first call of adder to be 0 and new_list to be []

def sym_tri_num_acc(old_list, adder, new_list): 
    if adder == 2:
        new_list = new_list + list(map(lambda x: str(adder) + x + str(adder), old_list))
        return new_list
    else:
        new_list = new_list + list(map(lambda x: str(adder) + x + str(adder), old_list))
        return sym_tri_num_acc(old_list, adder+1, new_list)


# sym_tri_num(n) produces a list of all symmetric trinary numbers of length in
# ascending order based on a base-3 number systerm which only uses digits 
# 0, 1, and 2. A symmetric trinary number is a trinary number that is a number
# that has the same first digit as the last, same second digit as the 
# second last and so on
# sym_tri_num: Nat -> (listof Str)
# requires: n >= 1
# Examples
# sym_tri_num(1) => ['0','1','2']
# sym_tri_num(2) => ['00','11','22']
# sym_tri_num(3) => ['000','010','020','101','111','121','202','212','222']
# sym_tri_num(4) => ['0000','0110','0220','1001','1111','1221','2002','2112','2222']

def sym_tri_num(n):
    if n==1:
        return ['0','1','2']
    elif n==2:
        return ['00','11','22']
    else:
        return sym_tri_num_acc(sym_tri_num(n-2), 0, [])