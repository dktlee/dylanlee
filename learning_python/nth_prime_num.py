##
## Dylan Lee
## Introduction to Computer Science (2015)
##

import math

## is_prime(n) produces True if n is a prime number or false otherwise
## is_prime: Nat -> Bool
## requires: 1 <= n < 126

def is_prime (n):
    return find_prime (2, n)

## find_prime (k, b) produces True if b has no natural divisors between 2 
## and itself thus making kb a prime number. If b has atleast one divisors
## between 2 and itself, then b is not a prime so it produces false
## find_prime: Nat Nat -> True
## requires: the initial call of k must be 2 to indicate where to start
##          counting up from to determine divisors

def find_prime (k, b):
    if k > math.sqrt(b):
        return True
    elif (b % k) != 0:
        return find_prime (k+1, b)
    else:
        return False
    
## nth_prime_acc (n, curr_num) produces the n-th prime number, which will be
## curr_num once n decreases to a value of 1 by recurssing
## nth_prime_acc: Nat Nat -> nat
## requires: the initial call of curr_num must be 2 to indicate the first
##           prime number is 2

def nth_prime_acc (n, curr_num):
    if (n == 1 and is_prime(curr_num)):
        return curr_num
    elif (n == 1 and not(is_prime(curr_num))):
        return nth_prime_acc (n, curr_num + 1)
    elif is_prime(curr_num):
        return nth_prime_acc (n - 1, curr_num +1)
    else:
        return nth_prime_acc (n, curr_num +1)
    
## nth_prime(n) prodcues the nth prime number starting with 2 as the first
## prime number
## nth_prime: Nat -> Nat
## requires: 1 <= n < 126
## Examples
## nth_prime(1) -> 2
## nth_prime(3) -> 5
## nth_prime(6) -> 13

def nth_prime(n):
    return nth_prime_acc (n, 2)