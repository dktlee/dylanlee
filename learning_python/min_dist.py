##
## Dylan Lee
## Introduction to Computer Science (2015)
##

import math

## min_dist (x1,x1,x2,y2,x3,y3) produces the house with shortest overall distance for the other two friends to travel
## to the chosen house, so its the house contained by the minimum of the two sides of the triangle made up by the three houses

def min_dist(x1,y1,x2,y2,x3,y3):
    distance_1_to2 = math.sqrt ((x1-x2)**2 + (y1-y2)**2)
    distance_1_to3 = math.sqrt ((x1-x3)**2 + (y1-y3)**2)
    distance_3_to2 = math.sqrt ((x3-x2)**2 + (y3-y2)**2)
    
    person1_stays_home = distance_1_to2 + distance_1_to3
    person2_stays_home = distance_1_to2 + distance_3_to2
    person3_stays_home = distance_1_to3 + distance_3_to2
    
    if person1_stays_home == min (person1_stays_home, person2_stays_home, person3_stays_home):
        house = 1
    elif person2_stays_home == min(person1_stays_home, person2_stays_home, person3_stays_home):
        house = 2
    else:
        house = 3
    return "House"+str(house)