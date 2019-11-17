##
## Dylan Lee
## Introduction to Computer Science (2015)
##

class Time:
#   Fields: hours, minutes, seconds
#      requires:
#         0 <= hours <= 23
#         0 <= minutes <= 59
#         0 <= seconds <= 59
    
# Constructor: Creates a Time object by calling Time(h,m,s),
# requires: 0 <= hour < 24, 0 <= minute, second < 60
    def __init__(self, hours,minutes,seconds):
        self.hours=hours
        self.minutes=minutes
        self.seconds=seconds

# String Representation: Implicitly called by print(t), where t is of time
# Time
    def __repr__(self):
        hours=str(self.hours)
        if self.minutes < 10:
            minutes= "0"+str(self.minutes)
        else:
            minutes=str(self.minutes)
        if self.seconds < 10:
            seconds= "0"+str(self.seconds)
        else:
            seconds=str(self.seconds)
        return hours+":"+minutes+":"+seconds
    
# Equality Test: Implicitly called when calling t1 == t2 or t1 != t2, where
# t1 is a Time value, and t2 is of type Any
    def __eq__(self, other):
        return isinstance(other, Time) and\
               self.hours == other.hours and\
               self.minutes == other.minutes and\
               self.seconds == other.seconds


# next_event(now, events): produces a Time object that represents the 
# next event in events, which is a list of Time's. The next event is the
# next time that will occur given the time now. 
# Examples 
# next_event(Time(17,23,10), [Time(15,45,0), Time(6,30,0), Time(22,0,0)]) => Time(22,0,0)
# next_event(Time(22,0,10), [Time(15,45,0), Time(6,30,0), Time(22,0,0)]) => Time(6,30,0)
# next_event(Time(15,45,0), [Time(15,45,0), Time(6,30,0), Time(22,0,0)]) => Time(15,45,0)

def next_event(now, events):
    wait_time_list=[] # list that will contain the time difference between now and the times in events
    
    if events == []:
        return None
    else:
        for x in events:
            second_difference = x.seconds - now.seconds
            minute_difference = (x.minutes - now.minutes) * 60
            hour_difference = (x.hours - now.hours) * 3600
            wait_time = second_difference + minute_difference + hour_difference # this is the total difference in time in seconds between now time and time in events
            
# if the difference in time is negative, that means the time from the events list is before the now time, which means that event occurs on the next day, therefore we must add 24 hours to that time            
            if wait_time < 0: 
                wait_time = wait_time + 24*60*60
                
            wait_time_list.append(wait_time)
        
        index=wait_time_list.index(min(wait_time_list))
        return events[index]            