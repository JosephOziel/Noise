from collections import deque

#------DEQUE STUFF------
def create_empty():
    return deque()

def create_with(list):
    return deque(list)

def append_right(deq, a):
    deq.append(a)
    return deq

def append_left(deq, a):
    deq.appendLeft(a)
    return deq

def pop_right(deq):
    deq.pop()
    return deq

def pop_left(deq):
    deq.popLeft()
    return deq

def extend_right(deq, list):
    deq.extend(list)
    return deq

def extend_left(deq, list):
    deq.extendLeft(list)
    return deq

def clear(deq):
    deq.clear()
    return deq

def copy(deq):
    return deq.copy()

def count(deq, a):
    return deq.count(a)

def index(deq, i):
    return deq[i]

def remove_index(deq, i):
    del deq[i]
    return deq

def insert(deq, i, a):
    deq.insert(i, a)
    return deq

def remove(deq, a):
    deq.remove(a)
    return deq

def rotate(deq, n):
    deq.rotate(n)
    return deq

#------------------------