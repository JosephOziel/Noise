from collections import deque

#------DEQUE STUFF------
def create_empty():
    return deque()

def create_with(list):
    return deque(list)

def append_right(deq, a):
    deq = deque(deq)
    deq.append(a)
    return deq

def append_left(deq, a):
    deq = deque(deq)
    deq.appendleft(a)
    return deq

def pop_right(deq):
    deq = deque(deq)
    d = deq.pop()
    return (deq, d)

def pop_left(deq):
    deq = deque(deq)
    d = deq.popleft()
    return (deq, d)

def extend_right(deq, list):
    deq = deque(deq)
    deq.extend(list)
    return deq

def extend_left(deq, list):
    deq = deque(deq)
    deq.extendleft(list)
    return deq

def clear(deq):
    deq = deque(deq)
    deq.clear()
    return deq

def copy(deq):
    deq = deque(deq)
    return deq.copy()

def count(deq, a):
    deq = deque(deq)
    return deq.count(a)

def index(deq, i):
    deq = deque(deq)
    return deq[i]

def remove_index(deq, i):
    deq = deque(deq)
    del deq[i]
    return deq

def insert(deq, i, a):
    deq = deque(deq)
    deq.insert(i, a)
    return deq

def remove(deq, a):
    deq = deque(deq)
    deq.remove(a)
    return deq

def rotate(deq, n):
    deq = deque(deq)
    deq.rotate(n)
    return deq

#------------------------