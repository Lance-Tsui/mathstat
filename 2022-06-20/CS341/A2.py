
from queue import Queue



field = dict()

nodequeue = Queue(maxsize=0)

# fire duration
duration = 0

# corns remaining
remain = 0

# fire time
firetime = 0

number = int(input())
for i in range(number):
    row = input().split()
    for j in range(number):
        # water
        if row[j] == 'w':
            field[(i,j)] = 0
        else:
            field[(i,j)] = 1
            remain = remain + 1
start = input().split()
start_x = int(start[0]) - 1
start_y = int(start[1]) - 1
nodequeue.put([start_x, start_y])
firetime = 1
while not nodequeue.empty():
    # if thie is burnable or not
    coord = tuple(nodequeue.get())
    # then find adjancent coords
    fire = field.get(coord)
    if fire:
        field[coord] = 0
        
    else:
        
print(duration)
print(remain)
print(firetime)
    


    
