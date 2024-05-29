
import math

# distance between two points
def dist(a, b):
    xlength = a[0] - b[0]
    ylength = a[1] - b[1]
    return math.sqrt(xlength * xlength + ylength * ylength)

# brute force solution
def bruteforce(Px):
    p1 = Px[0]
    p2 = Px[1]
    dist1 = dist(p1, p2)
    length = len(Px)
    if length == 2:
        return p1, p2, dist1
    p3 = Px[2]
    dist2 = dist(p1, p3)
    dist3 = dist(p2, p3)
    mindist = min(dist1, dist2, dist3)
    if mindist ==  dist1:
        return p1, p2, dist1
    elif mindist == dist2:
        return p1, p3, dist2
    else:
        return p2, p3, dist3

# shamos closest pair
def shamosclosestpair(templist):
    Px = templist
    Px.sort(key = lambda x: x[0])
    Py = templist
    Py.sort(key = lambda x: x[1])
    p1, p2, mindist = recurse(Px, Py)
    return p1[2], p2[2], mindist

# compare between pairs
def minDistPair(Pl, Pr, Ps):
    dist1 = Pl[2]
    dist2 = Pr[2]
    dist3 = Ps[2]
    mindist = min(dist1, dist2, dist3)
    if mindist == dist1:
        return Pl
    elif mindist == dist2:
        return Pr
    else:
        return Ps

def recurse(Px, Py):
    length = len(Px)
    midindex = length // 2
    if(length < 4):
        return bruteforce(Px)
    # split into 2
    Lx = Px[:midindex]
    Rx = Px[midindex:]

    midpoint = Px[midindex][0]  
    Ly = list()
    Ry = list()
    for x in Py:
        if x[0] <= midpoint:
           Ly.append(x)
        else:
           Ry.append(x)
    # left interval
    pairL = recurse(Lx, Ly)
    # right interval
    pairR = recurse(Rx, Ry)
    dist1 = pairL[2]
    dist2 = pairR[2]
    delta = 0
    if dist1 <= dist2:
        delta = dist1
        minpair = (pairL[0], pairL[1])
    else:
        delta = dist2
        minpair = (pairR[0], pairR[1])
    # between two intervals
    pairS = minimum_span_pair(Px, Py, delta, minpair)
    # return minDistPair
    return minDistPair(pairL, pairR, pairS)
    
def minimum_span_pair(Px, Py, delta, min_pair):
    length = len(Px)
    midindex = length // 2
    xmid = Px[midindex][0]

    s = [x for x in Py if abs(xmid - x[0]) <= delta]
    minimum = delta
    len_s = len(s)
    for i in range(len_s - 1):
        for j in range(i+1, len_s):
            p = s[i]
            q = s[j]
            dst = dist(p, q)
            if dst < minimum:
                min_pair = p, q
                minimum = dst
    return min_pair[0], min_pair[1], minimum

def main():
    totalcount = int(input())
    for i in range(totalcount):
        eachlength = int(input())
        targetlist = []
        for j in range(eachlength):
            tmplist = list(map(float,input().split()))
            tmplist.append(j)
            targetlist.append(tmplist)
        mindist = shamosclosestpair(targetlist)
        print(mindist[2])
        print(str(mindist[0]) + " " + str(mindist[1]))
main()