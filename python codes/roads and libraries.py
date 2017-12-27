#!/bin/python3

import sys

q = int(input().strip())
for a0 in range(q):
    n, m, x, y = input().strip().split(' ')
    n, m, x, y = [int(n), int(m), int(x), int(y)]
    
    ns=[]
    c1,c2=input().strip().split(' ')
    c1,c2=[int(city_1), int(city_2)]
    ns.append([c1,c2])
    for a1 in range(1,m):
        city_1, city_2 = input().strip().split(' ')
        city_1, city_2 = [int(city_1), int(city_2)]
        if x < y : print(x*n)
        else : 
            for i in ns :
            if (city_1 in ns[i] and (not (city_2 in ns[i]))) : 
                ns[i].append(city_2)
            elif (city_2 in ns[i] and (not (city_1 in ns[i]))): 
                ns[i].append(city_1)
            elif not(city_1 in ns[i] and city_2 in ns[i]) :
                ns.append([city_1,city_2])
                
                
