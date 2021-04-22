#!/usr/bin/python3
import os

l = []
with open('t.txt') as f:
  for i in f:
    l.append(int(i))
for i in range(0, len(l)):
  temp = open("temp.txt", "w")
  temp.write(str(l[i]))
  os.system("./a.out < temp.txt")
