#!/usr/bin/python

from __future__ import print_function
import sys, numpy

def abs(x):
  if x < 0:
    x = 0 - x
  return x

# INPUT:
# P   - number of code partitions
# W,H - width and height of the grid processors
# u_0, v_0, f_0
# u_1, v_1, f_1
# until EOF

# OUTPUT:
# N - which is W*H
# D: N x N matrix
# F: N x N matrix
if __name__ == "__main__":
  inp = open(sys.argv[1], 'r')

  # P W H
  s = inp.readline().split()
  assert(len(s) == 3)
  p,w,h = [int(x) for x in s]
  n = w*h
  assert(n >= p)

  # N
  print(n)
  print()

  # D - manhattan distrance matrix
  for x1 in xrange(h):
    for y1 in xrange(w):
      for x2 in xrange(h):
        for y2 in xrange(w):
          print(abs(x1-x2) + abs(y1-y2),end=' ')

      print()
  print()

  # F - Flow
  f = numpy.zeros((n,n))
  s = inp.readline()
  while(s):
    s = s.split()
    if len(s) == 3:
      # u v f_uv
      (u,v,w) = [int(x) for x in s]
      f[u][v] = f[u][v] + w
      f[v][u] = f[v][u] + w
      assert(u != v)
    elif len(s) != 0:
      raise "Flow information is given as 'u v f' per line."
    s = inp.readline()
      
  for row in f:
    for x in row:
      print(int(x),end=' ')
    print()
  
