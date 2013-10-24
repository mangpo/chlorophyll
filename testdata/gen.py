#!/usr/bin/python

import random, sys

if __name__ == "__main__":
  n = int(sys.argv[1])
  min = 0
  max = int(sys.argv[2])

  if max < 0:
    min = max
    max = -max

  delim = False
  if len(sys.argv) > 3:
    delim = sys.argv[3]
  # for i in xrange(n):
  #   sys.stdout.write(str(random.randint(0,max)))
  #   sys.stdout.write(' ')
  # sys.stdout.write('\n')

  for i in xrange(n):
    print random.randint(min,max),
    if delim:
      print delim,
  print ""
