#!/usr/bin/python

import random, sys

if __name__ == "__main__":
  n = int(sys.argv[1])
  max = int(sys.argv[2])

  delim = False
  if len(sys.argv) > 3:
    delim = sys.argv[3]
  # for i in xrange(n):
  #   sys.stdout.write(str(random.randint(0,max)))
  #   sys.stdout.write(' ')
  # sys.stdout.write('\n')

  for i in xrange(n):
    print random.randint(0,max),
    if delim:
      print delim,
  print ""
