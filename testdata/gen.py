#!/usr/bin/python

import random, sys

if __name__ == "__main__":
  n = int(sys.argv[1])
  max = int(sys.argv[2])
  for i in xrange(n):
    sys.stdout.write(str(random.randint(0,max)))
    sys.stdout.write(' ')
  sys.stdout.write('\n')
