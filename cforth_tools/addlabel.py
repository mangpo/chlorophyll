#!/usr/bin/python
'add labels to assembly-language dump'
import sys, os, re
from collections import defaultdict
WATCHED = ['js', 'jns', 'jz', 'jnz', 'jc', 'jnc', 'jg', 'jng', 'loop', 'loope']
TARGETS = {}
BY_ADDRESS = {}
LABELS = defaultdict(list)
def find_unlabeled(contents):
 'find all lines with hard-coded addresses we need to fix'
 for line in list(contents):
  if line[:27].isspace() and line.split()[0].endswith(':'):
   parts = [line[0:27]] + line[27:].split()
   label = parts[1][:-1]  # chop colon
   number = address(label)
   print 'label %s = %s' % (label, number)
   LABELS[number].append(label)
  else:
   parts = [line[0:28]] + line[28:].split()
   if parts[0][0].isdigit():
    BY_ADDRESS[int(parts[0].split()[0], 16)] = line
  if len(parts) > 2 and parts[1] in WATCHED and parts[2].startswith('0x'):
   target = parts[2]
   number = address(target)
   TARGETS[number] = [target, line]
   print 'target %s = %s' % (target, number)
def readfile(filename):
 'read in a file, closing it properly'
 input = open(filename)
 data = map(str.rstrip, input.readlines())
 input.close()
 return data
def writefile(filename, contents):
 'write out a file, closing it properly'
 output = open(filename, 'w')
 for line in contents:
  print >>output, line
 output.close()
def address(label):
 'return the address of a label as number'
 match = re.compile('[a-z]*0x([0-9a-f]+)').match(label)
 if match:
  return int(match.groups()[0], 16)
def fixup(filename):
 'edit the file in-place, adding labels where needed'
 contents = readfile(filename)
 find_unlabeled(contents)
 for number in TARGETS:
  target, line = TARGETS[number]
  label = ''
  if number in LABELS:
   label = LABELS[number][0]
  else:
   if number in BY_ADDRESS:
    label = 'branch' + target
    LABELS[number] = [label]
    label_line = BY_ADDRESS[number]
    index = contents.index(label_line)
    contents.insert(index, (' ' * 27) + label + ':')
  if label:
   index = contents.index(line)
   print 'replacing %s in "%s" with %s' % (target, line, label)
   contents[index] = line.replace(target, label, 1)
 writefile(filename, contents)
if __name__ == '__main__':
 print fixup(sys.argv[1])
