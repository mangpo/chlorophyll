#!/usr/bin/python
'convert relative call or jmp address to EIP address'
import sys, os
def relative(offset, instruction_address):
 return '0x%x' % ((offset + instruction_address) & 0xffffffff)
if __name__ == '__main__':
 if len(sys.argv) < 3:
  print >>sys.stderr, 'Usage: %s OFFSET ADDRESS_OF_INSTRUCTION'
 else:
  print relative(int(sys.argv[1], 16), int(sys.argv[2], 16))
