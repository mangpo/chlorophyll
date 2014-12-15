#!/usr/bin/python
"""\
huffman-encode and decode colorforth words

The 4-bit letters begin with zero, and have a value exactly the same as
their offset in the translation table. 5-bit letters begin with a one, and
are the second set of 8 characters in the table, and so begin with the
value 16, or 8 plus their offset. 7-bit letters, the remainder, all begin
with binary 11, and start at offset 16; 0b1100000 is decimal 96, so one 
needs to add 80 to the offset to create the 7-bit value.

See http://colorforth.com/chars.html, and updates in DB004 arrayForth
User's Manual, p. 42, from GreenArrays.com.
"""
import sys, os, struct, re
# the old huffman code is from http://www.colorforth.com/chars.html
OLDCODE  = ' rtoeani' + 'smcylgfw' + 'dvpbhxuq' + 'kzj34567' + \
           '891-0.2/' + ';:!+@*,?'
# new Huffman encoding from Tim Neitz
NEWCODE  = ' rtoeani' + 'smcylgfw' + 'dvpbhxuq' + '01234567' + \
           '89j-k.z/' + ";'!+@*,?"
CODE = NEWCODE  # assume Tim knows what he's doing
CODEDICT = dict([[c + (8 * (8 <= c < 16)) + (80 * (c >= 16)), CODE[c]]
 for c in range(len(CODE))])
HIGHBIT = 0x80000000L
MASK =    0xffffffffL
TRAILING_ZEROS = dict([[1 << n, n] for n in range(32)])
FUNCTIONS = [  #from tag in low 4 bits of each compressed 32-bit word
 # 'yellow' and 'green' become 'brown' and 'pine' if hexadecimal;
 # bit 4 of the word is 1 if it should be rendered as hex
 ['extension', 'white'],  # 0
 ['executeword', 'yellow'],
 ['executelong', 'yellow'],
 ['definition', 'red'],
 ['compileword', 'green'],  # 4
 ['compilelong', 'green'],
 ['compileshort', 'green'],
 ['compilemacro', 'cyan'],
 ['executeshort', 'yellow'],  # 8
 ['textnocaps', 'white'],
 ['textcapitalized', 'white'],
 ['textallcaps', 'white'],
 ['variable', 'magenta'],  # 12
 ['feedbackshort', 'silver'],
 ['formatting', 'blue'],
 ['commentedshort', 'white']
]
TAGS = [function[0] for function in FUNCTIONS]
COLORS = [function[1] for function in FUNCTIONS]
DEBUGLEVEL = int(os.getenv('DEBUGLEVEL') or '0')
def bits_unused(letter):
 """28 bits are available for compressing text

    but only 1-bits count; if a letter ends in zeroes, as does "@"
    with an encoding of 0b1111100, only 5 bits are deemed to have
    been used, so this should return 2 for the 2 unused bits."""
 if letter is None: return 0  # in case word was 0, lettercode undefined
 first_one_bit = letter & -letter  # Gosper's hack
 return TRAILING_ZEROS[first_one_bit]
def unpack_binary(coded):
 'return untagged number'
 return '[0x%x]' % (coded & 0xffffffff)
def pack_binary(string):
 'return raw binary number'
 return eval(string)[0]
def unpack(coded):
 'show packed number as text'
 text, bits, tag = '', 32 - 4, TAGS[coded & 0xf]  # low 4 bits are the tag
 # now get rid of sign bit and remove the tag from the number
 # this only works because coded is a long
 number, lettercode = coded & 0xfffffff0, None
 while number:
  lettercode, codebits, number = number >> 28, 4, (number << 4) & MASK
  fivebits = bool(lettercode & 8)  # at least 5 bits if high bit set
  sevenbits = fivebits and bool(lettercode & 4)  # 7 bits if top two bits set
  for iteration in range(fivebits + sevenbits + sevenbits):
   lettercode = (lettercode << 1) | bool(number & HIGHBIT)
   number = (number << 1) & MASK
   codebits += 1
  text, bits = text + CODEDICT[lettercode], bits - codebits
 if ' ' in text or bits + bits_unused(lettercode) < 0 or not len(text):
  debug('Cannot unpack, bits remaining: %d, text: "%s"' % (bits, text), 2)
  return [unpack_binary(coded), None]  # it wasn't a valid word
 elif tag.endswith(('short', 'long')):
  return [unpack_binary(coded), None]
 else:
  return [text, tag]
def pack(word):
 'pack text into number'
 word, packed, bits, unused, unpacked = word.lower(), 0, 28, 0, ''
 debug('packing "%s"' % word, 2)
 for index in range(len(word)):
  letter = word[index]
  lettercode = CODE.index(letter)
  length = 4 + (lettercode > 7) + (2 * (lettercode > 15))
  lettercode += (8 * (length == 5)) + (80 * (length == 7))
  unused = bits_unused(lettercode)
  debug(('0x%x' % packed, '0x%x' % lettercode, bits, length - unused), 2)
  if length - unused > bits:
   unpacked = word[index:]
   debug('cannot pack "%s", returning packed "%s"' % (word, word[:index]), 2)
   break
  else:
   packed, bits = (packed << length) | lettercode, bits - length
   if bits < 0:
    packed, bits = packed >> -bits, 0
 packed <<= (bits + 4)
 return [packed, unpacked]
def debug(message, debuglevel = 0, duplicate = None):
 if DEBUGLEVEL >= debuglevel:
  print >>sys.stderr, message
 if duplicate:
  print >>duplicate, message
if __name__ == '__main__':
 for word in sys.argv[1:]:
  if word.startswith('0x'):
   print unpack(int(word, 16))[0],
  else:
   print '0x%x' % pack(word)[0],
 print
