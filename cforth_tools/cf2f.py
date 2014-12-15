#!/usr/bin/python
'''\
simple colorforth to forth / forth to colorforth translator

see Charley Shattuck's post for the general idea:
http://www.strangegizmo.com/forth/ColorForth/msg00209.html
in addition, we will use [compile] in front of cyan words

also, we will use angle brackets, e.g. < macro >, around words defined in the
kernel to mark those with no tag, otherwise words outside of definitions
are assumed to be yellow (executed).

markup_ functions are helpers for cf2f, to "mark up" the bare words with
indicators as to their "color", i.e. syntax, in those cases where positional
or other clues are insufficient.

markdown_ functions are the opposite; used by f2cf to change markup into
the syntax "tags" used by colorforth.
'''
import sys, os, struct, re
try:
 from cfword import *
except:  # this should work with pytest
 sys.path.append('.')
 import cfword
 from cfword import *
# http://colorforth.com/parsed.html
PATTERNS = [  # no need to worry about spaces because we split on whitespace
 # name, input
 ['binary', '^\[0x[0-9a-f]{1,8}\]$'],
 ['shortdecimal', '^-?[0-9]{1,9}$'],  # match short before long
 ['shorthex', '^\$[0-9a-f]{1,8}$'],
 ['longdecimal', '^-?[0-9]{9,18}$'],
 ['longhex', '^\$[0-9a-f]{8,15}$'],
 ['textcapitalized', '^[A-Z][^A-Z]+$'],
 ['textallcaps', '^[A-Z][^a-z]*$'],
 ['textnocaps', '^[^A-Z]+$'],
]
MAPPING = {
 'execute': {
  'longhex': 'executelong',
  'longdecimal': 'executelong',
  'shorthex': 'executeshort',
  'shortdecimal': 'executeshort',
  'textnocaps': 'executeword',
 },
 'compile': {
  'longhex': 'compilelong',
  'longdecimal': 'compilelong',
  'shorthex': 'compileshort',
  'shortdecimal': 'compileshort',
  'textnocaps': 'compileword',
  'executeword': 'compileword',  # convert previously-marked execute
 },
} 
ACTIONS = {  # used to trigger markdown_* actions in f2cf
 ':': 'define',
 '[': 'execute',
 '(': 'comment',
 '<': 'bareword',
 '$': 'numeric',
 ':var': 'var',  # cannot use 'var' because arrayforth uses it in block 92
 '[compile]': 'macro',
 '^': 'capitalized',
 '^^': 'allcaps',
 '|': 'formatting',
 '#': 'commented',
 '=': 'feedback',
 ']': 'compile',
 # these last should never be encountered...
 ')': None,  # they are just there for markup_text (q.v.)
 '>': None,
}
MARKUP = {  # tagname patterns to trigger markup_ in cf2f
 'highlevel': [
  ['^compilemacro$', 'macro'],  # must go before compile
  ['^compile', 'compile'],
  ['^execute', 'execute'],
  ['^definition$', 'definition'],  # set state to 'compile'
  ['^text', 'text'],
  ['^variable$', 'variable'],
  ['.*word$', 'numeric'],  # this check must come *after* compile and execute
  ['^formatting$', 'formatting'],  # "blue words": cr, br, indent, etc.
  ['^commented', 'commented'],  # new "commented number" tag 0xf
  ['^feedback', 'feedback'],  # new "compiler feedback number" tag 0xd
 ],
 'binary': [
  ['^extension$', 'bareword'],
 ]
}
SIGNBIT = (1 << 26)
MAXSHORT = SIGNBIT - 1  # 27 bits max colorforth "short" integer
INT = 0xffffffff
DEFAULT = ['execute']  # default action, reset at start of each block
# bit 27 is the sign bit, so the above is the largest positive number
def cf2f(infile = sys.stdin, output = sys.stdout):
 blocks = getbinary(infile)
 for index in range(len(blocks)):
  DEFAULT[:] = ['execute']  # reset default action
  print >>output, '{block %d}' % index
  block = getwords(blocks[index])
  debug('unpacking block %d' % index, 2)
  unpack_all(block)
  output_text(block, output)
def output_text(block, output):
 for i in range(len(block)):
  if block[i][0] == ':' and not nocr(block, i - 1):
   output.softspace = False
   print >>output
  print >>output, block[i][0],
  if block[i][0] in ['cr', 'br', 'indent'] and block[i][1] == 'formatting':
   output.softspace = False
   print >>output
   if block[i][0] == 'br':  # double newline
    print >>output
   elif block[i][0] == 'indent':
    print '    ',  # 4 spaces plus one added by Python
 print >>output  # end of block
 print >>output
def nocr(block, index):
 return padblock(block)[index][0] == '-cr'
def is_empty(block):
 if len(block):
  if type(block[0]) is list:
   empty = [['[0x0]', None]] * len(block)
  else:
   empty = [0] * len(block)
  return block == empty
 else:
  return True
def pack_number(block, index, execute = (DEFAULT[-1] == 'execute')):
 'pack one- or two-word number and return updated index'
 base = [10, 16][block[index].startswith('$')]
 if not block[index].startswith('['):
  block[index] = int(block[index][base == 16:], base)
  if abs(block[index]) > MAXSHORT:
   short = False
   block.insert(index, 0)
  tag = ['compile', 'execute'][execute] + ['long', 'short'][short]
  block[index] = block[index] << 5 | hex << 4 | TAGS.index(tag)
  return index + 1 + (not short)
 else:
  block[index] = pack_binary(block[index][0])
  return index + 1
def unpack_number(block, index):
 'unpack one- or two-word number and return updated index'
 tag = TAGS[block[index] & 0xf]
 if tag.endswith(('short', 'long')):
  hex = bool(block[index] & 0x10)
  debug('unpacking number %s, tag=%s, hex=%s' % (block[index], tag, hex), 2)
  block[index] >>= 5
  if tag.endswith('long'):
   if block[index] != 0 or not block[index + 1:]:
    block[index] = [unpack_binary(TAGS.index(tag) | (hex << 4)), None]
    return index + 1, True
   else:
    block.pop(index)  # puts value in current slot
  if hex:
   block[index] = ['$%x' % (sign_extend(block[index], tag) & INT), tag]
  else:
   block[index] = ['%d' % block[index], tag]
  return index + 1, True
 else:
  return index, False
def sign_extend(number, tag):
 if tag.endswith('short') and number > MAXSHORT:
  number |= -SIGNBIT
 return number
def unpack_all_binary(block, index = 0):
 'go through raw numbers and unpack as [0xn] or untagged strings'
 while not is_empty(block[index:]):
  unpacked = unpack(block[index])
  if unpacked[1] != 'extension':
   unpacked = [unpack_binary(block[index]), None]
  block[index] = unpacked
  index += 1
 block[index:] = []
 markup(block, 'binary')
def unpack_all(rawblock, index = 0):
 'go through raw numbers and unpack into number and text strings'
 block = list(rawblock)  # copy in case we find it's not high-level forth
 while not is_empty(block[index:]):
  if padblock(block)[index - 1][1] == 'variable':
   block[index] = [unpack_binary(block[index]), None]
   index += 1
  else:
   index, processed = unpack_number(block, index)  # try as number first
   if not processed:
    block[index] = unpack(block[index])
    index += 1
   if block[index - 1][1] == None:
    debug('marking binary at %s' % block[:index + 1], 2)
    return unpack_all_binary(rawblock, index = 0)
 block[index:] = []  # trim trailing zeros
 if not connect_extensions(block):
  return unpack_all_binary(rawblock, index = 0)
 markup(block, 'highlevel')
 rawblock[:] = block
def markup(block, blocktype = 'highlevel', index = 0):
 '''add syntactic cues for text-mode rendition of colorforth

    each markup_ function will return an index and offset;
    the index is where the next markup_ function will act, and
    the offset is the number of words added beyond the index.
    a special offset of None will indicate to the markup loop
    to skip any further processing.'''
 while block[index:]:
  item, word, tag, adjust = block[index], block[index][0], block[index][1], 0
  debug('marking up: %s' % item, 2)
  for pattern in MARKUP[blocktype]:
   if re.compile(pattern[0]).match(tag or 'NO_MATCH'):
    debug('marking up as %s' % pattern[1], 2)
    index, offset = eval('markup_' + pattern[1])(block, index)
    if offset == None:
     break
    else:
     adjust += offset
  debug('adjusting index %d by %d' % (index, adjust + 1), 2)
  index += adjust + 1
def markup_formatting(block, index):
 'signify a formatting word to differentiate format cr from kernel cr'
 block.insert(index, ['|', 'markup'])
 return index + 1, None
def markup_feedback(block, index):
 'signify a "compiler feedback" number'
 debug('compiler feedback number: %s' % block[index], 2)
 block.insert(index, ['=', 'markup'])
 return index + 1, None
def markup_commented(block, index):
 'signify a "commented" number rather than one compiled or executed'
 block.insert(index, ['#', 'markup'])
 return index + 1, None
def markup_definition(block, index):
 'put colon before defined word'
 block.insert(index, [':', 'markup'])
 DEFAULT[-1] = 'compile'
 return index + 1, None  # skip over colon
def markup_variable(block, index):
 debug('marking up variable %s' % block[index][0], 2)
 block.insert(index, [':var', 'markup'])
 index += 1
 if block[index + 1:] and block[index + 1][0] == '[0x0]':
  block.pop(index + 1)
  return index, None
 else:
  return index + 1, None  # skip "binary" number following
def markup_macro(block, index):
 debug('marking up macro %s' % block[index][0], 2)
 block.insert(index, ['[compile]', 'markup'])
 return index + 1, None
def markup_textallcaps(block, index):
 word, marked_up = block[index][0], block[index][0].upper()
 if marked_up in [word.lower(), word.capitalize()]:
  debug('explicitly marking %s as allcaps' % block[index], 2)
  block.insert(index, ['^^', 'markup'])
  index += 1
 block[index][0] = marked_up
 return index  # note this should NOT return tuple
def markup_textcapitalized(block, index):
 word, marked_up = block[index][0], block[index][0].capitalize()
 debug('as is, upper, capital: %s' % [word, word.upper(), marked_up], 2)
 if marked_up in [word.lower(), word.upper()]:
  debug('explicitly marking %s as capitalized' % block[index], 2)
  block.insert(index, ['^', 'markup'])
  index += 1
 block[index][0] = marked_up
 return index  # note this should NOT return tuple
def markup_text(block, index):
 if block[index][0] in ACTIONS:
  return index  # skip markup
 debug('marking up text %s' % block[index][0], 2)
 block.insert(index, ['(', 'markup'])
 index += 1
 while block[index:] and block[index][1].startswith('text'):
  if block[index][1] == 'textallcaps':
   index = markup_textallcaps(block, index)
  elif block[index][1] == 'textcapitalized':
   index = markup_textcapitalized(block, index)
  index += 1
 block[index - 1][0] += ')'
 return index - 1, None  # point to last commented word
def markup_execute(block, index):
 if DEFAULT[-1] == 'compile':
  debug('switching default to "execute" at %s' % end(block, index), 2)
  block.insert(index, ['[', 'markup'])
  DEFAULT[-1] = 'execute'
  index += 1
  return index, 0
 elif padblock(block)[index + 1][1] == 'definition':
  block.insert(index + 1, [']', 'markup'])
  DEFAULT[-1] = 'compile'
  return index, 1
 else:
  return index, 0
def end(block, index):
 'end of current part of block, for debugging'
 return block[max(0, index - 9):index + 1]
def markup_compile(block, index):
 if DEFAULT[-1] == 'execute':
  debug('switching default to "compile" at %s' % end(block, index), 2)
  block.insert(index, [']', 'markup'])
  DEFAULT[-1] = 'compile'
  index += 1
 if block[index][0] == ';':
  debug('switching default to "execute" at %s' % end(block, index), 2)
  DEFAULT[-1] = 'execute'
 return index, 0
def padblock(block):
 'pad so that block[i-1] or block[i+1] will always see [[None, None]] at end'
 return block + [[None, None]]
def connect_extensions(block, index = 0, ok = True):
 '''join extension[s] to previous word

    due to binary value following a variable declaration (magenta word),
    a variable name cannot have an extension. otherwise how could
    colorforth know whether it were an extension or the value?'''
 debug('connecting extensions in block: %s' % block, 2)
 while block[index:]:
  tag = block[index][1]
  if tag == 'extension':
   debug('found bad tag at: %s' % end(block, index), 2)
   ok = False
   break
  elif tag is None or tag.endswith(('variable', 'short', 'long')):
   index += 1  # none of these can have extensions
   continue
  while padblock(block)[index + 1][1] == 'extension':
   block[index][0] += block.pop(index + 1)[0]
  index += 1
 return ok
def is_decimal(string):
 'determine if string represents a decimal number'
 return re.compile('^-?[0-9]+$').match(string)
def markup_numeric(block, index):
 'mark a word that looks like a number'
 if is_decimal(block[index][0]) and block[index][1].endswith('word'):
  debug('marking up "%s" as numeric-looking word' % block[index], 2)
  block.insert(index, ['$', 'markup'])
  return index + 1, None
 else:
  debug('skipping markup of "%s" to number' % block[index], 2)
  return index, 0
def markup_bareword(block, index):
 block.insert(index, ['<', 'markup'])
 index += 1
 while block[index:] and block[index][1] == 'extension':
  index += 1
 block.insert(index, ['>', 'markup'])
 return index + 1, None
def gettag(word):
 tag = word & 0xf
 return TAGS[tag]
def checkblock(block, index = 0):
 '''get rid of ":var", ":", "[" and the like, and correct tags accordingly

    helper function for f2cf (text to packed [binary])'''
 while block[index:]:
  debug('block[%d] = %s, default: %s' % (index, block[index], DEFAULT), 2)
  if block[index][0] in ACTIONS:  # trigger on the syntax
   action = block.pop(index)  # now index points to following word
   debug('popped action trigger %s' % action, 2)
   index = eval('markdown_' + ACTIONS[action[0]])(block, index)
  elif block[index][0] is not None:  # trigger on the parsing
   debug('processing item %s' % block[index], 2)
   if block[index][1] in MAPPING[DEFAULT[-1]]:
    block[index][1] = MAPPING[DEFAULT[-1]][block[index][1]]
    if block[index] == [';', 'compileword']:
     DEFAULT[-1] = 'execute'
     debug('default now: %s, at word: %s' % (DEFAULT, block[index]), 2)
   index += 1
  else:
   raise Exception, 'found unexpected None in block %s' % block
 debug('checked: %s' % block, 2)
 return
def markdown_define(block, index):
 DEFAULT[-1] = 'compile'
 block[index][1] = 'definition'
 return index + 1  # skip over defined word name
def markdown_macro(block, index):
 'pack cyan word, compiled macro'
 block[index][1] = 'compilemacro'
 return index + 1
def markdown_numeric(block, index):
 'pack highlevel word that looks like number (e.g. "0")'
 block[index][1] = DEFAULT[-1] + 'word'  # from 'shortdecimal'
 return index + 1
def markdown_bareword(block, index):
 'pack "extension" word that occurs outside high-level code'
 while block[index][0] != '>':
  debug('bareword: %s' % block[index:], 2)
  block[index][1] = 'extension'
  index += 1
 block.pop(index)
 return index
def markdown_formatting(block, index):
 "blue words; can't just trigger on the word because cr is also green"
 block[index][1] = 'formatting'
 return index + 1
def markdown_feedback(block, index):
 'silver numbers, tag 0xd'
 block[index][1] = 'feedbackshort'
 return index + 1
def markdown_commented(block, index):
 'white numbers, tag 0xf'
 block[index][1] = 'commentedshort'
 return index + 1
def markdown_compile(block, index):
 'encountered a "]"'
 if len(DEFAULT) == 3:  # [...] inside a definition
  DEFAULT.pop(-1)
  assert DEFAULT[-1] == 'compile'
 else:  # not sure, could be a shadow block
  DEFAULT[:] = ['compile']
 debug('default now: %s, next word: %s' % (DEFAULT, padblock(block)[index]), 2)
 return index  # let main loop handle this word
def markdown_execute(block, index):
 'usually an "execute" block within a compiled word'
 if len(DEFAULT) == 2:  # inside a definition?
  DEFAULT.append('execute')  # begin [...] block
 else:
  DEFAULT[:] = ['execute']
 debug('default now: %s, next word: %s' % (DEFAULT, block[index]), 2)
 return index  # let main loop handle this word
def markdown_comment(block, index, done = False):
 'words may have been parsed as executeword, executeshort, etc.'
 while not done:
  if block[index][0].endswith(')'):
   block[index][0] = block[index][0][:-1]  # chop the right parenthesis
   done = True
  if block[index][0] in ['^', '^^']:
   markup = block.pop(index)[0]
   block[index][1] = 'text' + ACTIONS[markup]
   index -= 1  # don't increment index, we need to check for close paren
  elif not block[index][1].startswith('text'):
   block[index][1] = 'textnocaps'
  index += not done
 if block[index][0] == '':  # the case of a standalone ')'
  block.pop(index)
 else:
  index += 1
 return index
def markdown_var(block, index):
 block[index][1] = 'variable'
 if block[index + 1][1] != 'binary':  # initialize uninitialized variable
  block.insert(index + 1, ['[0x0]', 'binary'])
 return index + 2  # skip initialized value
def f2cf(infile, output = sys.stdout):
 if not type(output) == file:
  if os.path.exists(output):
   output = open(output, 'rb+')
  else:
   output = open(output, 'wb')
 for number, block in getblocks(infile):
  DEFAULT[:] = ['execute']  # reset default action
  checkblock(block)
  if output.mode == 'rb+':
   output.seek(number * 1024)
  output.write(packblock(block))
def packword(word, tag):
 'split word into word plus extensions, pack into longs'
 packed = ''
 #debug('packing "%s", tag="%s"' % (word, tag), 2)
 while word:
  number, word = pack(word)
  packed += packlong(number | TAGS.index(tag))
  tag = 'extension'
 return packed
def packlong(value):
 'pack a value into a 32-bit integer'
 packed = struct.pack('<L', value & 0xffffffff)
 debug('packed 0x%x into %s' % (value, repr(packed)), 2)
 return packed
def packblock(block):
 packedblock = ''
 for word, tagname in block:
  tag = TAGS.index(tagname) if tagname in TAGS else None
  debug('packing word "%s", tagname %s, tag %s' % (word, tagname, tag), 2)
  if tagname.endswith(('short', 'long')):
   value = forthint(word)
   tag |= (value[1] == 'hex') << 4
   if tagname.endswith('long'):
    packedblock += packlong(tag) + packlong(value[0])
   else:
    packedblock += packlong((value[0] << 5) | tag)
  elif tagname == 'binary':
   packedblock += packlong(eval(word)[0])
  else:
   packedblock += packword(word, tagname)
 packedblock += '\0' * (1024 - len(packedblock))  # pad it out with nulls
 return packedblock
def tag(word):
 'helper function for f2cf'
 tagged = None
 for name, inpattern in PATTERNS:
  if re.compile(inpattern).match(word):
   tagged = name
   break
 debug('tagged "%s" as "%s"' % (word, tagged), 2)
 if tagged and tagged.startswith('short') and abs(forthint(word)[0]) > MAXSHORT:
  tagged = tagged.replace('short', 'long', 1)
  debug('changed tag to "%s"' % tagged, 2)
 return [word, tagged]
def forthint(word, number = None):
 if word.startswith('$') and word != '$':
  number = int(word[1:], 16)
  if number & 0x80000000:
   number |= -0x80000000  # sign-extend it
 elif is_decimal(word):
  number = int(word, 10)  # Python assumes octal with leading 0
 if number is not None:
  return number, ['decimal', 'hex'][word.startswith('$')]
 else:
  return None, None
def getbinary(filename, data = ''):
 'concatenate block data from files or stdin'
 if type(filename) == str:
  input = open(filename, 'rb')
 else:
  input = filename
 data += input.read()
 input.close()
 if not data:
  data = sys.stdin.read()
 blocks = [data[n:n + 1024] for n in range(0, len(data), 1024)]
 return blocks
def getblocks(filename):
 blocks = []
 for line in readlines(filename):
  newblock = re.compile('^{block (\d+)}$').match(line)
  if newblock:
   debug('processing block %s' % newblock.group(1), 2)
   blocks.append([int(newblock.group(1)), []])
  else:
   blocks[-1][1].extend(map(tag, line.split()))
 return blocks
def getwords(block):
 words = list(struct.unpack('<%dl' % (len(block) / 4), block))
 return words
def readlines(filename):
 input = open(filename)
 data = input.readlines()
 input.close()
 return data
if __name__ == '__main__':
 name = os.path.splitext(os.path.split(sys.argv[0])[-1])[0]
 debug('trying: %s with args %s' % (name, sys.argv[1:]))
 debug(eval(name)(*sys.argv[1:]))
