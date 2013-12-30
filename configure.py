import commands, sys, os

status, output = commands.getstatusoutput("raco planet show | grep aforth-optimizer.plt")

if len(output) == 0:
  print "F18A superoptimizer is not installed properly."
  sys.exit(0)

if output.find("mangpo") == -1:
  print "F18A superoptimizer (aforth-optimizer.plt) package has to be installed under \"mangpo\" and not your name."
  print "Run:"
  print "raco planet unlink <orginal_name> aforth-optimizer.plt 1 0"
  print "raco planet link mangpo aforth-optimizer.plt 1 0 <path to repo>"
  sys.exit(0)

status, output = commands.getstatusoutput("z3")

if not (output.find("not found") == -1):
  print "Please edit environmental variable PATH to point to z3."
  sys.exit(0)


status, abs_path = commands.getstatusoutput("pwd")
f = open("src/path.rkt", "w")
f.write("#lang racket\n")
f.write("(provide (all-defined-out))\n")
f.write("(define srcpath \"" + abs_path + "/src\")\n")
f.write("(define datapath \"" + abs_path + "/testdata\")\n")
print "src/path.rkt is created."
    

