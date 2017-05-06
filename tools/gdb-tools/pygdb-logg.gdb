# gdb script: pygdb-logg.gdb
# easier interface for pygdb-logg.py stuff
# from within gdb: (gdb) source -v pygdb-logg.gdb
# from cdmline: gdb -x pygdb-logg.gdb -se test.exe

# first, "include" the python file:
#source -v $CLASP_HOME/tools/gdb-tools/pygdb-logg.py

python
# gdb will 'recognize' this as python
#  upon 'source pygdb-logg.py'
# however, from gdb functions still have
#  to be called like:
#  (gdb) python print logExecCapture("bt")

import sys
import gdb
import os
import re

def logExecCapture(instr):
  # /dev/shm - save file in RAM
  ltxname="/tmp/c.log"

  gdb.execute("set logging file "+ltxname) # lpfname
  gdb.execute("set logging redirect on")
  gdb.execute("set logging overwrite on")
  gdb.execute("set logging on")
  gdb.execute(instr)
  gdb.execute("set logging off")
  replyContents = open(ltxname, 'r').read() # read entire file
  return replyContents

# stepi until breakpoint
def stepiUntilBreakpoint(log_file):
  isInBreakpoint = -1;
#  log_file = "/tmp/gdb.log"
  print("Writing to %s" % log_file)
  # as long as we don't find "Breakpoint" in report:
  gdb.execute("set disassemble-next-line on")
  gdb.execute("display/i $pc")
  logEverything = open(log_file, 'w')
  idx = 0
  while isInBreakpoint == -1:
    REP=logExecCapture("stepi")
    isInBreakpoint = REP.find("Breakpoint")
    print( "LOOP:: %s\n%s" % (isInBreakpoint, REP))
    logEverything.write("LOOP:: %d\n" % idx)
    idx = idx + 1
    logEverything.write("%s\n" % REP)
  logEverything.close()
    

def extractInstructions(log_file,inst_file):
  si = open(log_file,"r")
  so = open(inst_file,"w")
  print("%s  ->  %s\n" % (log_file,inst_file))
  lines = si.readlines()
  i = 0
  print("Number of lines = %d\n" % len(lines))
  while (i<len(lines)):
    if lines[i].find("x/i $pc")!=-1:
      line = lines[i+1][:-1]
      if (line[-2]==':'):
        line = line + lines[i+2][:-1]
      inst = line[5:]
      match = re.search(r'[^0-9a-f]',inst)
      so.write("%s\n" % inst[match.start():])
      i = i + 1
    i = i + 1
  si.close()
  so.close()
end

#
# define shorthand for stepiUntilBreakpoint():
define sub
  python stepiUntilBreakpoint($arg0,)
end

#
# Extract the instructions from the log in $arg0 and put them in $arg1
#
# eg:  extractinst "/tmp/run.log" "/tmp/instr.log"
define extractinst
  python extractInstructions($arg0,$arg1)
end

# set up breakpoints for test.exe:
b core::core__gdb

# go to main breakpoint
#run
