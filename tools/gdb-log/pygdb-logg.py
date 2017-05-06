# gdb will 'recognize' this as python
#  upon 'source pygdb-logg.py'
# however, from gdb functions still have
#  to be called like:
#  (gdb) python print logExecCapture("bt")

import sys
import gdb
import os

def logExecCapture(instr):
  # /dev/shm - save file in RAM
  ltxname="/dev/shm/c.log"

  gdb.execute("set logging file "+ltxname) # lpfname
  gdb.execute("set logging redirect on")
  gdb.execute("set logging overwrite on")
  gdb.execute("set logging on")
  gdb.execute(instr)
  gdb.execute("set logging off")

  replyContents = open(ltxname, 'r').read() # read entire file
  return replyContents

# next until breakpoint
def nextUntilBreakpoint():
  isInBreakpoint = -1;
  # as long as we don't find "Breakpoint" in report:
  while isInBreakpoint == -1:
    REP=logExecCapture("n")
    isInBreakpoint = REP.find("Breakpoint")
    print "LOOP:: ", isInBreakpoint, "\n", REP
    
