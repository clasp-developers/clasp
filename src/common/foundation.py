

import sys
import os





def pauseForDebugger():
    print "Waiting for the debugger to connect (hit enter to continue):",
    sys.stdin.readline()

#
#
# Get Defaults
#
# Return a database of defaults
# Some day add support for environment variables
DEFAULT_CYCLOMER_DATABASE = 'defaultTrimerDatabase'
def getDefaults():
    defaults = {}
    defaults[DEFAULT_CYCLOMER_DATABASE ] = "pass2.pml"
    return defaults 

class CmdLineError(Exception):
   pass

def cmdLineParse(switches, flags, argv = None):
   """cmdLineParse(switches, flags, argv = None)

   Parse command line arguments.

   switches = string of option characters not taking arguments
   flags = string of option characters taking an argument
   argv = command line to parse (including program name), defaults
          to sys.argv

   Returns (args, options) where:

   args = list of non-option arguments
   options = dictionary mapping switch character to number of
             occurrences of the switch, and flag character to
             list of arguments specified with that flag

   Arguments following "--" are regarded as non-option arguments
   even if they start with a hyphen.
   """
   if not argv:
     import sys
     argv = sys.argv
   argv = argv[1:]
   opts = {}
   args = []
   for c in switches:
     opts[c] = 0
   for c in flags:
     if c in switches:
       raise ValueError("'%c' both switch and flag" % c)
#     opts[c] = []
   seen_dashdash = 0
   while argv:
     arg = argv.pop(0)
     if arg == "--":
       seen_dashdash = 1
     elif not seen_dashdash and arg.startswith("-"):
       for c in arg[1:]:
         if c in switches:
           opts[c] += 1
         elif c in flags:
           try:
             val = argv.pop(0)
           except IndexError:
             raise CmdLineError("Missing argument for option -%c" % c)
#           opts[c].append(val)
           opts[c] = val
         else:
           raise CmdLineError("Unknown option -%c" % c)
     else:
       args.append(arg)
   return args, opts



