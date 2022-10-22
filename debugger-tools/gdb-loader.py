import os
import importlib


#
# The wrapper module
#
inspector_mod = None
debugger_mod = None

dir = os.path.dirname(os.path.expanduser(__file__))
print( "\n\n\nLoading clasp gdb python extension from directory = %s" % dir)

sys.path.insert(0,dir)

help_commands = []
def register_help(cmd,info):
    help_commands.append((cmd,info))

def show_help():
    for (cmd,info) in help_commands:
        print("%-30s- %s" % (cmd, info ))

def maybeReloadModules(verbose=False):
    global inspector_mod, debugger_mod
    if (inspector_mod == None):
      inspector_mod = importlib.import_module("clasp_inspect")
    else:
      importlib.reload(inspector_mod)
    if (debugger_mod == None):
      debugger_mod = importlib.import_module("backends.gdb")
    else:
      importlib.reload(debugger_mod)
    print( "maybeReloadModules verbose = %s" % verbose)
    inspector_mod.load_clasp_layout(debugger_mod,verbose)
    # Tell the debugger_mod about the inspector_mod
    debugger_mod.install_debugger_inspector(debugger_mod,inspector_mod)

class LispReload (gdb.Command):
  def __init__ (self):
    super (LispReload, self).__init__ ("lreload", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    print("Reloading debugger interface")
    maybeReloadModules()

class LispPrint (gdb.Command):
  def __init__ (self):
    super (LispPrint, self).__init__ ("lprint", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_print(debugger_mod,arg)

class LispHead (gdb.Command):
  def __init__ (self):
    super (LispHead, self).__init__ ("lhead", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_head(debugger_mod,arg)

class LispInspect (gdb.Command):
  def __init__ (self):
    super (LispInspect, self).__init__ ("linspect", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_inspect(debugger_mod,arg)

class LispFrame (gdb.Command):
  def __init__ (self):
    super (LispFrame, self).__init__ ("lframe", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_frame(debugger_mod,arg)

class LispDisassemble (gdb.Command):
  def __init__ (self):
    super (LispDisassemble, self).__init__ ("ldis", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_disassemble(debugger_mod,arg)

class LispTest (gdb.Command):
  def __init__ (self):
    super (LispTest, self).__init__ ("ltest", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_test(debugger_mod,arg)

class LispVm (gdb.Command):
  def __init__ (self):
    super (LispVm, self).__init__ ("lvm", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_vm(debugger_mod,arg)

class LispDynEnvStack (gdb.Command):
  def __init__ (self):
    super (LispDynEnvStack, self).__init__ ("lde", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_dump_dyn_env_stack(debugger_mod,arg)

class LispBacktrace (gdb.Command):
  def __init__ (self):
    super (LispBacktrace, self).__init__ ("lbt", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    gdb.execute("set print frame-arguments all")
    gdb.execute("bt "+arg)

class LispPrintVector (gdb.Command):
  def __init__ (self):
    super (LispPrintVector, self).__init__ ("lprve", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_print_vector(debugger_mod,arg)

class LispVectorPrint (gdb.Command):
  def __init__ (self):
    super (LispPrintVector, self).__init__ ("lvecprint", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_print_vector(debugger_mod,arg)

class LispSignalSIGUSR1 (gdb.Command):
  def __init__ (self):
    super (LispSignalSIGUSR1, self).__init__ ("ss", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    debugger_mod.signalSIGUSR1()

class LispDumpDebugInfo (gdb.Command):
  def __init__ (self):
    super (LispDumpDebugInfo, self).__init__("ldumpdebuginfo", gdb.COMMAND_USER)
    register_help("ldumpdebuginfo","Dump the debug info")

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules(True)

class LispHelp (gdb.Command):
  def __init__ (self):
    super (LispHelp, self).__init__("lhelp", gdb.COMMAND_USER)
    register_help("lhelp","This message")

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    show_help()
    
LispReload()
LispInspect()
LispPrint()
LispHead()
LispDisassemble()
LispTest()
LispFrame()
LispVm()
LispBacktrace()
LispDynEnvStack()
LispPrintVector()
LispSignalSIGUSR1()

register_help("lreload", "reload debugger extension")
register_help("lprint <address>","print lisp object in compact form")
register_help("linspect <address>","inspect lisp object - all fields")
register_help("lhead <address>","dump the clients header")
register_help("lframe","Dump the function name and args for a lisp frame trampoline")
register_help("ldis <bytecode-module-tptr>","Disassemble a bytecode-module")
register_help("ltest <address>","test module reloading")
register_help("lvm","Dump current vm status")
register_help("lbt [<num>]","Dump backtrace with arguments")
register_help("lde [<num>]","Dump dynamic environment stack")
register_help("lvecprint <addr> <num>","Print <num> values for a vector starting at <addr>")
register_help("ss","Shorthand for signal SIGUSR1")
LispDumpDebugInfo()
LispHelp()

show_help()

debugger_mod = importlib.import_module("backends.gdb")

