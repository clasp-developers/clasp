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

def maybeReloadModules():
    global inspector_mod, debugger_mod
    if (inspector_mod == None):
      inspector_mod = importlib.import_module("clasp_inspect")
    else:
      importlib.reload(inspector_mod)
    if (debugger_mod == None):
      debugger_mod = importlib.import_module("backends.gdb")
    else:
      importlib.reload(debugger_mod)
  
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
    
class LispTest (gdb.Command):
  def __init__ (self):
    super (LispTest, self).__init__ ("ltest", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    inspector_mod.do_lisp_test(debugger_mod,arg)
    
LispInspect()
LispPrint()
LispHead()
LispTest()

print("lprint <address> - print lisp object in compact form")
print("linspect <address> - inspect lisp object - all fields")
print("lhead <address> - dump the clients header")
print("ltest <address> - test module reloading")
print("python-interactive <expr> - (or pi) interactive Python session\n")
