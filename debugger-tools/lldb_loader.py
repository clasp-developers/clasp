import lldb


import os
import importlib


#
# The wrapper module
#
inspector_mod = None
debugger_mod = None

dir = os.path.dirname(os.path.expanduser(__file__))
print( "\n\n\nLoading clasp udb python extension from directory = %s" % dir)


def maybeReloadModules():
    global inspector_mod, debugger_mod
    if (inspector_mod == None):
      inspector_mod = importlib.import_module("clasp_inspect")
    else:
      importlib.reload(inspector_mod)
    if (debugger_mod == None):
      debugger_mod = importlib.import_module("backends.lldb")
    else:
      importlib.reload(debugger_mod)
    
def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('command script add -f lldb_loader.lisp_print lpr')
    debugger.HandleCommand('command script add -f lldb_loader.lisp_inspect lin')
    debugger.HandleCommand('command script add -f lldb_loader.lisp_test ltest')
    debugger.HandleCommand('command script add -f lldb_loader.lisp_head lhead')
      
def lisp_print(debugger, arg, result, internal_dict):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    debugger_mod.set_debugger(debugger)
    inspector_mod.do_lisp_print(debugger_mod,arg)

def lisp_inspect(debugger, arg, result, internal_dict):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    debugger_mod.set_debugger(debugger)
    inspector_mod.do_lisp_inspect(debugger_mod,arg)

def lisp_head(debugger, arg, result, internal_dict):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    debugger_mod.set_debugger(debugger)
    inspector_mod.do_lisp_head(debugger_mod,arg)
    
def lisp_test(debugger, arg, result, internal_dict):
    global inspector_mod, debugger_mod
    maybeReloadModules()
    debugger_mod.set_debugger(debugger)
    inspector_mod.do_lisp_test(debugger_mod,arg)
    
