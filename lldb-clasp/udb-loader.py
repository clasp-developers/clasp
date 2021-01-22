import os

dir = os.path.dirname(os.path.expanduser(__file__))
print( "\n\n\nLoading clasp udb python extension from directory = %s" % dir)

sys.path.insert(0,dir)

import clasp_inspect.udb_interface


# im.udb_interface

class PrintLisp (gdb.Command):
  def __init__ (self):
    super (PrintLisp, self).__init__ ("pl", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    clasp_inspect.udb_interface.do_print(arg)


class InspectLisp (gdb.Command):
  def __init__ (self):
    super (InspectLisp, self).__init__ ("il", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    clasp_inspect.udb_interface.do_inspect(arg)
    
InspectLisp()
PrintLisp()
clasp_inspect.udb_interface.do_udb_init_module()
print("pl <address> - print lisp object in compact form")
print("il <address> - inspect lisp object - all fields")
print("python-interactive <expr> - (or pi) interactive Python session\n")
