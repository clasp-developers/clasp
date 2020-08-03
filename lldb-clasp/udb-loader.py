import os

dir = os.path.dirname(os.path.expanduser(__file__))
print "dir = %s" % dir

sys.path.insert(0,dir)

import clasp_inspect.udb_interface


# im.udb_interface

class InspectLisp (gdb.Command):
  def __init__ (self):
    super (InspectLisp, self).__init__ ("il", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    clasp_inspect.udb_interface.inspect(arg)

InspectLisp()
clasp_inspect.udb_interface.do_udb_init_module()
