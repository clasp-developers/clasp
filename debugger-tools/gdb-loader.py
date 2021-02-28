print "In gdb-loader.py"

class HelloWorld (gdb.Command):

  def __init__ (self):
    super (HelloWorld, self).__init__ ("hello-world", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    print "Hello, World! arg: %s" % arg

HelloWorld()
