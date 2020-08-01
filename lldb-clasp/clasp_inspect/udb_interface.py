import gdb

from clasp_inspect.interface import Interface
from clasp_inspect.inspect import *
 
global_udb_interface = None

class UdbInterface(Interface):
    def __init__(self):
        global global_Structs
        print "In clasp_inspect"
        filename = "/tmp/clasp-layout.py"
        with open(filename, "rb") as source_file:
            code = compile(source_file.read(), filename, "exec")
        exec(code)
        SetupGlobals()
       
    def print_(self,msg):
        print msg

    def read_memory(self,address,len):
        i = gdb.inferiors()[0]
        m = i.read_memory(address,len)
        return m

def inspect(args):
    print "In inspect args: %s" % args
    global global_udb_interface
    args = args.split(" ")
    arg = args[0]
    verbose = False
    if (len(args)>1):
        verbose = True
    ptr = None
    if (arg[0:2]=='$r'):
        debugger.print_("Handle register %s\n" % arg)
        return
    if (arg[0:2]=='0x' and is_int(arg,16)):
        tptr = int(arg[3:],16)
    elif (is_int(arg,10)):
        tptr = int(arg,10)
    else:
        debugger.print_("Handle arg: %s" % arg)
    #     key = lldb.frame.FindVariable(arg)
    #     if (verbose): debugger.print("arg = %s" % key)
    #     theObject = key.GetChildMemberWithName("theObject")
    #     # theObject.GetValue() returns a string - why? dunno
    #     if (verbose): debugger.print("theObject.GetValue() = %s" % theObject.GetValue())
    #     tptr = int(theObject.GetValue(),16)
    print_tagged_ptr(global_udb_interface,verbose,tptr,toplevel=True)


def do_udb_init_module():
    global global_udb_interface
    print "In do__init_module"
    global_udb_interface = UdbInterface()
    print "Leaving do_lldb_init_module with global_udb_interface = %s" % global_udb_interface
    
    
