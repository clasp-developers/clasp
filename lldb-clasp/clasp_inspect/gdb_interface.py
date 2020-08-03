import gdb

from clasp_inspect.interface import Interface
from clasp_inspect.object_layout import *
 
global_udb_interface = None

class UdbInterface(Interface):
    def __init__(self,debugger,internal_dict,prefix):
        global global_Structs
        print "In clasp_inspect for gdb_interface"
        filename = "/tmp/clasp-layout.py"
        with open(filename, "rb") as source_file:
            code = compile(source_file.read(), filename, "exec")
        exec(code)
        SetupGlobals(self)
       
    def print(self,msg):
        print msg

    def read_memory(self,address,len):
        i = gdb.inferiors()[0]
        m = i.read_memory(address,len)
        return m

def inspect(args):
    print "In inspect"
    # global global_lldb_interface
    # args = command.split(" ")
    # arg = args[0]
    # verbose = False
    # if (len(args)>1):
    #     verbose = True
    # ptr = None
    # if (arg[0:2]=='$r'):
    #     debugger.print("Handle register %s\n" % arg)
    #     return
    # if (is_int(arg,16)):
    #     tptr = int(arg,16)
    # elif (is_int(arg,10)):
    #     tptr = int(arg,10)
    # else:
    #     key = lldb.frame.FindVariable(arg)
    #     if (verbose): debugger.print("arg = %s" % key)
    #     theObject = key.GetChildMemberWithName("theObject")
    #     # theObject.GetValue() returns a string - why? dunno
    #     if (verbose): debugger.print("theObject.GetValue() = %s" % theObject.GetValue())
    #     tptr = int(theObject.GetValue(),16)
    # print_tagged_ptr(global_lldb_interface,verbose,tptr,toplevel=True)


def do_udb_init_module():
    global global_lldb_interface
    prefix = "%s.clasp_inspect.lldb_interface" % prefix
    print("In do_lldb_init_module")
    print("installing %s.inspect" % prefix)
    debugger.HandleCommand('command script add -f %s.inspect il' % prefix)
    global_lldb_interface = UdbInterface(debugger,internal_dict,prefix)
    print("Leaving do_lldb_init_module")
    
    
