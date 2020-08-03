import lldb
from clasp_inspect.interface import Interface
from clasp_inspect.object_layout import *
 
global_lldb_interface = None


class LldbInterface(Interface):
    def __init__(self,debugger,internal_dict,prefix):
        global global_Structs
        self._debugger = debugger
        self._process = debugger.GetSelectedTarget().GetProcess()
        self.print("In clasp_inspect for lldb_interface")
        filename = "/tmp/clasp-layout.py"
        with open(filename, "rb") as source_file:
            code = compile(source_file.read(), filename, "exec")
        exec(code)
        SetupGlobals(self)
       
    def print_(self,msg):
        print(msg)

    def read_memory(self,address,len):
        err = lldb.SBError()
        tptr = self._process.ReadUnsignedFromMemory(address,len,err)
        return tptr
    


def inspect(debugger,command,result,internal_dict):
    global global_lldb_interface
    args = command.split(" ")
    arg = args[0]
    verbose = False
    if (len(args)>1):
        verbose = True
    ptr = None
    if (arg[0:2]=='$r'):
        debugger.print_("Handle register %s\n" % arg)
        return
    if (is_int(arg,16)):
        tptr = int(arg,16)
    elif (is_int(arg,10)):
        tptr = int(arg,10)
    else:
        key = lldb.frame.FindVariable(arg)
        if (verbose): debugger.print_("arg = %s" % key)
        theObject = key.GetChildMemberWithName("theObject")
        # theObject.GetValue() returns a string - why? dunno
        if (verbose): debugger.print_("theObject.GetValue() = %s" % theObject.GetValue())
        tptr = int(theObject.GetValue(),16)
    print_tagged_ptr(global_lldb_interface,verbose,tptr,toplevel=True)


def do_lldb_init_module(debugger,internal_dict,prefix):
    global global_lldb_interface
    prefix = "%s.clasp_inspect.lldb_interface" % prefix
    print("In do_lldb_init_module")
    print("installing %s.inspect" % prefix)
    debugger.HandleCommand('command script add -f %s.inspect il' % prefix)
    global_lldb_interface = LldbInterface(debugger,internal_dict,prefix)
    print("Leaving do_lldb_init_module")
    
    
