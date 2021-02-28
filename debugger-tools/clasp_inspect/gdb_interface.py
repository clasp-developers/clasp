import struct
import gdb

from clasp_inspect.interface import Interface
from clasp_inspect.object_layout import *
from clasp_inspect.translate import *


global_gdb_interface = None

class GdbInterface(Interface):
    def __init__(self):
        global global_structs
        print( "In clasp_inspect for UdbInterface")
        filename = "/tmp/clasp_layout.py"
        with open(filename, "rb") as source_file:
            code = compile(source_file.read(), filename, "exec")
        exec(code)
        SetupGlobals(self)
        self._ByteOrder = 'little'
        self._verbose = False
        
    def print_(self,msg):
        print(msg)

    def dbg_print(self,msg):
        if (self._verbose):
            print(msg)

    def read_memory(self,address,len):
        i = gdb.inferiors()[0]
        #print "About to read_memory at 0x%x len: %d" % (address, len)
        mem = i.read_memory(address,len)
        #print "About to create fmt"
        fmt = ('<' if self._ByteOrder == 'little' else '>') + {1: 'B', 2: 'H', 4: 'L', 8: 'Q'}[len]
        #print "About to struct.unpack with fmt: |%s|" % fmt
        val = struct.unpack(fmt,mem)
        #print "Read and unpacked mem at 0x%x len: %d with fmt: %s and got: 0x%x" % (address,len,fmt,val[0])
        return val[0]

    def evaluate(self,string):
        return int(gdb.parse_and_eval(string))

def arg_to_tptr(debugger,args):
    args = args.split(" ")
    arg = args[0]
    verbose = False
    if (len(args)>1):
        verbose = True
    ptr = None
    if (arg[0]=='$'):
        tptr = int(gdb.selected_frame().read_register(arg[1:]))
    elif (arg[0:2]=='0x' and is_int(arg,16)):
        tptr = int(arg[2:],16)
    elif (is_int(arg,10)):
        tptr = int(arg,10)
    else:
        tptr = int(debugger.evaluate(arg))
    print("arg_to_tptr returning %x" % tptr)
    return tptr
    
def do_print(args):
    #print "In inspect args: %s" % args
    global global_gdb_interface
    tptr = arg_to_tptr(global_gdb_interface,args)
    obj = translate_tagged_ptr(global_gdb_interface,tptr)
    print( obj.__repr__())
    return obj


def do_inspect(args):
    #print "In inspect args: %s" % args
    global global_gdb_interface
    tptr = arg_to_tptr(global_gdb_interface,args)
    obj = general_tagged_ptr(global_gdb_interface,tptr)
    print( "general_tagged_ptr returned: %s" % obj.__repr__())
    return obj

def do_gdb_init_module():
    global global_gdb_interface
    global_gdb_interface = GdbInterface()
    print( "Leaving do_lldb_init_module with global_gdb_interface = %s" % global_gdb_interface)
    
    
