import gdb
from gdb.FrameDecorator import FrameDecorator
import itertools

import struct
from io import StringIO

def dbg_print(msg):
    if (verbose):
        print(msg)

verbose = False   # turns on dbg_print(xxxx)
debugger_mod = None
inspector_mod = None

ByteOrder = 'little'

def install_debugger_inspector(debugger,inspector):
    global inspector_mod
    global debugger_mod
    dbg_print("udb.py:install_inspector %s" % inspector)
    debugger_mod = debugger
    inspector_mod = inspector

def read_memory(address,len=8):
    i = gdb.inferiors()[0]
    #print "About to read_memory at 0x%x len: %d" % (address, len)
    mem = i.read_memory(address,len)
    #print "About to create fmt"
    fmt = ('<' if ByteOrder == 'little' else '>') + {1: 'B', 2: 'H', 4: 'L', 8: 'Q'}[len]
    #print "About to struct.unpack with fmt: |%s|" % fmt
    val = struct.unpack(fmt,mem)
    #print "Read and unpacked mem at 0x%x len: %d with fmt: %s and got: 0x%x" % (address,len,fmt,val[0])
    return val[0]

def test_debugger(arg):
    print("In udb test_debugger arg: %s" % arg)

def dump_memory(address):
    cmd0 = "x/8xg 0x%x" % (address-64)
    print("======dump before header")
    gdb.execute(cmd0)
    cmd = "x/16xg 0x%x" % address
    print("------Dump from header")
    gdb.execute(cmd)

def evaluate(string):
    return int(gdb.parse_and_eval(string))

def convenience_variable(name):
    return gdb.convenience_variable(name)

def set_convenience_variable(name,val):
    gdb.set_convenience_variable(name,val)

def signalSIGUSR1():
    gdb.execute("signal SIGUSR1")

# ---- new functionality

def newest_frame():
    return gdb.newest_frame()

def selected_frame():
    return gdb.selected_frame()

def older_frame(frame):
    return frame.older()

def frame_base_pointer(frame):
    # will this be machine independent?
    return frame.read_register("rbp")

def frame_stack_pointer(frame):
    # will this be machine independent?
    return frame.read_register("rsp")

def block(frame):
    return frame.block()

def lisp_selected_frame():
    frame = gdb.selected_frame()
    block = frame.block()
    sa = []
    for symbol in block:
        if (symbol.name == "sa"):
            sa = symbol.value(frame)
    result = (sa[0],sa[1],sa[2])
    return result

def global_variable_string(name):
    object = gdb.parse_and_eval(name)
    return object.string()

def global_variable_string(name):
    object = gdb.parse_and_eval(name)
    return "%s" % object.string()

def global_variable_int(name):
    object = gdb.parse_and_eval(name)
    return int("%s" % object.string())

def global_variable_null(name):
    object = gdb.parse_and_eval(name)
    object


def clasp_python_info():
    vm_opcodes = global_variable_string("global_python_virtual_machine_codes")
    class_layouts = global_variable_string("global_python_class_layouts")
    return (vm_opcodes, class_layouts)


# ------------------------------------------------------------
#
# Frame filters
#
#

class InlineFilter():

    def __init__(self):
        self.name = "InlinedFrameFilter"
        self.priority = 100
        self.enabled = True
        gdb.frame_filters[self.name] = self

    def filter(self, frame_iter):
        frame_iter = map(InlinedFrameDecorator, frame_iter)
        return frame_iter

class InlinedFrameDecorator(FrameDecorator):
    
    def __init__(self, fobj):
        self.fobj = fobj
        super(InlinedFrameDecorator, self).__init__(fobj)

    def function(self):
        global inspector_mod
        global debugger_mod
        dbg_print( "InlinedFrameDecorator:function inspector_mod %s" % inspector_mod)
        frame = self.fobj.inferior_frame()
        name = str(frame.name())
        dbg_print("InlinedFrameDecorator::function name = %s" % name)
        if frame.type() == gdb.INLINE_FRAME:
            name = name + " [inlined]"
        if (name == "bytecode_trampoline_with_stackmap"):
            stackmap_var = frame.read_var("trampoline_save_args")
            arg = "%s"%stackmap_var[0]
            dbg_print("do_lisp_print arg= %s" % arg)
            lisp_name = inspector_mod.function_name(debugger_mod,arg)
            dbg_print("lisp_name = %s" % lisp_name )
            name = "BYTECODE_FRAME(%s)"%lisp_name
        return name

    def frame_args(self):
        frame = self.fobj.inferior_frame()
        name = str(frame.name())
        dbg_print("InlinedFrameDecorator::function name = %s" % name)
        if (name == "bytecode_trampoline_with_stackmap"):
            stackmap_var = frame.read_var("trampoline_save_args")
            nargs = stackmap_var[1]
            vargs = stackmap_var[2]
            args = []
            for iarg in range(0,nargs):
                tptr = read_memory(vargs+(8*iarg),len=8)
                try:
                    varg = inspector_mod.do_lisp_print_value(debugger_mod,"%s"%tptr)
                except:
                    varg = "BADARG(0x%x)"%tptr
                args.append( LispArg("a%d" % iarg, "%s"%varg))
            return args

class LispArg:
    def __init__(self,name,value):
        self._name = name
        self._value = value
    def symbol(self):
        return self._name
    def value(self):
        return self._value

    

filter_inline = InlineFilter()
