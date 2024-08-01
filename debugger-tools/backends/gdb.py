import gdb
from gdb.FrameDecorator import FrameDecorator

import struct
from io import StringIO

def dbg_print(msg):
    if (verbose):
        print(msg)

verbose = False   # turns on dbg_print(xxxx)
debugger_mod = None
inspector_mod = None
VMFilter = None

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

def evaluate_int(string):
    return int(gdb.parse_and_eval(string))

def convenience_variable(name):
    if name.isdigit(): # if it's all digits, interpret it as a history ref
        return gdb.history(int(name))
    else:
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

# This filter collapses bytecode_vm and intermediates into an
# overall bytecode_call.
class VMFrameFilter():

    def __init__(self):
        self.name = "VMFrameFilter"
        self.priority = 100
        self.enabled = True
        gdb.frame_filters[self.name] = self

    def filter(self, frame_iter):
        return ElidingVMFrameIterator(frame_iter)

def bytecode_vm_frame_p(frame):
    fname = frame.name()
    if (fname == None):
        return False
    else:
        # Kinda fragile but I'm not sure what a better way is.
        return "bytecode_vm" in fname

def bytecode_call_frame_p(frame):
    fname = frame.name()
    if (fname == None):
        return False
    else:
        return "bytecode_call" in fname

class ElidingVMFrameIterator():

    def __init__(self, ii):
        self.input_iterator = ii

    def __iter__(self):
        return self

    def __next__(self):
        frame = next(self.input_iterator)

        if (not bytecode_vm_frame_p(frame.inferior_frame())):
            return frame

        # We have a call to bytecode_vm.
        # Eat frames until we hit bytecode_call.
        # This should be ok since bytecode_vm is
        # not called from elsewhere.
        elision = [frame] # frames we're going to elide.
        while True:
            try:
                frame = next(self.input_iterator)
            except StopIteration: # no bytecode_call somehow
                return frame
            if bytecode_call_frame_p(frame.inferior_frame()):
                # Done.
                return VMFrameDecorator(frame, elision)
            else:
                # OK, so this frame sucks and we're skipping it.
                # Not sure this will add in the right order
                # but ohhhhh well
                elision.append(frame)

class ElidingFrameDecorator(FrameDecorator):

    def __init__(self, frame, elided_frames):
        super(ElidingFrameDecorator, self).__init__(frame)
        self.frame = frame
        self.elided_frames = elided_frames

    def elided(self):
        return self.elided_frames

# Tries to get a Lisp function name.
class VMFrameDecorator(ElidingFrameDecorator):

    def function(self):
        # Some of the elided frames are bytecode_vm calls.
        # If they are, try to grab the closure argument.
        if inspector_mod != None:
            for sframe in self.elided_frames:
                inf = sframe.inferior_frame()
                if bytecode_vm_frame_p(inf):
                    closure = inf.read_var("closure")
                    if closure.is_optimized_out:
                        break
                    # We have something. Tag it,
                    tclosure = int(closure) | 1
                    # and then pass to the inspector
                    name = inspector_mod.function_name(debugger_mod, tclosure)
                    if (name == None):
                        break
                    return str(name) + " [bytecode]"
        # Give up
        return super(VMFrameDecorator, self).function()

    def frame_args(self):
        # Pull from lcc_nargs and lcc_args, which are usually
        # not optimized out.
        inf = self.frame.inferior_frame()
        nargs = inf.read_var("lcc_nargs")
        args = inf.read_var("lcc_args")
        if nargs.is_optimized_out or args.is_optimized_out:
            return None
        return [SymValWrapper("arg" + str(n), args[n]) for n in range(int(nargs))]

class SymValWrapper():
    def __init__(self, symbol, value):
        self.sym = symbol
        self.val = value
    def value(self):
        return self.val
    def symbol(self):
        return self.sym

# This filter elides inline frames, since we have a lot of em.
class InlineFrameFilter():

    def __init__(self):
        self.name = "InlineFrameFilter"
        self.priority = 99
        self.enabled = True
        gdb.frame_filters[self.name] = self

    def filter(self, frame_iter):
        return InlineFrameIterator(frame_iter)

class InlineFrameIterator():

    def __init__(self, ii):
        self.input_iterator = ii

    def __iter__(self):
        return self

    def __next__(self):
        frame = next(self.input_iterator)

        if (not frame.inferior_frame().type() == gdb.INLINE_FRAME):
            return frame

        # Eat frames until we hit something not inlined.
        elision = [frame] # frames we're going to elide.
        while True:
            try:
                frame = next(self.input_iterator)
            except StopIteration: # no bytecode_call somehow
                return frame
            if frame.inferior_frame().type() == gdb.INLINE_FRAME:
                elision.append(frame)
            else:
                # Done.
                return ElidingFrameDecorator(frame, elision)

vm_filter = VMFrameFilter()
inline_filter = InlineFrameFilter()
