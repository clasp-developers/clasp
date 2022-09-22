import gdb
import struct
from io import StringIO


ByteOrder = 'little'

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


# ---- new functionality

def selected_frame():
    return gdb.selected_frame()

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

def clasp_python_info():
    vm_opcodes = global_variable_string("global_python_virtual_machine_codes")
    class_layouts = global_variable_string("global_python_class_layouts")
    return (vm_opcodes, class_layouts)


# ------------------------------------------------------------
#
# Frame filters
#
#


class Filter_inline:
    def __init__(self):
        self.name = "__invoke_impl"
        self.enabled = True
        self.priority = 100
        gdb.frame_filters[self.name] = self

def filter(self,frame_iterator):
    return ElidingInlineIterator(frame_iterator)

class ElidingInlineIterator:
    def __init__(self, ii):
        self.input_iterator = ii

    def __iter__(self):
        return self

    def next(self):
        frame = next(self.input_iterator)

        if frame.inferior_frame().type() != gdb.INLINE_FRAME:
            return frame

        try:
            eliding_frame = next(self.input_iterator)
        except StopIteration:
            return frame
        return ElidingFrameDecorator(eliding_frame, [frame])

print("Installing filter_inline")
filter_inline = Filter_inline()
