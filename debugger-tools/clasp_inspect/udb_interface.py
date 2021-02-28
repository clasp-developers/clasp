import struct
import gdb

from clasp_inspect.interface import Interface
from clasp_inspect.object_layout import *
from clasp_inspect.translate import *
from clasp_inspect.gdb_interface import *

class UdbInterface(GdbInterface):
    pass


def do_udb_init_module():
    print("Initializing global_gdb_interface")
    clasp_inspect.gdb_interface.global_gdb_interface = UdbInterface()
    print( "Leaving do_lldb_init_module with global_gdb_interface = %s" % global_gdb_interface)
    
    
