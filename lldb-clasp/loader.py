#
# Load this into lldb using
#
# command script import <path-to-loader.py>
#
#


import extend_lldb

print("Imported extend_lldb")

def __lldb_init_module(debugger, internal_dict):
    extend_lldb.do_lldb_init_module(debugger,internal_dict,"loader")

