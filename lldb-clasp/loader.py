#
# Load this into lldb using
#
# command script import <path-to-loader.py>
#
#


import extend_lldb

print("Imported extend_lldb")

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('process handle -p true -n false -s false SIGXFSZ')
    debugger.HandleCommand('process handle -p true -n false -s false SIGWINCH')
    debugger.HandleCommand('process handle -p true -n false -s false SIGXCPU')
    debugger.HandleCommand('process handle -p true -n false -s false SIGSEGV')

    extend_lldb.do_lldb_init_module(debugger,internal_dict,"loader")

