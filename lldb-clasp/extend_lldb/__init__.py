
import extend_lldb.print_function
import extend_lldb.clasp_inspect
import extend_lldb.loadperf

def do_lldb_init_module(debugger, internal_dict,prefix):
    prefix = "%s.extend_lldb" % prefix
    extend_lldb.print_function.do_lldb_init_module(debugger,internal_dict,prefix)
    extend_lldb.clasp_inspect.do_lldb_init_module(debugger,internal_dict,prefix)
    extend_lldb.loadperf.do_lldb_init_module(debugger,internal_dict,prefix)
