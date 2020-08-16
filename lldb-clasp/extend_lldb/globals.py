import lldb

def dump_globals(debugger,command,result,internal_dict):
    target = debugger.GetSelectedTarget()
    if target:
        # Get the executable module
        module = target.module[target.executable.basename]
        if module:
            # Keep track of which variables we have already looked up
            global_names = list()
            # Iterate through all symbols in the symbol table and watch for any
            # DATA symbols
            for symbol in module.symbols:
                if symbol.type == lldb.eSymbolTypeData:
                    # The symbol is a DATA symbol, lets try and find all global variables
                    # that match this name and print them
                    global_name = symbol.name
                    # Make sure we don't lookup the same variable twice
                    if global_name not in global_names:
                        global_names.append(global_name)
                        # Find all global variables by name
                        global_variable_list = module.FindGlobalVariables(
                            target, global_name, lldb.UINT32_MAX)
                        if global_variable_list:
                            # Print results for anything that matched
                            for global_variable in global_variable_list:
                                # returns the global variable name as a string
                                print('name = %s' % global_variable.name)
                                # Returns the variable value as a string
                                print('value = %s' % global_variable.value)
                                print('type = %s' % global_variable.type)    # Returns an lldb.SBType object
                                # Returns an lldb.SBAddress (section offset
                                # address) for this global
                                print('addr = %s' % global_variable.addr)
                                # Returns the file virtual address for this
                                # global
                                print('file_addr = 0x%x' % global_variable.addr.file_addr)
                                # returns the global variable value as a string
                                print('location = %s' % global_variable.location)
                                # Returns the size in bytes of this global
                                # variable
                                print('size = %s' % global_variable.size)
                                print()

def find_global(debugger,command,result,internal_dict):
    print("command = %s" % command)
    seek_name = command
    target = debugger.GetSelectedTarget()
    if target:
        # Get the executable module
        module = target.module[target.executable.basename]
        if module:
            # Keep track of which variables we have already looked up
            global_names = list()
            # Iterate through all symbols in the symbol table and watch for any
            # DATA symbols
            for symbol in module.symbols:
                if symbol.type == lldb.eSymbolTypeData:
                    # The symbol is a DATA symbol, lets try and find all global variables
                    # that match this name and print them
                    global_name = symbol.name
                    # Make sure we don't lookup the same variable twice
                    if global_name not in global_names:
                        global_names.append(global_name)
                        # Find all global variables by name
                        global_variable_list = module.FindGlobalVariables(
                            target, global_name, lldb.UINT32_MAX)
                        if global_variable_list:
                            # Print results for anything that matched
                            for global_variable in global_variable_list:
                                if (global_variable.name == seek_name):
                                    # returns the global variable name as a string
                                    print('name = %s' % global_variable.name)
                                    # Returns the variable value as a string
                                    print('value = %s' % global_variable.value)
                                    print('type = %s' % global_variable.type)    # Returns an lldb.SBType object
                                    # Returns an lldb.SBAddress (section offset
                                    # address) for this global
                                    print('addr = %s' % global_variable.addr)
                                    # Returns the file virtual address for this
                                    # global
                                    print('file_addr = 0x%x' % global_variable.addr.file_addr)
                                    # returns the global variable value as a string
                                    print('location = %s' % global_variable.location)
                                    # Returns the size in bytes of this global
                                    # variable
                                    print('size = %s' % global_variable.size)
                                    print()
                                    return global_variable
    return None


def do_lldb_init_module(debugger, internal_dict,prefix):
    prefix = "%s.globals" % prefix
    debugger.HandleCommand('command script add -f %s.count_globals count_globals' % prefix)
    debugger.HandleCommand('command script add -f %s.find_global find_global' % prefix)

