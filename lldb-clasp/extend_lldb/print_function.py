
import lldb
import extend_lldb.loadperf

# ===================================================
# Utilities for locating/checking executable programs
# ===================================================

def is_exe(fpath):
    """Returns True if fpath is an executable."""
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

def which(program):
    """Returns the full path to a program; None otherwise."""
    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file
    return None

# ===================================================
# Disassembly for an SBFunction or an SBSymbol object
# ===================================================

def disassemble(target, function_or_symbol):
    """Disassemble the function or symbol given a target.

    It returns the disassembly content in a string object.
    """
    buf = StringIO.StringIO()
    insts = function_or_symbol.GetInstructions(target)
    for i in insts:
        print >> buf, i
    return buf.getvalue()


# ==========================================================
# Integer (byte size 1, 2, 4, and 8) to bytearray conversion
# ==========================================================

def int_to_bytearray(val, bytesize):
    """Utility function to convert an integer into a bytearray.

    It returns the bytearray in the little endian format.  It is easy to get the
    big endian format, just do ba.reverse() on the returned object.
    """
    import struct

    if bytesize == 1:
        return bytearray([val])

    # Little endian followed by a format character.
    template = "<%c"
    if bytesize == 2:
        fmt = template % 'h'
    elif bytesize == 4:
        fmt = template % 'i'
    elif bytesize == 4:
        fmt = template % 'q'
    else:
        return None

    packed = struct.pack(fmt, val)
    return bytearray(map(ord, packed))

def bytearray_to_int(bytes, bytesize):
    """Utility function to convert a bytearray into an integer.

    It interprets the bytearray in the little endian format. For a big endian
    bytearray, just do ba.reverse() on the object before passing it in.
    """
    import struct

    if bytesize == 1:
        return ba[0]

    # Little endian followed by a format character.
    template = "<%c"
    if bytesize == 2:
        fmt = template % 'h'
    elif bytesize == 4:
        fmt = template % 'i'
    elif bytesize == 4:
        fmt = template % 'q'
    else:
        return None

    unpacked = struct.unpack(fmt, str(bytes))
    return unpacked[0]


# ==============================================================
# Get the description of an lldb object or None if not available
# ==============================================================
def get_description(obj, option=None):
    """Calls lldb_obj.GetDescription() and returns a string, or None.

    For SBTarget and SBBreakpointLocation lldb objects, an extra option can be
    passed in to describe the detailed level of description desired:
        o lldb.eDescriptionLevelBrief
        o lldb.eDescriptionLevelFull
        o lldb.eDescriptionLevelVerbose
    """
    method = getattr(obj, 'GetDescription')
    if not method:
        return None
    if isinstance(obj, lldb.SBTarget) or isinstance(obj, lldb.SBBreakpointLocation):
        if option is None:
            option = lldb.eDescriptionLevelBrief

    stream = lldb.SBStream()
    if option is None:
        success = method(stream)
    else:
        success = method(stream, option)
    if not success:
        return None
    return stream.GetData()
        

# =================================================
# Convert some enum value to its string counterpart
# =================================================

def state_type_to_str(enum):
    """Returns the stateType string given an enum."""
    if enum == lldb.eStateInvalid:
        return "invalid"
    elif enum == lldb.eStateUnloaded:
        return "unloaded"
    elif enum == lldb.eStateConnected:
        return "connected"
    elif enum == lldb.eStateAttaching:
        return "attaching"
    elif enum == lldb.eStateLaunching:
        return "launching"
    elif enum == lldb.eStateStopped:
        return "stopped"
    elif enum == lldb.eStateRunning:
        return "running"
    elif enum == lldb.eStateStepping:
        return "stepping"
    elif enum == lldb.eStateCrashed:
        return "crashed"
    elif enum == lldb.eStateDetached:
        return "detached"
    elif enum == lldb.eStateExited:
        return "exited"
    elif enum == lldb.eStateSuspended:
        return "suspended"
    else:
        raise Exception("Unknown StateType enum")

def stop_reason_to_str(enum):
    """Returns the stopReason string given an enum."""
    if enum == lldb.eStopReasonInvalid:
        return "invalid"
    elif enum == lldb.eStopReasonNone:
        return "none"
    elif enum == lldb.eStopReasonTrace:
        return "trace"
    elif enum == lldb.eStopReasonBreakpoint:
        return "breakpoint"
    elif enum == lldb.eStopReasonWatchpoint:
        return "watchpoint"
    elif enum == lldb.eStopReasonSignal:
        return "signal"
    elif enum == lldb.eStopReasonException:
        return "exception"
    elif enum == lldb.eStopReasonPlanComplete:
        return "plancomplete"
    else:
        raise Exception("Unknown StopReason enum")

def value_type_to_str(enum):
    """Returns the valueType string given an enum."""
    if enum == lldb.eValueTypeInvalid:
        return "invalid"
    elif enum == lldb.eValueTypeVariableGlobal:
        return "global_variable"
    elif enum == lldb.eValueTypeVariableStatic:
        return "static_variable"
    elif enum == lldb.eValueTypeVariableArgument:
        return "argument_variable"
    elif enum == lldb.eValueTypeVariableLocal:
        return "local_variable"
    elif enum == lldb.eValueTypeRegister:
        return "register"
    elif enum == lldb.eValueTypeRegisterSet:
        return "register_set"
    elif enum == lldb.eValueTypeConstResult:
        return "constant_result"
    else:
        raise Exception("Unknown ValueType enum")


def get_module_names(thread):
    """
    Returns a sequence of module names from the stack frames of this thread.
    """
    def GetModuleName(i):
        return thread.GetFrameAtIndex(i).GetModule().GetFileSpec().GetFilename()

    return map(GetModuleName, range(thread.GetNumFrames()))

def get_function_names(thread):
    """
    Returns a sequence of function names from the stack frames of this thread.
    """
    def GetFuncName(i):
        return thread.GetFrameAtIndex(i).GetFunctionName()

    return map(GetFuncName, range(thread.GetNumFrames()))


def get_symbol_names(thread):
    """
    Returns a sequence of symbols for this thread.
    """
    def GetSymbol(i):
        return thread.GetFrameAtIndex(i).GetSymbol().GetName()

    return map(GetSymbol, range(thread.GetNumFrames()))


def get_pc_addresses(thread):
    """
    Returns a sequence of pc addresses for this thread.
    """
    def GetPCAddress(i):
        return thread.GetFrameAtIndex(i).GetPCAddress()

    return map(GetPCAddress, range(thread.GetNumFrames()))

def get_filenames(thread):
    """
    Returns a sequence of file names from the stack frames of this thread.
    """
    def GetFilename(i):
        return thread.GetFrameAtIndex(i).GetLineEntry().GetFileSpec().GetFilename()

    return map(GetFilename, range(thread.GetNumFrames()))


def get_line_numbers(thread):
    """
    Returns a sequence of line numbers from the stack frames of this thread.
    """
    def GetLineNumber(i):
        return thread.GetFrameAtIndex(i).GetLineEntry().GetLine()

    return map(GetLineNumber, range(thread.GetNumFrames()))


def get_module_names(thread):
    """
    Returns a sequence of module names from the stack frames of this thread.
    """
    def GetModuleName(i):
        return thread.GetFrameAtIndex(i).GetModule().GetFileSpec().GetFilename()

    return map(GetModuleName, range(thread.GetNumFrames()))


def get_stack_frames(thread):
    """
    Returns a sequence of stack frames for this thread.
    """
    def GetStackFrame(i):
        return thread.GetFrameAtIndex(i)

    return map(GetStackFrame, range(thread.GetNumFrames()))

def get_args_as_string(frame, showFuncName=True):
    """
    Returns the args of the input frame object as a string.
    """
    # arguments     => True
    # locals        => False
    # statics       => False
    # in_scope_only => True
    vars = frame.GetVariables(True, False, False, True) # type of SBValueList
    args = [] # list of strings
    for var in vars:
        args.append("(%s)%s=%s" % (var.GetTypeName(),
                                   var.GetName(),
                                   var.GetValue()))
    if frame.GetFunction():
        name = frame.GetFunction().GetName()
    elif frame.GetSymbol():
        name = frame.GetSymbol().GetName()
    else:
        name = ""
    if showFuncName:
        return "%s(%s)" % (name, ", ".join(args))
    else:
        return "(%s)" % (", ".join(args))
        



def lbt(debugger, command, result, internal_dict):
    """Prints a simple stack trace of this thread."""
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    thread = process.GetSelectedThread()

    print("process id= %d" % process.GetProcessID() , file=result)
    print("The number of frames = %d" % len(thread))

    target = thread.GetProcess().GetTarget()

    depth = thread.GetNumFrames()

    mods = list(get_module_names(thread))
    funcs = list(get_function_names(thread))
    symbols = list(get_symbol_names(thread))
    files = list(get_filenames(thread))
    lines = list(get_line_numbers(thread))
    addrs = list(get_pc_addresses(thread))

    if thread.GetStopReason() != lldb.eStopReasonInvalid:
        desc =  "stop reason=" + stop_reason_to_str(thread.GetStopReason())
    else:
        desc = ""
    print("Stack trace for thread id={0:#x} name={1} queue={2} ".format(
        thread.GetThreadID(), thread.GetName(), thread.GetQueueName()) + desc,
          file=result)
    for i in range(depth):
        frame = thread.GetFrameAtIndex(i)
        function = frame.GetFunction()
        load_addr = addrs[i].GetLoadAddress(target)
        if not function:
            file_addr = addrs[i].GetFileAddress()
            start_addr = frame.GetSymbol().GetStartAddress().GetFileAddress()
            symbol_offset = file_addr - start_addr
            sym = symbols[i]
            if (sym == None):
                sym_start = extend_lldb.loadperf.lookup_address(load_addr)
                sym = sym_start[0]
                symbol_offset = load_addr-sym_start[1]
            mmod = mods[i]
            if (mmod == None):
                mmod = ""
            print('  frame #{num}: {addr:#016x} {mod}`{symbol} + {offset}'.format(
                num=i, addr=load_addr, mod=mmod, symbol=sym, offset=symbol_offset)
                  ,file=result)
        else:
            print('  frame #{num}: {addr:#016x} {mod}`{func} at {file}:{line} {args}'.format(
                num=i, addr=load_addr, mod=mods[i],
                func='%s [inlined]' % funcs[i] if frame.IsInlined() else funcs[i],
                file=files[i], line=lines[i],
                args=get_args_as_string(frame, showFuncName=False) if not frame.IsInlined() else '()')
                  ,file=result)

def do_lldb_init_module(debugger, internal_dict,prefix):
    prefix = "%s.print_function" % prefix
    debugger.HandleCommand('command script add -f %s.lbt lbt' % prefix)
    print('The "lbt" python command has been installed - %s.lbt and is ready for use.' % prefix)



print("print_function loading")
