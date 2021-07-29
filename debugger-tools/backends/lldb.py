import lldb


debugger = None
process = None

ByteOrder = 'little'

def read_memory(address,len=8):
    global debugger,process
    error = lldb.SBError()
    tptr = process.ReadUnsignedFromMemory(address,len,error)
    return tptr

def test_debugger(arg):
    print("In udb test_debugger arg: %s" % arg)

def dump_memory(address,bytes=False):
    global debugger
    if (bytes):
        cmd0 = "x -c 64 0x%x" % (address-64)
    else:
        cmd0 = "x/8xg 0x%x" % (address-64)
    print("======dump before header: %s" % cmd0)
    debugger.HandleCommand(cmd0)
    if (bytes):
        cmd = "x -c 128 0x%x" % address
    else:
        cmd = "x/16xg 0x%x" % address
    print("------Dump from header: %s" % cmd )
    debugger.HandleCommand(cmd)
    
def evaluate(string):
    global debugger,process
    thread = process.GetSelectedThread()
    frame = thread.GetSelectedFrame()
    result = frame.EvaluateExpression(string).unsigned
    return result

def convenience_variable(name):
    return evaluate("$%s" % name)

def set_convenience_variable(name,val):
    evaluate("uintptr_t $%s = %s" % (name, val))

def set_debugger(dbg):
    global debugger, process
    debugger = dbg
    process = dbg.GetSelectedTarget().GetProcess()
    
    
