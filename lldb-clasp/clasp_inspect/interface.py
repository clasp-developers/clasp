

class Interface:
    def __init__(self):
        pass

    def print_(self,msg):
        pass

    def read_memory(self,address,len):
        pass

class LldbInterface(Interface):
    def __init__(self,debugger):
        self._debugger = debugger
        self._process = debugger.GetSelectedTarget().GetProcess()

    def print_(self,msg):
        print(msg)
    def read_memory(self,address,len):
        err = lldb.SBError()
        tptr = self._process.ReadUnsignedFromMemory(address,len,err)
        return tptr
    
