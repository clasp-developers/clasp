
import extend_lldb.loadperf

global_perf = None

class FuncRange():
    def __init__(self,start,length,name):
        self._start=start
        self._length=length
        self._name = name

def func_start(f):
    return f._start

class PerfFile():
    def __init__(self):
        self._funcs = []
        
    def load(self,filename):
        print("Loading file: %s\n" % filename)
        with open(filename) as fin:
            for line in fin:
                parts = line.split()
                ff = FuncRange(int(parts[0],16),int(parts[1],16),parts[2])
                self._funcs.append(ff)
        self._funcs.sort(key=func_start)
        return self


    def binary_search(self, x):
#        print("binary_search started %x" % x)
        arr = self._funcs
        low = 0
        high = len(arr) - 1
        mid = 0
        while low <= high:
#            print("binary_search low/high -> %d/%d" % ( low, high))
#            print("     0x%x ... 0x%x ... 0x%x" % (arr[low]._start,x,arr[high]._start))
            mid = (high + low) // 2
            # Check if x is present at mid 
            if (arr[mid]._start < x) : 
                low = mid + 1
                # If x is greater, ignore left half 
            elif arr[mid]._start > x: 
                high = mid - 1
                # If x is smaller, ignore right half
            if ((arr[mid]._start <= x) and (x < (arr[mid]._start+arr[mid]._length))):
#                print("binary_search returning mid = %d" % mid)
                return mid
            if (low==high):
                if ((arr[low]._start <= x) and (x < (arr[low]._start+arr[low]._length))):
#                    print("binary_search returning low = %d" % low)
                    return low
                print("binary_search failed low==high returning None")
                print("binary_search low/high -> %d/%d" % ( low, high))
                print("     [0x%x - 0x%x] ... 0x%x ... 0x%x" % (arr[low]._start,(arr[low]._start+arr[low]._length),x,arr[high]._start))
                return None
        # If we reach here, then the element was not present
        print("binary_search Dropping through the bottom low/high -> %d/%d" % (low,high))
        return None

    def lookup_function(self,address):
        if (address<self._funcs[0]._start):
            return None
        if (address >= (self._funcs[-1]._start+self._funcs[-1]._length)):
            return None
        mid = self.binary_search(address)
        if (mid != None):
            return (self._funcs[mid]._name, self._funcs[mid]._start)
            
def load_perf_file(pid):
    global global_perf
    global_perf = PerfFile().load("/tmp/perf-%d.map" % pid)
    print("perf = %s" % global_perf)
    print("Loaded perf number of entries: %d" % len(global_perf._funcs))


def lookup_address(address):
    global global_perf
    func_start = global_perf.lookup_function(address)
#    print("Looked up address %x -> %s" % (address , func))
    return func_start
    
def do_lldb_init_module(debugger,internal_dict,prefix):
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    pid = process.GetProcessID()
    load_perf_file(pid)
    print("In loadperf pid = %d" % pid)
