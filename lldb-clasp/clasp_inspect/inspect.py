    
global_DataTypes = {}
global_Kinds = {}
global_Structs = {}
global_HeaderStruct = None

def SetupGlobals():
    global global_HeaderStruct
    global_HeaderStruct = global_Structs["gctools::Header_s"]
    if (global_HeaderStruct==None):
        raise "Could not find gctools::Header_s struct"

class StructType:
    def __init__(self,name,sizeof,fields):
        self._name = name
        self._sizeof = sizeof
        self._fields = fields
        
class DataType:
    def __init__(self,data_type,name,sizeof):
        self._data_type = data_type
        self._name = name
        self._sizeof = sizeof
        
class ClassKind:
    def __init__(self,stamp,name,size):
        self._stamp = stamp
        self._name = name
        self._size = size
        self._fields = {}
        self._variable_array0 = None
        self._variable_capacity = None
        self._variable_fields = {}

class TemplatedKind:
    def __init__(self,stamp,name,size):
        self._stamp = stamp
        self._name = name
        self._size = size
        self._fields = {}

class ContainerKind:
    def __init__(self,stamp,name,size):
        self._stamp = stamp
        self._name = name
        self._size = size
        self._variable_array0 = None
        self._variable_capacity = None
        self._variable_fields = {}

class BitunitContainerKind:
    def __init__(self,stamp,name,size,bits_per_bitunit):
        self._stamp = stamp
        self._name = name
        self._size = size
        self._bits_per_bitunit = bits_per_bitunit
        self._variable_array0 = None
        self._variable_capacity = None
        self._variable_fields = {}
        
class FixedField:
    def __init__(self,index,data_type,field_name,field_offset):
        self._index = index
        self._data_type = data_type
        self._field_name = field_name
        self._field_offset = field_offset

class VariableArray0:
    def __init__(self,name,offset):
        self._name = name
        self._offset = offset

class VariableCapacity:
    def __init__(self,element_size,end_offset,capacity_offset):
        self._element_size = element_size
        self._end_offset = end_offset
        self._capacity_offset = capacity_offset

class VariableField:
    def __init__(self,index,data_type,field_name,field_offset):
        self._index = index
        self._data_type = data_type
        self._field_name = field_name
        self._field_offset = field_offset

def verify_not_tagged(address):
    if (taggedp(address)):
        raise "The address %x is still tagged" % address

def object(address):
    if (generalp(address)):
        return GeneralPtr(address)
    if (consp(address)):
        return ConsPtr(address)
    if (fixnump(address)):
        return Fixnum(address)

class Fixnum:
    def __init__(self,address):
        if (fixnump(address)):
            self._Value = address>>2
            return
        raise("%x is not a fixnum" % address)
    def value(self):
        return self._Value
    def __repr__(self):
        return str(self._Value)

class TPtr:
    def consp(self):
        return False
    def generalp(self):
        return False
    def fixnump(self):
        return False
    
class ConsPtr(TPtr):
    def __init__(self,address):
        if (consp(address)):
            address = untag_cons(address)
        verify_not_tagged(address)
        self._address = address
    def consp(self):
        return True
    def car(self):
        global global_Process
        err = lldb.SBError()
        value = global_Process.ReadUnsignedFromMemory(self._address,8,err)
        return value
    def cdr(self):
        global global_Process
        err = lldb.SBError()
        value = global_Process.ReadUnsignedFromMemory(self._address+8,8,err)
        return value
    def __repr__(self):
        car = object(self.car())
        cdr = object(self.cdr())
        if (cdr.consp()):
            return ("(%s %s)" % ( car, cdr ))
        else:
            return ("(%s . %s )" % ( car, cdr))
        
class GeneralPtr(TPtr):
    def __init__(self,address):
        global global_Process
        if (generalp(address)):
            address = untag_general(address)
        verify_not_tagged(address)
        self._address = address
        self._header_ptr = address - global_HeaderStruct._sizeof
        err = lldb.SBError()
        header = global_Process.ReadUnsignedFromMemory(self._header_ptr,8,err)
        if (err.Success()):
            self._stamp = header>>4
            self._class = global_Kinds[self._stamp]
            self._className = self._class._name
    def generalp(self):
        return True
    
    def __repr__(self):
        if (self._className=="core::Symbol_O"):
            return "Symbol - get name"
        return "a %s" % self._className
   
def Init_test(msg):
    # print("Init_test -> %s" % msg)
    pass

def Init_struct(name,sizeof,fields):
    global global_Structs
    global_Structs[name] = StructType(name,sizeof,fields)
    
def Init_data_type(data_type,name,sizeof):
    global global_DataTypes
    global_DataTypes[data_type] = DataType(data_type,name,sizeof)

def Init_class_kind(stamp, name, size):
    global global_Kinds
    # print("Init__class_kind stamp = %d\n" % stamp)
    global_Kinds[stamp] = ClassKind(stamp,name,size)

def Init_templated_kind(stamp, name, size):
    # print("Init__templated_kind stamp = %d\n" % stamp)
    global_Kinds[stamp] = TemplatedKind(stamp,name,size)

def Init_container_kind(stamp, name, size):
    # print("Init__container_kind stamp = %d\n" % stamp)
    global_Kinds[stamp] = ContainerKind(stamp,name,size)


def Init_bitunit_container_kind(stamp, name, size, bits_per_bitunit):
    # print("Init__bitunit_container_kind stamp = %d\n" % stamp)
    global_Kinds[stamp] = BitunitContainerKind(stamp,name,size,bits_per_bitunit)
    
def Init__fixed_field(stamp,index,data_type,field_name,field_offset):
    # print("Init__fixed_field stamp = %d\n" % stamp)
    classKind = global_Kinds[stamp]
    field = FixedField(index,data_type,field_name,field_offset)
    classKind._fields[index] = field

def Init__variable_array0(stamp,name,offset):
    # print("Init__variable_array0 stamp = %d\n" % stamp)
    classKind = global_Kinds[stamp]
    classKind._variable_array0 = VariableArray0(name,offset)

def Init__variable_capacity(stamp,element_size,end_offset,capacity_offset):
    # print("Init__variable_capacity stamp = %d\n" % stamp)
    classKind = global_Kinds[stamp]
    classKind._variable_capacity = VariableCapacity(element_size,end_offset,capacity_offset)

def Init__variable_field(stamp,index,data_type,field_name,field_offset):
    # print("Init__variable_field stamp=%d\n" % stamp)
    classKind = global_Kinds[stamp]
    field = VariableField(index,data_type,field_name,field_offset)
    classKind._variable_fields[index] = field



def valid_tptr(val):
    return "0x%x" % val

def is_int(str,base):
    try:
        int(str,base)
        return True
    except ValueError:
        return False

def taggedp(tptr):
    return (tptr&0xf)!=0
                
def generalp(tptr):
    return (tptr&0xf==1)

def untag_general(tptr):
    return tptr-1

def consp(tptr):
    return (tptr&0xf==3)

def untag_cons(tptr):
    return tptr-3

def read_unsigned_at_offset(debugger,verbose,base,offset):
    tptr = debugger.read_memory(base+offset,8)
    if (verbose): debugger.print_("read_unsigned_at_offset offset: %x" % (base+offset))
    return tptr
    
def print_object_type(debugger,verbose,obj,type_=0):
    if (type_==0):
        print_tagged_ptr(debugger,verbose,obj,toplevel=False)
    debugger.print_("print_object_type Handle obj: %d  type: %d\n" % (obj, type_))

def print_variable_array0(debugger,verbose,indent,class_,obj,toplevel=False):
    base = untag_general(obj)
    data_offset = class_._variable_array0._offset
    element_size = class_._variable_capacity._element_size
    length_offset = class_._variable_capacity._end_offset
    length_ = read_unsigned_at_offset(debugger,verbose,base,length_offset)
    debugger.print_("%d slots" % length_)
    debugger.print_("variable_fields -> %s" % class_._variable_fields)
    for index in range(0,length_):
        element_offset = data_offset+element_size*index
        debugger.print_("element[%d]@0x%x" % (index, base+element_offset))
        for field_index in class_._variable_fields:
            field = class_._variable_fields[field_index]
            field_offset = element_offset + field._field_offset
            data = read_unsigned_at_offset(debugger,verbose,base,field_offset)
            debugger.print_("%s%s -> %s" % (indent,field._field_name, valid_tptr(data)));
    
def print_simple_base_string(debugger,verbose,indent,class_,obj):
    base = untag_general(obj)
    data_offset = class_._variable_array0._offset
    length_offset = class_._variable_capacity._end_offset
    length_ = read_unsigned_at_offset(debugger,verbose,base,length_offset)
    data = debugger.read_memory(base+data_offset,length_)
    debugger.print_("Data: %s "% str(data))

def print_ClosureWithSlots_O(debugger,verbose,indent,class_,obj):
    debugger.print_("class dict -> %s" % class_.__dict__)
    
def print_shallow_object_type(debugger,verbose,indent,obj,type_=0,toplevel=False):
    if (generalp(obj)):
        base = untag_general(obj)
        header_ptr = base - global_HeaderStruct._sizeof;
        header = debugger.read_memory(header_ptr,8)
        if (header):
            stamp = header>>4
            if (not toplevel and verbose): debugger.print_("%sheader@%x stamp = %d" % (indent,header_ptr,stamp))
            class_ = global_Kinds[stamp]
            name = class_._name
            if (name=="core::SimpleBaseString_O"):
                if (not toplevel and verbose): debugger.print_("class_ = %s" % class_.__dict__)
                print_simple_base_string(debugger,verbose,indent,class_,obj)
                return True
            if (not toplevel and verbose): debugger.print_("%sclass = %s" % (indent,name))
            return False
    return False
    
def print_tagged_ptr(debugger,verbose,tptr,toplevel=False):
    global global_HeaderStruct
    if (generalp(tptr)):
        base = untag_general(tptr)
        header_ptr = base - global_HeaderStruct._sizeof
        header = debugger.read_memory(header_ptr,8)
        if (header):
            stamp = header>>4
            if (verbose): debugger.print_("header@%x stamp = %d" % (header_ptr,stamp))
            class_ = global_Kinds[stamp]
            name = class_._name
            printed = print_shallow_object_type(debugger,verbose,0,tptr,toplevel)
            if (printed): return
            debugger.print_("a %s" % name )
            if (isinstance(class_,ClassKind)):
                for field in class_._fields.values():
                    val = read_unsigned_at_offset(debugger,verbose,base,field._field_offset)
                    debugger.print_("field %s: %s" % (field._field_name,valid_tptr(val)))
                    type_ = field._data_type
                    print_shallow_object_type(debugger,verbose,"  ",val,type_,toplevel=False)
            if (class_._variable_array0):
                print_variable_array0(debugger,verbose,"  ",class_,tptr,toplevel=False)
            return
        return
    if (consp(tptr)):
        cons = ConsPtr(untag_cons(tptr))
        debugger.print_("It's a cons")
    debugger.print_("print_tagged_ptr handle: %s\n" % tptr)



