
from io import StringIO

verbose = False
layout = None
debugger = None

def dbg_print(msg):
    if (verbose):
        print(msg)

def load_clasp_layout():
    print( "Loading /tmp/clasp_layout.py 2")
    filename = "/tmp/clasp_layout.py"
    with open(filename, "rb") as source_file:
        code = compile(source_file.read(), filename, "exec")
    exec(code)
    SetupGlobals()


global_ints = {}    
global_dataTypes = {}
global_kinds = {}
global_structs = {}
global_StampWtagMtagStruct = None
debugger = None

info = {}

def SetupGlobals():
    global info
    info["ints"] = global_ints
    info["dataTypes"] = global_dataTypes
    info["kinds"] = global_kinds
    info["structs"] = global_structs
    info["headerStruct"] = global_structs["gctools::Header_s"]
    info["stampWtagMtagStruct"] = global_structs["gctools::Header_s::StampWtagMtag"]


class FieldType:
    def __init__(self,values):
        self._name = values[0]
        self._type = values[1]
        self._offset = values[2]
        self._sizeof = values[3]

class StructType:
    def __init__(self,name,sizeof,fields):
        self._name = name
        self._sizeof = sizeof
        self._fields = {}
        for vals in fields:
            fieldType = FieldType(vals)
            self._fields[fieldType._name] = fieldType
        
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
        self._index = index # unused?
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

def Init_test(msg):
    # print("Init_test -> %s" % msg)
    pass

def Init_global_ints(name,value):
    global_ints[name] = value
    
def Init_struct(name,sizeof,fields):
    global global_structs
    global_structs[name] = StructType(name,sizeof,fields)
    
def Init_data_type(data_type,name,sizeof):
    global global_dataTypes
    global_dataTypes[data_type] = DataType(data_type,name,sizeof)

def Init_class_kind(stamp, name, size):
    global global_kinds
    # print("Init__class_kind stamp = %d\n" % stamp)
    global_kinds[stamp] = ClassKind(stamp,name,size)

def Init_templated_kind(stamp, name, size):
    # print("Init__templated_kind stamp = %d\n" % stamp)
    global_kinds[stamp] = TemplatedKind(stamp,name,size)

def Init_container_kind(stamp, name, size):
    # print("Init__container_kind stamp = %d\n" % stamp)
    global_kinds[stamp] = ContainerKind(stamp,name,size)

def Init_bitunit_container_kind(stamp, name, size, bits_per_bitunit):
    # print("Init__bitunit_container_kind stamp = %d\n" % stamp)
    global_kinds[stamp] = BitunitContainerKind(stamp,name,size,bits_per_bitunit)
    
def Init__fixed_field(stamp,index,data_type,field_name,field_offset):
    # print("Init__fixed_field stamp = %d\n" % stamp)
    classKind = global_kinds[stamp]
    field = FixedField(index,data_type,field_name,field_offset)
    classKind._fields[index] = field

def Init__variable_array0(stamp,name,offset):
    # print("Init__variable_array0 stamp = %d\n" % stamp)
    classKind = global_kinds[stamp]
    classKind._variable_array0 = VariableArray0(name,offset)

def Init__variable_capacity(stamp,element_size,end_offset,capacity_offset):
    # print("Init__variable_capacity stamp = %d\n" % stamp)
    classKind = global_kinds[stamp]
    classKind._variable_capacity = VariableCapacity(element_size,end_offset,capacity_offset)

def Init__variable_field(stamp,index,data_type,field_name,field_offset):
    # print("Init__variable_field stamp=%d\n" % stamp)
    classKind = global_kinds[stamp]
    field = VariableField(index,data_type,field_name,field_offset)
    classKind._variable_fields[index] = field




############################################################
############################################################


def verify_not_tagged(address):
    if (taggedp(address)):
        raise "The address %x is still tagged" % address

def valid_tptr(val):
    return "0x%x" % val

def is_int(str,base):
    try:
        int(str,base)
        return True
    except ValueError:
        return False

def flags(tptr):
    return (tptr&info["ints"]["IMMEDIATE_MASK"])

def taggedp(tptr):
    return (flags(tptr)!=0)
                
def generalp(tptr):
    return (flags(tptr)==info["ints"]["GENERAL_TAG"])

def untag_general(tptr):
    return tptr-info["ints"]["GENERAL_TAG"]

def fixnump(tptr):
    return (tptr&info["ints"]["FIXNUM_MASK"]==0)

def consp(tptr):
    return (flags(tptr)==info["ints"]["CONS_TAG"])

def untag_cons(tptr):
    return tptr-info["ints"]["CONS_TAG"]

def untag(tptr):
    return tptr - flags(tptr)

def vaslistp(tptr):
    return (tptr&info["ints"]["IMMEDIATE_MASK"]==info["ints"]["VASLIST0_TAG"])

def untag_vaslist(tptr):
    return (tptr - info["ints"]["VASLIST1_TAG"])

def read_unsigned_at_offset(debugger,verbose,base,offset):
    tptr = debugger.read_memory(base+offset,8)
    if (verbose): dbg_print("read_unsigned_at_offset offset: %x" % (base+offset))
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
    dbg_print("%d slots" % length_)
    dbg_print("variable_fields -> %s" % class_._variable_fields)
    for index in range(0,length_):
        element_offset = data_offset+element_size*index
        dbg_print("element[%d]@0x%x" % (index, base+element_offset))
        for field_index in class_._variable_fields:
            field = class_._variable_fields[field_index]
            field_offset = element_offset + field._field_offset
            data = read_unsigned_at_offset(debugger,verbose,base,field_offset)
            dbg_print("%s%s -> %s" % (indent,field._field_name, valid_tptr(data)));
    
def print_simple_base_string(debugger,verbose,indent,class_,obj):
    base = untag_general(obj)
    data_offset = class_._variable_array0._offset
    length_offset = class_._variable_capacity._end_offset
    length_ = read_unsigned_at_offset(debugger,verbose,base,length_offset)
    data = debugger.read_memory(base+data_offset,length_)
    dbg_print("Data: %s "% str(data))

def print_ClosureWithSlots_O(debugger,verbose,indent,class_,obj):
    dbg_print("class dict -> %s" % class_.__dict__)
    
def print_shallow_object_type(debugger,verbose,indent,obj,type_=0,toplevel=False):
    if (generalp(obj)):
        (stamp,mtag) = read_stamp_mtag(debugger,obj)
        if (stamp):
            class_ = debugger._kinds[stamp]
            name = class_._name
            if (name=="core::SimpleBaseString_O"):
                if (not toplevel and verbose): dbg_print("class_ = %s" % class_.__dict__)
                print_simple_base_string(debugger,verbose,indent,class_,obj)
                return True
            if (not toplevel and verbose): dbg_print("%sclass = %s" % (indent,name))
            return False
    return False

def read_string(debugger,address,char_size,end):
    str = ""
    dbg_print( "read_string address 0x%x  char_size=%d  end=%d" % ( address, char_size, end ))
    for index in range(end):
        char_val = debugger.read_memory(address+(index*char_size),char_size)
        if (char_val>127):
            char_char = "\\%d" % char_val
        else:
            char_char = chr(char_val)
        str += char_char
    return str
        

class Fixnum:
    def __init__(self,debugger,address):
        if (fixnump(address)):
            self._Value = info["ints"]["FIXNUM_SHIFT"]
            return
        raise("%x is not a fixnum" % address)
    def value(self):
        return self._Value
    def __repr__(self):
        return str(self._Value)

class Vaslist:
    def __init__(self,debugger,address):
        if (vaslistp(address)):
            self._Value = address
            return
        raise("%x is not a vaslist" % address)
    def value(self):
        return self._Value
    def __repr__(self):
        return str(address)
    
class T_O:
    def consp(self):
        return False
    def generalp(self):
        return False
    def fixnump(self):
        return False
    
class Cons_O(T_O):
    def __init__(self,debugger,address):
        if (consp(address)):
            address = untag_cons(address)
        verify_not_tagged(address)
        self._debugger = debugger
        self._address = address
    def consp(self):
        return True
    def car(self):
        return self._debugger.read_memory(self._address,8)
    def cdr(self):
        return self._debugger.read_memory(self._address+8,8) 
    def __repr__(self):
        car = translate_tagged_ptr(self._debugger,self.car())
        cdr = translate_tagged_ptr(self._debugger,self.cdr())
        if (cdr.consp()):
            return ("(%s %s)" % ( car, cdr ))
        else:
            return ("(%s . %s )" % ( car, cdr))
        
class General_O(T_O):
    def __init__(self,debugger,tclient):
        if (generalp(tclient)):
            address = untag_general(tclient)
        else:
            address = tclient
        verify_not_tagged(address)
        self._debugger = debugger
        self._address = address
        dbg_print("In General_O")
        (stamp,mtag) = read_stamp_mtag(debugger,address)
        self._stamp = stamp
        self._class = info["kinds"][stamp]
        self._className = self._class._name
        self._classSize = self._class._size
        self._fieldAtOffset = {}
        for idx in range(len(self._class._fields)):
            cur = self._class._fields[idx]
            self._fieldAtOffset[cur._field_offset] = cur
        
    def generalp(self):
        return True
    def field(self,name):
        field_ = None
        for idx in range(len(self._class._fields)):
            cur = self._class._fields[idx]
            if (cur._field_name==name):
                field_ = cur
                break
        if (field_):
            if (field_._data_type == 0):
                offset = field_._field_offset
                tptr = self._debugger.read_memory(self._address+offset,8)
                return translate_tagged_ptr(self._debugger,tptr)
            if (field_._data_type == 1):
                offset = field_._field_offset
                tptr = self._debugger.read_memory(self._address+offset,8)
                return translate_tagged_ptr(self._debugger,tptr)
            if (field_._data_type == 2):
                offset = field_._field_offset
                tptr = self._debugger.read_memory(self._address+offset,8)
                return translate_tagged_ptr(self._debugger,tptr)
            raise Exception("Handle _data_type %d for %s" % (field_._data_type, cur._field_name))
        raise Exception("There is no field named %s in %s" % ( name, self._className))
    def nilp(self):
        result = (self._className == "core::Null_O")
        return result
    
    def __repr__(self):
        return "a %s" % self._className
   

class Array_O(General_O):
    def __init__(self,debugger,address):
        General_O.__init__(self,debugger,address)

class SimpleCharacterString_O(Array_O):
    def __init__(self,debugger,tptr):
        Array_O.__init__(self,debugger,tptr)
        end_offset = self._class._variable_capacity._end_offset
        char_size = self._class._variable_capacity._element_size
        data_offset = self._class._variable_array0._offset
        end = debugger.read_memory(self._address+end_offset,8)
        dbg_print("SimpleCharacterString_O end_offset->%d char_size->%d data_offset->%d end->%d" % (end_offset,char_size,data_offset,end))
        self._String = read_string(debugger,self._address+data_offset,char_size,end)
    def str(self):
        return self._String
    def __repr__(self):
        return "%s[%s]" % (self._className,self._String)

class SimpleBaseString_O(Array_O):
    def __init__(self,debugger,tptr):
        Array_O.__init__(self,debugger,tptr)
        end_offset = self._class._variable_capacity._end_offset
        char_size = self._class._variable_capacity._element_size
        data_offset = self._class._variable_array0._offset
        end = debugger.read_memory(self._address+end_offset,8)
        dbg_print("SimpleBaseString_O end_offset->%d char_size->%d data_offset->%d end->%d" % (end_offset,char_size,data_offset,end))
        self._String = read_string(debugger,self._address+data_offset,char_size,end)
    def str(self):
        return self._String
    def __repr__(self):
        return '"%s"' % self._String

class Package_O(General_O):
    def __init__(self,debugger,tptr):
        General_O.__init__(self,debugger,tptr)
        self._Name = self.field("_Name")
    def name(self):
        return self._Name
    def __repr__(self):
        return "Package[%s]" % (self._Name.str())

class Symbol_O(General_O):
    def __init__(self,debugger,tptr):
        General_O.__init__(self,debugger,tptr)
        self._Name = self.field("_Name")
        self._Package = self.field("_HomePackage")
    def __repr__(self):
        if (self._Package.nilp() and self._Name.str() == "UNBOUND"):
            return "#<UNBOUND>"
        try:
            return "Symbol[%s::%s]" % (self._Package.name().str(),self._Name.str())
        except:
            return "Symbol[%s %s]" % (self._Package, self._Name )

class GodObject_O(General_O):
    def __init__(self,debugger,tptr):
        General_O.__init__(self,debugger,tptr)
        self._Ptr = tptr
        self._debugger = debugger

    def __repr__(self):
        # dump all fields from general object
        out = StringIO()
        out.write("Dump %s at 0x%x\n" % (self._class._name, self._Ptr))
        idx = 1
        for offset in range(0,self._classSize,8):
            if (not offset in self._fieldAtOffset):
                addr = self._address+offset
                tptr = self._debugger.read_memory(addr,8)
                out.write("[         off: +%3d @0x%x] $f%d-> 0x%x %d\n" % (offset, addr, idx, tptr, tptr))
            else:
                cur = self._fieldAtOffset[offset]
                addr = self._address+cur._field_offset
                tptr = self._debugger.read_memory(addr,8)
                obj = translate_tagged_ptr(self._debugger,tptr)
                out.write("[type: %2d off: +%3d @0x%x] $f%d-> 0x%x %20s %s\n" %(cur._data_type, cur._field_offset, addr, idx, tptr, cur._field_name, obj))
            self._debugger.set_convenience_variable("f%d"%idx, tptr )
            idx += 1
        return out.getvalue()

def read_stamp_mtag(debugger,tclient):
    base = untag(tclient)-info["stampWtagMtagStruct"]._sizeof
    stamp_wtag_mtag_ptr = base+info["stampWtagMtagStruct"]._fields["_value"]._offset
    stamp_wtag_mtag_sizeof = info["stampWtagMtagStruct"]._fields["_value"]._sizeof
    stamp_wtag_mtag = debugger.read_memory(stamp_wtag_mtag_ptr,stamp_wtag_mtag_sizeof)
    mtag = 0
    if (stamp_wtag_mtag & info["ints"]["GENERAL_MTAG_MASK"] == info["ints"]["GENERAL_MTAG"]):
        stamp = stamp_wtag_mtag >> info["ints"]["GENERAL_STAMP_SHIFT"]
        mtag = 0
    else:
        stamp = stamp_wtag_mtag >> info["ints"]["MTAG_SHIFT"]
        mtag = stamp_wtag_mtag & info["ints"]["MTAG_MASK"]
    return (stamp, mtag)
        
def translate_tagged_ptr(debugger,tptr):
    dbg_print("In translate_tagged_ptr with tptr: 0x%x" % tptr)
    if (generalp(tptr)):
        base = untag_general(tptr)
        dbg_print("global_headerStruct in translate -> %s" % info["headerStruct"] )
        (stamp,mtag) = read_stamp_mtag(debugger,tptr)
        dbg_print("About to read_memory")
        if (stamp):
            if (stamp not in info["kinds"]):
                dbg_print("Could not find class for stamp: %d" % stamp)
            else:
                class_ = info["kinds"][stamp]
                name = class_._name
                dbg_print("general object class name = %s" % name)
                if (name=="core::Package_O"):
                    return Package_O(debugger,tptr)
                if (name=="core::Symbol_O"):
                    return Symbol_O(debugger,tptr)
                if (name=="core::SimpleBaseString_O"):
                    return SimpleBaseString_O(debugger,tptr)
                if (name=="core::SimpleCharacterString_O"):
                    return SimpleCharacterString_O(debugger,tptr)
                return General_O(debugger,tptr)
        return
    if (consp(tptr)):
        return Cons_O(debugger,tptr)
    if (fixnump(tptr)):
        return Fixnum(debugger,tptr)
    if (vaslistp(tptr)):
        return Vaslist(debugger,tptr)

    
def any_tagged_ptr(debugger,tptr):
    dbg_print("In any_tagged_ptr 0x%x" % tptr)
    if (generalp(tptr)):
        base = untag_general(tptr)
        dbg_print("global_headerStruct in translate -> %s" % info["headerStruct"])
        (stamp,mtag) = read_stamp_mtag(debugger,tptr)
        if (stamp):
            if (stamp not in info["kinds"]):
                dbg_print("Could not find class for stamp: %d" % stamp)
            else:
                class_ = info["kinds"][stamp]
                name = class_._name
                dbg_print("general object class name = %s" % name)
                return GodObject_O(debugger,tptr)
        return
    if (consp(tptr)):
        return Cons_O(debugger,tptr)
    if (fixnump(tptr)):
        return Fixnum(debugger,tptr)
    if (vaslistp(tptr)):
        return Vaslist(debugger,tptr)


############################################################    
############################################################    
############################################################    

def arg_to_tptr(debugger,args):
    args = args.split(" ")
    arg = args[0]
    verbose = False
    if (len(args)>1):
        verbose = True
    ptr = None
    if (arg[0]=='$'):
        tptr = int(debugger.convenience_variable(arg[1:]))
    elif (arg[0:2]=='0x' and is_int(arg,16)):
        tptr = int(arg[2:],16)
    elif (is_int(arg,10)):
        tptr = int(arg,10)
    else:
        tptr = int(debugger.evaluate(arg))
    return tptr

def do_lisp_test(debugger,arg):
    print("  In do_test arg: %s" % arg)
    print("    to tptr: %d" % arg_to_tptr(debugger,arg) )

def do_lisp_print(debugger_mod,arg):
    #print "In inspect args: %s" % args
    tptr = arg_to_tptr(debugger_mod,arg)
    obj = translate_tagged_ptr(debugger_mod,tptr)
    print( obj.__repr__())
    print( "translate_tagged_ptr returned: %s" % obj.__repr__())
    return obj

def do_lisp_inspect(debugger_mod,arg):
    #print "In inspect args: %s" % args
    tptr = arg_to_tptr(debugger_mod,arg)
    obj = any_tagged_ptr(debugger_mod,tptr)
    print( "any_tagged_ptr returned: %s" % obj.__repr__())
    return obj


def do_lisp_head(debugger_mod,arg):
    tptr = arg_to_tptr(debugger_mod,arg)
    tag = (tptr & info["ints"]["IMMEDIATE_MASK"])
    client = tptr - tag
    if (tag == info["ints"]["GENERAL_TAG"]):
        header = client - info["headerStruct"]._sizeof
    else:
        header = client - info["stampWtagMtagStruct"]._sizeof
    debugger_mod.dump_memory(header)




load_clasp_layout()

