import os
from io import StringIO

verbose = False   # turns on dbg_print(xxxx)
layout = None
debugger = False

data_type_smart_ptr = 1
data_type_atomic_smart_ptr = 2
data_type_tagged_ptr = 3

def dbg_print(msg):
    if (verbose):
        print(msg)

def shallowString(obj):
    if (obj):
        return obj.shallowString()
    return "%s" % obj

global_ints = {}
global_dataTypes = {}
global_kinds = {}
global_structs = {}
global_StampWtagMtagStruct = None
debugger = None

convenience = 0

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

    def __repr__(self):
        return "%s"%self._name
    
class TemplatedKind:
    def __init__(self,stamp,name,size):
        self._stamp = stamp
        self._name = name
        self._size = size
        self._fields = {}
        self._variable_array0 = None
        self._variable_capacity = None
        self._variable_fields = {}

class ContainerKind:
    def __init__(self,stamp,name,size):
        self._stamp = stamp
        self._name = name
        self._size = size
        self._fields = {}
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

    def __repr__(self):
        return "%s[+%d]" %( self._field_name, self._field_offset )
    
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
    dbg_print("In Init_class_kind stamp = %d name = %s\n" % (stamp, name))
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
    dbg_print("In Init__fixed_field stamp = %d field_name = %s field_offset=%d\n" % (stamp, field_name, field_offset))
    classKind = global_kinds[stamp]
    field = FixedField(index,data_type,field_name,field_offset)
    classKind._fields[index] = field

def Init__variable_array0(stamp,name,offset):
    # print("Init__variable_array0 stamp = %d\n" % stamp)
    classKind = global_kinds[stamp]
    classKind._variable_array0 = VariableArray0(name,offset)

def Init__variable_capacity(stamp,element_size,end_offset,capacity_offset):
    dbg_print("Init__variable_capacity stamp = %d\n" % stamp)
    classKind = global_kinds[stamp]
    dbg_print("Init__variable_capacity for %s" % classKind._name )
    classKind._variable_capacity = VariableCapacity(element_size,end_offset,capacity_offset)

def Init__variable_field(stamp,index,data_type,field_name,field_offset):
    # print("Init__variable_field stamp=%d\n" % stamp)
    classKind = global_kinds[stamp]
    field = VariableField(index,data_type,field_name,field_offset)
    classKind._variable_fields[field_name] = field




############################################################
############################################################


def verify_not_tagged(address):
    if (taggedp(address)):
        raise Exception("The address %x is still tagged" % address)

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

def characterp(tptr):
    return (flags(tptr)==info["ints"]["CHARACTER_TAG"])

def consp(tptr):
    return (flags(tptr)==info["ints"]["CONS_TAG"])

def untag_cons(tptr):
    return tptr-info["ints"]["CONS_TAG"]

def untag(tptr):
    return tptr - flags(tptr)

def vaslistp(tptr):
    return (tptr&info["ints"]["IMMEDIATE_MASK"]==info["ints"]["VASLIST0_TAG"])

def untag_vaslist(tptr):
    return (tptr - info["ints"]["VASLIST0_TAG"])

def read_unsigned_at_offset(debugger,verbose,base,offset):
    tptr = debugger.read_memory(base+offset,8)
    dbg_print("read_unsigned_at_offset offset: %x" % (base+offset))
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
    dbg_print("variable_fields -> %s" % class_._variable_fields.keys())
    for index in range(0,length_):
        element_offset = data_offset+element_size*index
        dbg_print("element[%d]@0x%x" % (index, base+element_offset))
        for field_name in class_._variable_fields.keys():
            field = class_._variable_fields[field_name]
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

def print_simple_character_string(debugger,verbose,indent,class_,obj):
    base = untag_general(obj)
    data_offset = class_._variable_array0._offset
    length_offset = class_._variable_capacity._end_offset
    length_ = read_unsigned_at_offset(debugger,verbose,base,length_offset)
    data = debugger.read_memory(base+data_offset,length_)
    dbg_print("Data: %s "% str(data))
    
def print_Closure_O(debugger,verbose,indent,class_,obj):
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
            if (name=="core::SimpleCharacterString_O"):
                if (not toplevel and verbose): dbg_print("class_ = %s" % class_.__dict__)
                print_simple_character_string(debugger,verbose,indent,class_,obj)
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
        
def twos_comp(val, bits):
    """compute the 2's complement of int value val"""
    if (val & (1 << (bits - 1))) != 0: # if sign bit is set e.g., 8bit: 128-255
        val = val - (1 << bits)        # compute negative value
    return val

class Fixnum:
    def __init__(self,debugger,address):
        if (fixnump(address)):
            val = address >> info["ints"]["FIXNUM_SHIFT"]
            if ((val & (1<<61))>0):
                self._Value = twos_comp(val,62)
            else:
                self._Value = val
            return
        raise Exception("%x is not a fixnum" % address)
    def value(self):
        return self._Value
    def __repr__(self):
        return str(self._Value)
    def shallowString(self):
        return self.__repr__()
    def consp(self):
        return False

class Character:
    def __init__(self,debugger,address):
        if (characterp(address)):
            val = address >> 3
            self._Value = val
            return
        raise Exception("%x is not a character" % address)
    def value(self):
        return self._Value
    def __repr__(self):
        return "#\[%s]" % str(self._Value)
    def shallowString(self):
        return self.__repr__()
    def consp(self):
        return False
    
class Vaslist:
    def __init__(self,debugger,address):
        if (vaslistp(address)):
            self._debugger = debugger
            self._Value = address
            self._address = untag_vaslist(address)
            self._args = debugger.read_memory(self._address+0,8)
            self._nargs = debugger.read_memory(self._address+8,8)>>info["ints"]["VASLIST-NARGS-SHIFT"]
            return
        raise Exception("%x is not a vaslist" % address)
    def value(self):
        return self._Value
    def __repr__(self):
        out = StringIO()
        out.write("Vaslist: 0x%x args: 0x%x  nargs: %d\n" % (self._Value,self._args, self._nargs))
        if (self._nargs>2048):
            raise Exception("%x is not a proper vaslist - it has %d values" % (self._address, self._nargs))
        for index in range(0,self._nargs):
            rgp = self._args+8*index
            val = self._debugger.read_memory(rgp,8)
            out.write("     farg%d = 0x%x\n" % (index,val))
        return out.getvalue()
    def inspectString(self):
        return self.__repr__()
    def shallowString(self):
        return self.__repr__()
    def consp(self):
        return False
    def generalp(self):
        return False
    def fixnump(self):
        return False
    
class T_O:
    def consp(self):
        return False
    def generalp(self):
        return False
    def fixnump(self):
        return False
    def shallowString(self):
        return self.__repr__()
    
class Cons_O(T_O):
    def __init__(self,debugger,address):
        if (consp(address)):
            address = untag_cons(address)
        verify_not_tagged(address)
        self._debugger = debugger
        self._address = address
    def consp(self):
        return True
    def nilp(self):
        return False
    def car(self):
        return self._debugger.read_memory(self._address,8)
    def cdr(self):
        return self._debugger.read_memory(self._address+8,8)
    def inspectString(self):
        return "(0x%x . 0x%x)" % (self.car(), self.cdr())
    def shallowString(self):
        return self.inspectString()
    def tailStr(self):
        carObj = translate_tagged_ptr(self._debugger, self.car())
        carStr = str(carObj)
        cdrObj = translate_tagged_ptr(self._debugger, self.cdr())
        if (cdrObj.nilp()):
            return carStr + ")"
        elif (cdrObj.consp()):
            return carStr + " " + cdrObj.tailStr()
        else: # dotted list
            return carStr + " . " + str(cdrObj) + ")"
    def __repr__(self):
        carObj = translate_tagged_ptr(self._debugger,self.car())
        carStr = carObj.__repr__()
        cdrObj = translate_tagged_ptr(self._debugger,self.cdr())
        cdrStr = cdrObj.__repr__()
        return "(%s . %s)" % ( carStr, cdrStr )
    def __str__(self):
        return "(" + self.tailStr()
        
class General_O(T_O):
    def __init__(self,debugger,tclient):
        self._tptr = tclient
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
        self._fieldAtOffset = {}
        for idx in range(len(self._class._fields)):
            try:
                cur = self._class._fields[idx]
                self._fieldAtOffset[cur._field_offset] = cur
            except:
                cur = None

    def generalp(self):
        return True
    def fieldWithName(self,name):
        field_ = None
        for idx in range(len(self._class._fields)):
            cur = self._class._fields[idx]
            if (cur._field_name==name):
                field_ = cur
                break
        if (field_):
            return field_
        dbg_print("In class %s" % self._class)
        dbg_print("Could not find field %s in %s" % (name, self._class._fields))
        raise Exception("Could not find field named: %s" % name)

    def offsetOfFieldWithName(self,name):
        field_ = self.fieldWithName(name)
        return field_._field_offset

    def field_tagged_ptr(self,name):
        field_ = self.fieldWithName(name)
        return self._debugger.read_memory(self._address+field_._field_offset,8)

    def field(self,name):
        field_ = self.fieldWithName(name)
        if (field_):
            offset = field_._field_offset
            if (field_._data_type == 0):
                tptr = self._debugger.read_memory(self._address+offset,8)
                return translate_tagged_ptr(self._debugger,tptr)
            if (field_._data_type == 1):
                tptr = self._debugger.read_memory(self._address+offset,8)
                return translate_tagged_ptr(self._debugger,tptr)
            if (field_._data_type == 2):
                tptr = self._debugger.read_memory(self._address+offset,8)
                return translate_tagged_ptr(self._debugger,tptr)
            if (field_._data_type == 7):
                val = self._debugger.read_memory(self._address+offset,8)
                return val
            if (field_._data_type == 4):
                val = self._debugger.read_memory(self._address+offset,8)
                return val
            raise Exception("Handle _data_type %d for %s" % (field_._data_type, field_._field_name))
        raise Exception("There is no field named %s in %s" % ( name, self._className))

    def variable_data_offset(self):
        data_offset = max(self._class._variable_capacity._end_offset,self._class._variable_capacity._capacity_offset)+8
        addr = self._address+data_offset
        return addr

    def array_element_field_addr(self,index,variable_field_name):
        array = self.variable_data_offset()
        element_size = self._class._variable_capacity._element_size
        try:
            variable_field = self._class._variable_fields[variable_field_name]
        except:
            raise Exception("Could not find %s in %s" % (variable_field_name,self._class._variable_fields.keys()))
        addr = array+(element_size * index) + variable_field._field_offset
        return addr
  
    def nilp(self):
        result = (self._className == "core::Null_O")
        return result
    
    def __repr__(self):
        return "a %s @0x%x" % (self._className, self._tptr) 

    def __str__(self):
        if self.nilp():
            return "()"
        else:
            return repr(self)

class Array_O(General_O):
    def __init__(self,debugger,address):
        General_O.__init__(self,debugger,address)

class SimpleVector_O(Array_O):
    def __init__(self,debugger,tptr):
        Array_O.__init__(self,debugger,tptr)
        self._end_offset = self._class._variable_capacity._end_offset
        self._element_size = self._class._variable_capacity._element_size
        self._data_offset = self._class._variable_array0._offset
        self._debugger = debugger
        self._end = debugger.read_memory(self._address+self._end_offset,8)
    def aref(self, index):
        val = self._debugger.read_memory(self._data_offset+self._address+(index*self._element_size),self._element_size);
        return translate_tagged_ptr(self._debugger, val)
    def __str__(self):
        if (self._end == 0): # empty vector #()
            return "#()"
        else:
            result = "#(" + str(self.aref(0))
            for x in range(1, self._end):
                result = result + " " + str(self.aref(x))
            return result + ")"
    def dump(self,start=0,end=None):
        out = StringIO();
        if (end==None):
            end = self._end;
        end = min(end,self._end);
        start = max(0,start);
        for x in range(start,end):
           val = self._debugger.read_memory(self._data_offset+self._address+(x*self._element_size),self._element_size);
           out.write("[%d] = %s\n" % (x,inspect_tagged_ptr(val).str()))
        return out.getvalue();
    def __repr__(self):
        return str(self)

    
class SimpleCharacterString_O(Array_O):
    def __init__(self,debugger,tptr):
        Array_O.__init__(self,debugger,tptr)
        end_offset = self._class._variable_capacity._end_offset
        char_size = self._class._variable_capacity._element_size
        data_offset = self._class._variable_array0._offset
        end = debugger.read_memory(self._address+end_offset,8)
        dbg_print("SimpleCharacterString_O end_offset->%d char_size->%d data_offset->%d end->%d" % (end_offset,char_size,data_offset,end))
        data = ''
        for ci in range(0,end):
            charval = debugger.read_memory(self._address+data_offset+ci*char_size,char_size)
            data = data + chr(charval)
        self._String = data
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

class Function_O(General_O):
    def __init__(self,debugger,tptr):
        General_O.__init__(self,debugger,tptr)
        self._TheSimpleFun = self.field("_TheSimpleFun")
        self._FunctionDescription = self._TheSimpleFun.field("_FunctionDescription")
        self._Name = self._FunctionDescription.field("_functionName")
    def name(self):
        return self._Name
    def __repr__(self):
        return "Function[:name %s]" % self.name()
    def __str__(self):
        return "#<FUNCTION %s>" % self.name()

class SimpleFun_O(General_O):
    def __init__(self,debugger,tptr):
        General_O.__init__(self,debugger,tptr)
        self._FunctionDescription = self.field("_FunctionDescription")
        self._Name = self._FunctionDescription.field("_functionName")
    def name(self):
        return self._Name
    def __repr__(self):
        return "Function[:name %s]" % self.name()
    def __str__(self):
        return "#<SIMPLE-FUN %s>" % self.name()


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
    def __str__(self):
        if (self._Package.nilp()):
            if self._Name.str() == "UNBOUND":
                return "#<UNBOUND>"
            else:
                return "#:" + self._Name.str()
        return self._Package.name().str() + "::" + self._Name.str()

class Rack_O(General_O):
    def __init__(self,debugger,tptr):
        General_O.__init__(self,debugger,tptr)
    def __repr__(self):
        return "#<Rack>"

class JITDylib_O(General_O):
    def __init__(self,debugger,tptr):
        General_O.__init__(self,debugger,tptr)
        self._name = translate_tagged_ptr(self._debugger,self.field_tagged_ptr("_name"))
    def __repr__(self):
        return "#<JITDylib %s @ 0x%x>" % (self._name.__repr__(), self._address )

class Instance_O(General_O):
    def __init__(self,debugger,tptr):
        General_O.__init__(self,debugger,tptr)
        self._debugger = debugger
        self._Class_tagged_ptr = self.field_tagged_ptr("_Class")
        self._Rack_tagged_ptr = self.field_tagged_ptr("_Rack")
        dbg_print("Instance_O self._class._fields = %s" % self._class._fields)
    def className(self):
        class_ = translate_tagged_ptr(self._debugger,self._Class_tagged_ptr)
        rack = translate_tagged_ptr(self._debugger,self._Rack_tagged_ptr)
        class_rack_tptr = class_.field_tagged_ptr("_Rack")
        class_rack = translate_tagged_ptr(self._debugger,class_rack_tptr)
        class_name_slot_index = info["ints"]["REF_CLASS_CLASS_NAME"]
        class_name_addr = class_rack.array_element_field_addr(class_name_slot_index,"only")
        class_name_tptr = self._debugger.read_memory(class_name_addr,len=8)
        class_name_obj = translate_tagged_ptr(self._debugger,class_name_tptr)
        return str(class_name_obj)
    def str(self):
        return "#<%s>" % self.className()
    def dump(self,start=0,end=None):
        return self.str()
    def __repr__(self):
        return self.str()


def nextConvenienceCharacter():
    global convenience
    thechr = chr(convenience+ord('a'))
    convenience = convenience + 1
    if (convenience>25):
        convenience = 0
    return thechr

class UniversalObject_O(General_O):
    def __init__(self,debugger,tptr):
        General_O.__init__(self,debugger,tptr)
        self._Ptr = tptr
        self._debugger = debugger

    def inspectString(self):
        # dump all fields from general object
        out = StringIO()
        out.write("Dump %s at 0x%x\n" % (self._class._name, self._Ptr))
        idx = 1
        # print("global_kinds = %s\n" % global_kinds[306]._fields )
        convchar = nextConvenienceCharacter()
        convcharindex = 0
        fixedSize = self._class._size
        variableEndAndCapacity = True
        variableArray0 = False
        if (self._class._variable_array0 != None):
            variableArray0 = True
            variableEndAndCapacity = (self._class._variable_capacity._end_offset != self._class._variable_capacity._capacity_offset)
        for offset in range(0,fixedSize,8):
            data_type_name = ""
            field_name = ""
            obj_str = ""
            maybe_convchar_assign = ""
            addr = self._address+offset
            maybe_confchar_assign = ""
            if (variableArray0 and (offset == self._class._variable_capacity._end_offset)):
                tptr = self._debugger.read_memory(addr,8)
                obj_str = "%d" % tptr
                field_name = "_End"
            elif (variableArray0 and (offset == self._class._variable_capacity._capacity_offset and variableEndAndCapacity)):
                tptr = self._debugger.read_memory(addr,8)
                obj_str = "%d" % tptr
                field_name = "_Capacity"
            elif (not offset in self._fieldAtOffset):
                tptr = self._debugger.read_memory(addr,8)
                out_str = "0x%x"%tptr
            else:
                cur = self._fieldAtOffset[offset]
                field_name = cur._field_name
                data_type_index = cur._data_type
                data_type = info["dataTypes"][data_type_index]
                data_type_name = "%d" % data_type_index
                if ( (data_type_index == data_type_smart_ptr) or
                     (data_type_index == data_type_atomic_smart_ptr) or
                     (data_type_index == data_type_tagged_ptr) ):
                    tptr = self._debugger.read_memory(addr,8)
                    maybe_convchar = "%c%d" % (convchar, convcharindex)
                    convcharindex = convcharindex + 1
                    self._debugger.set_convenience_variable(maybe_convchar, tptr )
                    maybe_convchar_assign = "$%s->" % maybe_convchar
#                    obj_str = shallowString(translate_tagged_ptr(self._debugger,tptr))
                    try:
                        obj_str = shallowString(translate_tagged_ptr(self._debugger,tptr))
                    except:
                        obj_str = "UNTRANSLATABLE_OBJECT(0x%x)"%tptr
                else:
                    tptr = self._debugger.read_memory(addr,data_type._sizeof)
                    maybe_convchar = "%c%d" % (convchar, convcharindex)
                    convcharindex = convcharindex + 1
                    maybe_convchar_assign = "$%s->" % maybe_convchar
                    self._debugger.set_convenience_variable(maybe_convchar, tptr )
                    obj_str = "%d" % tptr
            out.write("[type: %2s off: +%3d @0x%x] %8s %18s %20s %s\n" % (data_type_name, offset, addr, maybe_convchar_assign, "0x%x"%tptr, field_name, obj_str))
            idx += 1
        if (self._class._variable_array0 != None):
            data_offset = max(self._class._variable_capacity._end_offset,self._class._variable_capacity._capacity_offset)+8
            addr = self._address+data_offset
            maybe_convchar = "%c%d" % (convchar, convcharindex)
            convcharindex = convcharindex + 1
            maybe_convchar_assign = "<-$%s" % maybe_convchar
            self._debugger.set_convenience_variable(maybe_convchar, addr )
            out.write("[-vector- off: +%3d @0x%x %s]\n" % (data_offset, addr, maybe_convchar_assign ))
            out.write("[  fields: %s ]\n" % self._class._variable_fields )
        return out.getvalue()
    def __repr__(self):
        return self.inspectString()

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
        dbg_print("About to assign class using stamp: %d" % stamp)
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
                if (name=="core::SimpleVector_O"):
                    return SimpleVector_O(debugger,tptr)
                if (name=="core::Instance_O"):
                    return Instance_O(debugger,tptr)
                if (name=="core::Rack_O"):
                    return Rack_O(debugger,tptr)
                if (name in ["core::Function_O", "core::FuncallableInstance_O", "core::Closure_O", "core::BuiltinClosure_O"]):
                    return Function_O(debugger,tptr)
                if (name in ["core::BytecodeSimpleFun_O", "core::SimpleCoreFun_O", "SimpleFun_O"]):
                    return SimpleFun_O(debugger,tptr)
                if (name=="llvmo::JITDylib_O"):
                    return JITDylib_O(debugger,tptr)
                return General_O(debugger,tptr)
        return
    if (consp(tptr)):
        return Cons_O(debugger,tptr)
    if (fixnump(tptr)):
        return Fixnum(debugger,tptr)
    if (characterp(tptr)):
        return Character(debugger,tptr)
    if (vaslistp(tptr)):
        return Vaslist(debugger,tptr)

    
def inspect_tagged_ptr(debugger,tptr):
    dbg_print("In inspect_tagged_ptr 0x%x" % tptr)
    if (generalp(tptr)):
        base = untag_general(tptr)
        dbg_print("global_headerStruct in translate -> %s" % info["headerStruct"])
        (stamp,mtag) = read_stamp_mtag(debugger,tptr)
        if (stamp):
            if (stamp not in info["kinds"]):
                dbg_print("Could not find class for stamp: %d" % stamp)
            else:
                dbg_print("Assigning class_ using stamp %d" % stamp)
                class_ = info["kinds"][stamp]
                name = class_._name
                dbg_print("general object class name = %s" % name)
                return UniversalObject_O(debugger,tptr)
        return
    if (consp(tptr)):
        return Cons_O(debugger,tptr)
    if (fixnump(tptr)):
        return Fixnum(debugger,tptr)
    if (vaslistp(tptr)):
        dbg_print("vaslistp is True")
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
        tptr = debugger.evaluate_int(arg)
    return tptr

def do_lisp_vm(debugger,arg):
    fp = debugger.evaluate_int("my_thread->_VM._framePointer")
    sp = debugger.evaluate_int("my_thread->_VM._stackPointer")
    pc = debugger.evaluate_int("my_thread->_VM._pc")
    instr = debugger.evaluate_int("*(unsigned char*)(my_thread->_VM._pc)")
    print("0x%x [%2d] %-20s | sp=0x%x fp=0x%x\n" % (pc, instr, global_codes[instr]._name, sp, fp ))


def do_lisp_frame(debugger,arg):
    frame_args = debugger.lisp_selected_frame()
    closure = frame_args[0]
    oclosure = translate_tagged_ptr(debugger,closure)
    nargs = frame_args[1]
    args = frame_args[2]
    call = [ oclosure.__repr__() ]
    for iarg in range(0,nargs):
        one_arg = debugger.read_memory(args+(8*iarg),len=8)
        tone_arg = translate_tagged_ptr(debugger,one_arg)
        call.append( tone_arg.__repr__() )
    print("%s" % call)

def gather_frame_pointers(debugger):
#    obj = global_names["long"]
#    print("test obj._name = %s" % obj._name)
#    print("test obj._value = %s" % obj._value)
    newest_frame = debugger.newest_frame()
    frame = newest_frame
    level = 0
    prev_pc = None
    frames = []
    while (frame != None):
        frames.append(frame)
        frame = debugger.older_frame(frame)
        level = level + 1
    return frames

def stack_address_level(debugger,frames,target):
    dbg_print("Looking for target: 0x%x" % target)
    level = 0
    for frame in frames:
        dbg_print("%d 0x%x %s" % (level,debugger.frame_base_pointer(frame),frame))
        if (target<=debugger.frame_base_pointer(frame)):
            return level
        level = level + 1

def do_lisp_test(debugger,arg):
    obj = global_names["long"]
    print("test obj._name = %s" % obj._name)
    print("test obj._value = %s" % obj._value)

def do_lisp_print_vector(debugger,arg):
    args = arg.split(" ")
    print("args = %s" % args)
    start = arg_to_tptr(debugger,args[0])
    num = int(args[1])
    for im in range(0,num):
        tptr = debugger.read_memory(start,len=8)
        tptr_obj = translate_tagged_ptr(debugger,tptr)
        print("0x%x -> %s" % (start, tptr_obj.__repr__()))
        start = start + 8

def do_lisp_disassemble(debugger,arg):
    tptr = arg_to_tptr(debugger,arg)
    obj = translate_tagged_ptr(debugger,tptr)
    literals = obj.field("_Literals")
    literals_address = literals._address
    literals_class_size = literals._class._size
    literals_start = literals_address + literals_class_size
    bytecode = obj.field("_Bytecode")
    bytecode_address = bytecode._address
    bytecode_len = bytecode.field("_Length")
    bytecode_class_size = bytecode._class._size
    bytecode_start = bytecode_address + bytecode_class_size
    bytecode_end = bytecode_start + bytecode_len
    print( "test bytecode_start = %x" % bytecode_start)
    print( "test bytecode_len   = %d" % bytecode_len)
    print( "test literals       = %x" % literals_start )
    (vm,instructions,labels) = disassemble_region(debugger,literals_start,bytecode_start,bytecode_end)
    print("labels = %s" % labels)
    for label in labels.values():
        print( "label: 0x%x %s" % (label._address, label._name))
    print_disassembly(vm,instructions,labels)

def do_lisp_print(debugger_mod,arg):
    tptr = arg_to_tptr(debugger_mod,arg)
    obj = translate_tagged_ptr(debugger_mod,tptr)
    print(str(obj))
    dbg_print( "in do_lisp_print: %s" % obj.__repr__())
    return str(obj)

def isLexDynEnv(car_obj):
    if (car_obj._className == "core::LexDynEnv_O" or
        car_obj._className == "core::TagbodyDynEnv_O" or
        car_obj._className == "core::BlockDynEnv_O"):
        return True
    return False

def isUnwindProtectDynEnv(car_obj):
    if (car_obj._className == "core::UnwindProtectDynEnv_O"):
        return True
    return False

def isBindingDynEnv(car_obj):
    if (car_obj._className == "core::BindingDynEnv_O"):
        return True
    return False

def do_lisp_dump_dyn_env_stack(debugger_mod,arg):
    #print "In inspect args: %s" % args
    tptr = debugger_mod.evaluate_int("my_thread->_DynEnvStackBottom.theObject")
    depth = 999999
    if (arg!=""):
        depth = int(arg)
    frames = gather_frame_pointers(debugger_mod)
    while (consp(tptr) ):
        if (depth==0):
            return
        tobj = translate_tagged_ptr(debugger_mod,tptr)
        tframe_level = stack_address_level(debugger_mod,frames,tptr)
        car = tobj.car()
        tptr = tobj.cdr()
        car_obj = translate_tagged_ptr(debugger_mod,car)
        extra_info = ""
        if (isLexDynEnv(car_obj)):
            target = car_obj.field("target")
            target_level = stack_address_level(debugger_mod,frames,target)
            frame = car_obj.field("frame")
            frame_level = stack_address_level(debugger_mod,frames,frame)
            extra_info = ":target 0x%x(#%-3d) :frame  0x%x(#%-3d)" % (target, target_level, frame, frame_level)
        elif (isUnwindProtectDynEnv(car_obj)):
            target = car_obj.field("target")
            target_level = stack_address_level(debugger_mod,frames,target)
            extra_info = ":target 0x%x(#%-3d)" % (target, target_level)
        elif (isBindingDynEnv(car_obj)):
            address = car_obj._address
            address_level = stack_address_level(debugger_mod,frames,address)
            extra_info = ":addr   0x%x(#%-3d)" % (address, address_level)
            
        print("0x%x(#%-3d) [0x%x %s] %s" % (tptr, tframe_level, car, car_obj, extra_info))
        depth = depth - 1

def do_lisp_print_value(debugger_mod,arg):
    #print "In inspect args: %s" % args
    tptr = arg_to_tptr(debugger_mod,arg)
    obj = translate_tagged_ptr(debugger_mod,tptr)
    dbg_print( "in do_lisp_print_value: %s" % obj.__repr__())
    return obj.__repr__()

#
# Return the name of a function given a tagged pointer that is a function object
#
def function_name(debugger_mod,tptr):
    fn = translate_tagged_ptr(debugger_mod,tptr)
    if (not (isinstance(fn, Function_O) or isinstance(fn, SimpleFun_O))):
        return None
    fn_name = fn.name()
    if (isinstance(fn_name,Symbol_O)):
        name = fn_name
    else:
        name = "%s" % fn_name
    return name

def do_lisp_inspect(debugger_mod,arg):
    dbg_print( "In inspect args: %s" % arg )
    tptr = arg_to_tptr(debugger_mod,arg)
    print("inspect tptr 0x%x" % tptr)
    obj = inspect_tagged_ptr(debugger_mod,tptr)
    print("%s" % obj.inspectString())
    dbg_print( "inspect_tagged_ptr returned:\n %s" % obj.__repr__())
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

def do_lisp_head_bytes(debugger_mod,arg):
    tptr = arg_to_tptr(debugger_mod,arg)
    tag = (tptr & info["ints"]["IMMEDIATE_MASK"])
    client = tptr - tag
    if (tag == info["ints"]["GENERAL_TAG"]):
        header = client - info["headerStruct"]._sizeof
    else:
        header = client - info["stampWtagMtagStruct"]._sizeof
    debugger_mod.dump_memory(header,bytes=True)

def do_(debugger_mod,arg):
    tptr = arg_to_tptr(debugger_mod,arg)
    tag = (tptr & info["ints"]["IMMEDIATE_MASK"])
    client = tptr - tag
    if (tag == info["ints"]["GENERAL_TAG"]):
        header = client - info["headerStruct"]._sizeof
    else:
        header = client - info["stampWtagMtagStruct"]._sizeof
    debugger_mod.dump_memory(header,bytes=True)


def dome():
    print("In dome")


class label():
    def __init__(self,address,name):
        self._address = address
        self._name = name

class instruction_description():
    def __init__(self,name,val,arguments=[],long_arguments=[]):
        self._name = name
        self._value = val
        self._arguments = arguments
        self._long_arguments = long_arguments

    def arguments(self,isLong):
        if (isLong):
            return self._long_arguments
        else:
            return self._arguments

class instruction():
    def __init__(self,address,isLong,description,args):
        self._address = address
        self._isLong = isLong
        self._description = description
        self._args = args

    def maybe_label_addresses(self):
        labels = []
        arginfos = self._description.arguments(self._isLong)
        for argi in range(0,len(arginfos)):
            arginfo = arginfos[argi]
            arg = self._args[argi]
            if (arginfo & const_argtype_mask == const_label_arg):
                labels.append(self._address+arg)
        if (len(labels)==0):
            return None
        else:
            return labels

#
# Instruction arguments are ( argtype | argvalue)
#
const_arglength_mask   = 0b000111
const_argtype_mask     = 0b111000
const_constant_arg     = 0b001000
const_keys_arg         = 0b011000
const_label_arg        = 0b010000

global_vm_long = 0
def new_instr(name, code, args=[], long_args=[]):
    instr = instruction_description(name,code,args,long_args)
    global_codes[instr._value] = instr
    global_names[instr._name] = instr
    if (instr._name == "long"):
        global_vm_long = instr._value

def constant_arg(index):
    return const_constant_arg|index

def keys_arg(index):
    return const_keys_arg|index

def label_arg(index):
    return const_label_arg|index

global_codes = [None] * (256)
global_names = {}

## opcodes are loaded by load_clasp_layout

def load_clasp_layout(thedebugger,verbose=False):
    (vm_opcodes, class_layouts) = thedebugger.clasp_python_info()
    exec(vm_opcodes)
    exec(class_layouts)
    if (verbose==True):
        print("verbose = %s" % verbose)
        print("class_layouts = %s" % class_layouts )
        print("vm_opcodes = %s" % vm_opcodes)
    SetupGlobals()


class virtual_machine():
    def __init__(self,debugger,pc,literals):
        self._pc = pc
        self._debugger = debugger
        self._literals = literals

    def next_uint8(self):
        val = self._debugger.read_memory(self._pc,len=1)
        self.incf(1)
        return val

    def next_uint16(self):
        byte0 = self._debugger.read_memory(self._pc,len=1)
        byte1 = self._debugger.read_memory(self._pc+1,len=1)
        val = byte0 + ash(byte1,8)
        self.incf(2)
        return val

    def next_int8(self):
        byte0 = self._debugger.read_memory(self._pc,len=1)
        self.incf()
        if (byte0>=128):
            return 256-byte0
        return byte0

    def next_int16(self):
        byte0 = self._debugger.read_memory(self._pc,len=1)
        byte1 = self._debugger.read_memory(self._pc+1,len=1)
        word = byte0+ash(byte1,-8)
        self.incf(2)
        if (word>=32768):
            return 65536-word
        return word

    def next_int24(self):
        byte0 = self._debugger.read_memory(self._pc,len=1)
        byte1 = self._debugger.read_memory(self._pc+1,len=1)
        byte2 = self._debugger.read_memory(self._pc+2,len=1)
        word = byte0+ash(byte1,-8)+ash(byte2,-16)
        self.incf(3)
        print("Read int24 %d %d %d -> %d" % (byte0, byte1, byte2, word))
        if (word>=8388608):
            return 16777216-word
        return word
    
    def incf(self,num=1):
        self._pc = self._pc + num


def ash( val, count ):
    if (count>0):
        return val >> count
    else:
        return val << (- count )

def logand( x, y ):
    return x & y

def logior( x, y ):
    return x | y

def lognot(x):
    return ~x

def early_mask_field( size, position, integer ):
    return logand(ash(lognot(ash(-1,size)), position), integer)

def dis_signed(x, size):
    return logior(x,- early_mask_field(1,(size-1),x))


def next_arg(pc,arginfo):
    argtype = const_argtype_mask & arginfo
    arglength = const_arglength_mask & arginfo
    if (argtype!=const_label_arg):
        if (arglength==1):
            arg = pc.next_uint8()
            return arg
        if (arglength==2):
            arg = pc.next_uint16()
            return arg
    else:
        if (arglength==1):
            return pc.next_int8()
        if (arglength==2):
            return pc.next_int16()
        if (arglength==3):
            return pc.next_int24()
    raise Exception("Bad argument info %d" % arginfo)



def disassemble_instruction(vm):
    address = vm._pc
    isLong = False
    code_or_long = vm.next_uint8()
#    print("Instruction 0x%x  %d" % (address,code_or_long))
    if (code_or_long==global_vm_long):
        isLong = True
        code = vm.next_uint8()
        code_name = global_codes[code]._name
        code_arg_types = global_codes[code]._long_arguments
    else:
        code = code_or_long
        code_name = global_codes[code]._name
        code_arg_types = global_codes[code]._arguments
    args = [ next_arg(vm,arg_type) for arg_type in code_arg_types ]
    instr = instruction(address,isLong,global_codes[code],args)
    return instr

def expand_arg(vm,arg_index,instr,labels):
    description = instr._description
    arginfos = description.arguments(instr._isLong)
    arginfo = arginfos[arg_index]
#    print("expand_arg arg_index = %d arginfo = %d" % (arg_index,arginfo))
    arg = instr._args[arg_index]
    argtype = arginfo & const_argtype_mask
#    print("expand_arg arg_type = %d " % argtype )
    if (argtype==const_constant_arg):
        arg_tptr = vm._debugger.read_memory(vm._literals+arg*8,len=8)
        arg_object = translate_tagged_ptr(vm._debugger,arg_tptr)
        arg_str = "%s" % arg_object.__repr__()
    elif (argtype==const_keys_arg):
        keys_count = instr._args[arg_index-1]
        keys = []
        for keyi in range(0,keys_count):
            key_tptr = vm._debugger.read_memory(vm._literals+(arg+keyi)*8,len=8)
            key_object = translate_tagged_ptr(vm._debugger,key_tptr)
            keys.append(key_object.__str__())
        arg_str = "%s" % keys
    elif (argtype==const_label_arg):
        label_address = instr._address+arg
        if (label_address in labels):
            label = labels[label_address]
            arg_str = "%s[%d]" % (label._name, arg)
        else:
            arg_str = "<Illegal label address 0x%8x>" % label_address
    else:
        arg_str = "%d" % arg
    return arg_str

def print_instruction(vm,instr,labels):
    expanded_args = [ expand_arg(vm,argi,instr,labels) for argi in range(0,len(instr._args)) ]
    print(" 0x%8x %20s %s" % (instr._address, instr._description._name, expanded_args) )

def disassemble_region(debugger,literals,start_address,end_address):
    vm = virtual_machine(debugger,start_address,literals)
    # gather all the disassembled instructions
    instructions = []
    while vm._pc<end_address:
        instruction = disassemble_instruction(vm)
        instructions.append(instruction)
    # Gather any special addresses/labels
    label_index = 0
    labels = {}
    for instr in instructions:
        maybe_label_addresses = instr.maybe_label_addresses()
        if (maybe_label_addresses):
            for label_address in maybe_label_addresses:
                onelabel = label(label_address,"L%d" % label_index)
                labels[label_address] = onelabel
                label_index = label_index + 1
    return (vm,instructions, labels)

def print_disassembly(vm,instructions,labels):
#    for key,label in labels.items():
#        print("label = 0x%x %s" % (label._address,label._name))
    for instr in instructions:
        if (instr._address in labels):
            print("%s:" % labels[instr._address]._name)
        print_instruction(vm,instr,labels)
