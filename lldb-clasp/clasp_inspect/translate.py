
import clasp_inspect.object_layout

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
    if (verbose): debugger.dbg_print("read_unsigned_at_offset offset: %x" % (base+offset))
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
    debugger.dbg_print("%d slots" % length_)
    debugger.dbg_print("variable_fields -> %s" % class_._variable_fields)
    for index in range(0,length_):
        element_offset = data_offset+element_size*index
        debugger.dbg_print("element[%d]@0x%x" % (index, base+element_offset))
        for field_index in class_._variable_fields:
            field = class_._variable_fields[field_index]
            field_offset = element_offset + field._field_offset
            data = read_unsigned_at_offset(debugger,verbose,base,field_offset)
            debugger.dbg_print("%s%s -> %s" % (indent,field._field_name, valid_tptr(data)));
    
def print_simple_base_string(debugger,verbose,indent,class_,obj):
    base = untag_general(obj)
    data_offset = class_._variable_array0._offset
    length_offset = class_._variable_capacity._end_offset
    length_ = read_unsigned_at_offset(debugger,verbose,base,length_offset)
    data = debugger.read_memory(base+data_offset,length_)
    debugger.dbg_print("Data: %s "% str(data))

def print_ClosureWithSlots_O(debugger,verbose,indent,class_,obj):
    debugger.dbg_print("class dict -> %s" % class_.__dict__)
    
def print_shallow_object_type(debugger,verbose,indent,obj,type_=0,toplevel=False):
    if (generalp(obj)):
        base = untag_general(obj)
        header_ptr = base - clasp_inspect.object_layout.global_HeaderStruct._sizeof;
        header = debugger.read_memory(header_ptr,8)
        if (header):
            stamp = header>>4
            if (not toplevel and verbose): debugger.dbg_print("%sheader@%x stamp = %d" % (indent,header_ptr,stamp))
            class_ = clasp_inspect.object_layout.global_Kinds[stamp]
            name = class_._name
            if (name=="core::SimpleBaseString_O"):
                if (not toplevel and verbose): debugger.dbg_print("class_ = %s" % class_.__dict__)
                print_simple_base_string(debugger,verbose,indent,class_,obj)
                return True
            if (not toplevel and verbose): debugger.dbg_print("%sclass = %s" % (indent,name))
            return False
    return False

def read_string(debugger,address,char_size,end):
    str = ""
    debugger.dbg_print( "read_string address 0x%x  char_size=%d  end=%d" % ( address, char_size, end ))
    for index in range(end):
        char_val = debugger.read_memory(address+(index*char_size),char_size)
        if (char_val>127):
            char_char = "\\%d" % char_val
        else:
            char_char = chr(char_val)
        str += char_char
    return str
        

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
    def __init__(self,debugger,address):
        if (generalp(address)):
            address = untag_general(address)
        verify_not_tagged(address)
        self._debugger = debugger
        self._address = address
        debugger.dbg_print("In General_O")
        self._header_ptr = address - clasp_inspect.object_layout.global_HeaderStruct._sizeof
        header = debugger.read_memory(self._header_ptr,8)
        if (header):
            self._stamp = header>>4
            self._class = clasp_inspect.object_layout.global_Kinds[self._stamp]
            self._className = self._class._name
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
            raise Exception("Handle _data_type %d" % field_._data_type)
        raise Exception("There is no field named %s in %s" % ( name, self._className))
    
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
        debugger.dbg_print("SimpleCharacterString_O end_offset->%d char_size->%d data_offset->%d end->%d" % (end_offset,char_size,data_offset,end))
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
        debugger.dbg_print("SimpleBaseString_O end_offset->%d char_size->%d data_offset->%d end->%d" % (end_offset,char_size,data_offset,end))
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
        return "Package[%s]" % (self._Package._Name.str())

class Symbol_O(General_O):
    def __init__(self,debugger,tptr):
        General_O.__init__(self,debugger,tptr)
        self._Name = self.field("_Name")
        self._Package = self.field("_HomePackage")
    def __repr__(self):
        return "Symbol[%s::%s]" % (self._Package.name().str(),self._Name.str())

def translate_tagged_ptr(debugger,tptr):
    debugger.dbg_print("In translate_tagged_ptr 0x%x" % tptr)
    if (generalp(tptr)):
        base = untag_general(tptr)
        debugger.dbg_print("global_HeaderStruct in translate -> %s" % clasp_inspect.object_layout.global_HeaderStruct )
        header_ptr = base - clasp_inspect.object_layout.global_HeaderStruct._sizeof
        debugger.dbg_print("About to read_memory")
        header = debugger.read_memory(header_ptr,8)
        if (header):
            stamp = header>>4
            if (debugger._verbose): debugger.dbg_print("header@%x stamp = %d" % (header_ptr,stamp))
            class_ = clasp_inspect.object_layout.global_Kinds[stamp]
            name = class_._name
            debugger.dbg_print("general object class name = %s" % name)
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
        return Fixnum(tptr)

    
