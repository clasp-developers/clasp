

def print_tagged_ptr(debugger,verbose,tptr,toplevel=False):
    if (generalp(tptr)):
        (stamp,mtag) = read_stamp_mtag(debugger,tptr)
        if (stamp):
            class_ = global_kinds[stamp]
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
        cons = Cons_O(untag_cons(tptr))
        debugger.print_("It's a cons")
    debugger.print_("print_tagged_ptr handle: %s\n" % tptr)



