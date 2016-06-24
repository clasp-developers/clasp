import sys
import re
import os
import glob
import StringIO


classesFileName = sys.argv[1]
directories = sys.argv[2:]
fileNames = []
for dir in directories:
    fileNames.extend(glob.glob("%s/configure_symbol_scrape.py" % dir))
    fileNames.extend(glob.glob("%s/include/*Package.h" % dir))
    fileNames.extend(glob.glob("%s/include/*Package.h" % dir))
    fileNames.extend(glob.glob("%s/include/*.h" % dir))
    fileNames.extend(glob.glob("%s/*.cc" % dir))


# keep track of how classes are defined in C++ and make sure it matches LISP_CLASS definitions

CurrentPackage = None
CurrentNamespaceStack = []
NamespaceForPackage = { None: "::", }
PackageForNamespace = { "": None }
cppClassDefinitionBases = {}

def enum(**enums):
    return type('Enum', (), enums)

ClassType = enum(simple=1,template=2,specializer=3,templateOnTemplate=4)

#
# Topological sort
#
## {{{ http://code.activestate.com/recipes/577413/ (r1)
try:
    from functools import reduce
except:
    pass

sampleTopologicalSortData = {
    'des_system_lib':   set('std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee'.split()),
    'dw01':             set('ieee dw01 dware gtech'.split()),
    'dw02':             set('ieee dw02 dware'.split()),
    'dw03':             set('std synopsys dware dw03 dw02 dw01 ieee gtech'.split()),
    'dw04':             set('dw04 ieee dw01 dware gtech'.split()),
    'dw05':             set('dw05 ieee dware'.split()),
    'dw06':             set('dw06 ieee dware'.split()),
    'dw07':             set('ieee dware'.split()),
    'dware':            set('ieee dware'.split()),
    'gtech':            set('ieee gtech'.split()),
    'ramlib':           set('std ieee'.split()),
    'std_cell_lib':     set('ieee std_cell_lib'.split()),
    'synopsys':         set(),
    }

def toposort2(data):
    for k, v in data.items():
        v.discard(k) # Ignore self dependencies
    extra_items_in_deps = reduce(set.union, data.values()) - set(data.keys())

#new code
    for item in extra_items_in_deps:
        data.update({item:set()})
# original code
#    data.update({item:set() for item in extra_items_in_deps})
#----END
#    print("Data = %s\n" % data)

    while True:
#new code
        orderednew = set()
        for item,dep in data.items():
            if not dep:
                orderednew.add(item)
        ordered = orderednew
#original code
#        ordered = set(item for item,dep in data.items() if not dep)
#----END
#        print("orderednew = %s\n" % orderednew)
#        print("ordered  = %s\n" % ordered )
#        print("   SAME ordered = %d\n" % (orderednew == ordered))

        if not ordered:
            break
        yield sorted(ordered)


#new code
        datanew = {}
        for item,dep in data.items():
            if item not in ordered:
                datanew.update({item:(dep-ordered)})
        data = datanew
#original code
#        data = {item: (dep - ordered) for item,dep in data.items()
#                if item not in ordered}
#---END
#        print("datanew = %s\n" % datanew)
#        print("data    = %s\n" % data)
#        print("   SAME data = %d\n" % (datanew == data))

    assert not data, "A cyclic dependency exists amongst %r" % data

## end of http://code.activestate.com/recipes/577413/ }}}





#
# Handle namespaces and packages
#

def currentPackage():
    return CurrentPackage

def clearNamespaceStack():
    global CurrentNamespaceStack
    CurrentNamespaceStack = []

#
# Without a lot more complicated C++ parsing I can't get
# namespace scope so I will only support one level of namespace
#
def pushNamespace(s,fn,ln):
    global CurrentPackage, CurrentNamespaceStack, PackageForNamespace
    sys.stderr.write("Namespace changed to %s - %s:%d\n" % (s,fn,ln))
    CurrentNamespaceStack = [s] # only toplevel namespaces are supported without
    if ( s in PackageForNamespace ):
        CurrentPackage = PackageForNamespace[s]
    else:
        CurrentPackage = None
    sys.stderr.write("    CurrentPackage set to %s\n" % CurrentPackage )


class Predicate:
    def __init__(self,group,target,requirements,fileName,lineNumber,ignoreMe=False,classType=ClassType.simple,specializeOn=None):
        self._Group = group
        self._Target = target
## Only the first requirement is used!!
## See  https://github.com/drmeister/clasp/wiki/Clasp-developers
        sys.stderr.write( "Group: %s  target: %s   filename: %s\n" % ( group, target, fileName))
        if ( len(requirements) > 0 ):
            self._Requirements = set([ requirements[0] ])  # Only the first item is used!!!!!
        else:
            self._Requirements = set()
        self._FileName = fileName
        self._LineNumber = lineNumber
        self._IgnoreMe = ignoreMe
        self._ClassType = classType
        self._SpecializeOn = specializeOn

    def getFileName(self):
        return self._FileName

    def ignore(self):
        return self._IgnoreMe

    def getTarget(self):
        return self._Target

    def classType(self):
        return self._ClassType

    def specializeOn(self):
        return self._SpecializeOn
        
    def getRequirements(self):
        return self._Requirements


#
# Define a function to call to initialize a class or other data structures
# that must be called after aBaseInitializer
class	OneClass(Predicate):
    def __init__(self,group,aPackage,className,baseNames,fn,ln,ignoreMe=False,classType=ClassType.simple,specializeOn=None,directoryPackageName=None):
	if __debug__:
	    sys.stderr.write("Creating one Initializer(%s) baseInitializer(%s)\n"%(className,baseNames))
        if ( className == "" ):
            raise Exception("Empty className at %s:%d" % (fn,ln))
        Predicate.__init__(self,group,className,baseNames,fn,ln,ignoreMe,classType,specializeOn)
	self._Package = aPackage
        self._MetaClassName = "core::BuiltInClass"
        self._DirectoryPackageName = directoryPackageName

    def getDirectoryPackageName(self):
        return self._DirectoryPackageName
    
    def getNamespace(self):
        return self.getTarget().split(":")[0]

    def getClassName(self):
        return self.getTarget()

    def getClassNameReplaceColons(self):
        """Replace all colons with underscores"""
        cn = self.getTarget()
        return cn.replace(":","_")

    
    def getClassSymbolName(self):
        fullName = self.getTarget()
        separator = fullName.find("::")
        nameOnly = fullName[separator+2:]
        return "_sym_%s"%nameOnly
    
    def getBases(self):
        return self.getRequirements()

    def getBasesAsArray(self):
        sys.stderr.write("For class[%s] requirements = %s\n" % (self.getClassName(),repr(self.getRequirements())))
        bases = []
        for  b in self.getRequirements():
            cn = b
            bases.append(cn)
        sys.stderr.write("Returning bases[%s]\n"%repr(bases))
        return bases

    def setMetaClassName(self,metaClassName):
        self._MetaClassName = metaClassName

    def getMetaClassName(self):
        return self._MetaClassName

    def setPackage(self,packageName):
        self._Package = packageName

    def setLispClassName(self,lispClassName):
        self._LispClassName = lispClassName


#    def placeString(self):
#	if self._BaseInitializer == None:
#	    return ""
#	baseName = self.baseInitializerName();
#	if ( not baseName in self._AllInitializers ):
#	    print "Could not find %s (actual: %s) in class dictionary"%(baseName,self._BaseInitializer)
#	    print "Defined in file: %s  lineNumber: %d"%(self._FileName,self._LineNumber)
#	    for k,c in self._AllInitializers.iteritems():
#	        print "    key(%s)   val(%s)"%(k,c)
#	    raise Exception, "Problem in file: %s"%self._FileName
#	str = self._AllInitializers[baseName].placeString()
#	str += "."+self._Initializer
#	return str
#

    def beginIfDefPackage(self,fout):
        if (self._Package == None ):
            sys.stderr.write("Package for class[%s] is None - raising exception\n" % self.getClassName() )
            raise Exception("Package for class[%s] is None" % self.getClassName() )
	fout.write("#ifdef Use_%s\n"%self._Package)
	fout.flush()
        
    def endIfDefPackage(self,fout):
	fout.write("#endif // ifdef Use_%s\n"%self._Package)
	fout.flush()
        
    def declareExtern(self,fout):
        fout.write("#ifdef EXTERN_REGISTER\n")
	fout.write("extern void %s%s(core::Lisp_sp); // base(s): %s\n"%(self._Group.getPrefix(),self.getClassNameReplaceColons(),self.getBases()))
        fout.write("#endif // EXTERN_REGISTER\n")
	fout.flush()
	
    def register(self,fout):
        if ( self.getClassNameReplaceColons() == "" ):
            sys.stderr.write("Bad classname[%s] - raising exception\n" % self.getClassNameReplaceColons() )
            raise Exception("Bad classname[%s] " % self.getClassNameReplaceColons() )
        fout.write("#ifdef INVOKE_REGISTER\n")
        fout.write("{_BLOCK_TRACE(\"initializing %s%s\");\n"%(self._Group.getPrefix(),self.getClassNameReplaceColons()));
	fout.write("    %s::%s%s(_lisp); // base(s): %s\n"%(self.getNamespace(),self._Group.getPrefix(),self.getClassNameReplaceColons(),self.getBases()))
	fout.write("}\n")
        fout.write("#endif // INVOKE_REGISTER\n")
	fout.flush()



    def writeClassMacro(self,fout):
        if (self.classType()==ClassType.specializer):
            fout.write("_SPECIALIZER_CLASS_MACRO(%s)\n" % self.getClassName())
        elif (self.classType()==ClassType.template):
            fout.write("_TEMPLATE_CLASS_MACRO(%s)\n" % self.getClassName())
        else:
            fout.write("_CLASS_MACRO(%s)\n" % self.getClassName() )
	fout.flush()

    def registerPython(self,fout):
        fout.write("extern void Call_exposePython_%s(::core::Lisp_sp lisp);\n" % (self.getClassNameReplaceColons()))
        fout.write("{_DBG(\"exposing to python: %s\");\n"%(self.getClassNameReplaceColons()));
	fout.write("	Call_exposePython_%s(_lisp); // base(s): %s\n"%(self.getClassNameReplaceColons(),self.getBases()))
	fout.write("}\n")
	fout.flush()
	


#
# Define a function to call to initialize a class or other data structures
# that must be called after aBaseInitializer
class	OneInitializer(Predicate):
    def __init__(self,group,functionName,requirements,fn,ln,ignoreMe=False):
	if __debug__:
	    sys.stderr.write( "Creating one Initializer(%s) requirements(%s)\n"%(functionName,requirements))
        Predicate.__init__(self,group,functionName,requirements,fn,ln,ignoreMe)

    def ignore(self):
        return self._IgnoreMe

    def getFunctionName(self):
        return self.getTarget()


    def beginIfDefPackage(self,fout):
        if (self._Package == None ):
            raise Exception("Package for function[%s] is None" % self.getFunctionName() )
	fout.write("#ifdef Use_%s\n"%self._Package)
	fout.flush()
        
    def endIfDefPackage(self,fout):
	fout.write("#endif // ifdef Use_%s\n"%self._Package)
	fout.flush()
        
    def declareExtern(self,fout):
        ns = NamespaceForPackage[self._Package]
	fout.write("extern void %s%s(core::Lisp_sp); // predecessor(s): %s\n"%(self._Group.getPrefix(),self.getFunctionNameReplaceColons(),self.getPredecessors()))
	fout.flush()
	
    def register(self,fout):
        fout.write("{_BLOCK_TRACE(\"initializing %s%s\");\n"%(self._Group.getPrefix(),self.getFunctionNameReplaceColons()));
	fout.write("	%s%s(_lisp); // predecessor(s): %s\n"%(self._Group.getPrefix(),self.getFunctionNameReplaceColons(),self.getPredecessors()))
	fout.write("}\n")
	fout.flush()

    def registerPython(self,fout):
        fout.write("extern void Call_exposePython_%s(::core::Lisp_sp lisp);\n" % (self.getFunctionName()))
        fout.write("{_DBG(\"exposing to python: %s\");\n"%(self.getFunctionName()));
	fout.write("	Call_exposePython_%s(_lisp); // requirements(s): %s\n"%(self.getFunctionName(),self.getRequirements()))
	fout.write("}\n")
	fout.flush()
	





def make_argument_dict(cl):
    return { "CLASSSYMBOL" : cl.getClassSymbolName(),
             "OCLASS" : cl.getClassName(),
             "CLASSNAME":("class%sval"%cl.getClassNameReplaceColons()),
             "METACLASSNAME": cl.getMetaClassName() }


class	ClassGroup:
    def __init__(self,prefix):
        self._Prefix = prefix
	self._AllPredicates = {}

    def iterInitializers(self):
        return self._AllPredicates.itervalues()

    def getClass(self,x):
        return self._AllPredicates[x]

    def getPrefix(self):
        return self._Prefix

    def createOneClass(self,aPackage,aClassName,baseNames,fn,ln,directoryPackageName=None,ignoreMe=False,classType=ClassType.simple,specializeOn=None):
        if ( not aClassName in self._AllPredicates ):
            one = OneClass(self,aPackage,aClassName,baseNames,fn,ln,ignoreMe,classType,specializeOn,directoryPackageName)
            sys.stderr.write( "createOnClass@%s:%d --> %s\n" % (fn, ln, aClassName))
            self._AllPredicates[aClassName] = one
            self._LatestClass = one
        else:
            sys.stderr.write( "I already have a class for: %s\n" % aClassName )
            self._LatestClass = self._AllPredicates[aClassName]

    def errorIfLispBasesDontMatchLatestClassBases(self,basesList,fileName,lineNumber):
        bases = set(basesList)
        cl = self._LatestClass
        if ( len(bases) != len(cl.getBases()) or (not bases.issubset(cl.getBases())) ):
#            raise Exception("WARNING!!!!!!  In %s:%d for class[%s] mismatch between the specification of LISP_BASES(%s) and the c++ bases[%s]" % (fileName,lineNumber,cl.getClassName(),bases,cl.getBases()))
            sys.stderr.write( "WARNING!!!!!!  In %s:%d for class[%s] mismatch between the specification of LISP_BASES(%s) and the c++ bases[%s]\n" % (fileName,lineNumber,cl.getClassName(),bases,cl.getBases()))
                            
    def errorIfClassNameDoesntMatchLatestClass(self,className):
        cl = self._LatestClass
        if ( className != cl.getClassName() ):
            raise Exception("For class[%s] mismatch between the LISP_CLASS name(%s) and the c++ class name[%s]" % (cl.getClassName(),className,cl.getClassName()))

    def updateLatestClass(self,packageName,className,lispClassName):
        if ( className != self._LatestClass.getClassName()):
            raise Exception("Bad updated info for class[%s]" % self._LatestClass.getClassName())
        if ( packageName == None ):
            raise Exception("Illegal packageName of None for class[%s]" % self._LatestClass.getClassName())
        sys.stderr.write( "updateLatestClass info for class[%s] packageName[%s]\n" % (className,packageName))
        self._LatestClass.setPackage(packageName)
        self._LatestClass.setLispClassName(lispClassName)

    def updateLatestClassMetaClass(self,metaClassName):
        self._LatestClass.setMetaClassName(metaClassName)


    def addNamespace(self,s):
        """If the name doesn't have a namespace then add the current namespace"""
        global CurrentNamespaceStack
        if ( s in self._AllPredicates ):
            return s
        cp = s.rfind("::")
        if ( cp > 0 ):
            return s
        nsName = ""
        for n in CurrentNamespaceStack:
            nsName = nsName + n + "::"
            nsName = nsName + s
        return nsName

        


    def toposort(self):
        # Perform a topological sort on all of my members
        tree = {}
        sawRequirements = False
        for c in self._AllPredicates.itervalues():
            tree[c.getTarget()] = c.getRequirements()
            sawRequirements = True
        allSorted = []
        if ( sawRequirements ):
            for oneLevel in toposort2(tree):
                for oneClass in oneLevel:
                    allSorted.append(oneClass)
        else:
            for c in self._AllPredicates.itervalues():
                allSorted.append(c.getTarget())
        return allSorted

    def writeCode(self,fout):
        allSorted = self.toposort()
        # write out the sorted initializers
        fout.write("#if defined(EXPOSE_TO_CANDO) || defined(ALL_STAGES)\n")
	for x in allSorted:
            try:
                cl = self.getClass(x)
            except:
                sys.stderr.write("Class[%s] was not defined\n" % x)
                continue
            if ( not cl.ignore() ):
                cl.beginIfDefPackage(fout)
                cl.declareExtern(fout)
                cl.register(fout)
                cl.endIfDefPackage(fout)
        fout.write("#endif // EXPOSE_TO_CANDO\n")
        fout.write("#undef EXPOSE_TO_CANDO\n")
        fout.write("#ifdef EXPOSE_TO_PYTHON\n")
	for x in allSorted:
            try:
                cl = self.getClass(x)
            except:
                sys.stderr.write("Class[%s] was not defined\n" % x)
                continue
            if ( not cl.ignore() ):
                cl.beginIfDefPackage(fout)
                cl.registerPython(fout)
                cl.endIfDefPackage(fout)
        fout.write("#endif // EXPOSE_TO_PYTHON\n")
        fout.write("#undef EXPOSE_TO_PYTHON\n")
        sys.stderr.write( "Writing macros\n")
        fout.write("#if defined(EXPAND_CLASS_MACROS)\n")
	for x in allSorted:
            try:
                cl = self.getClass(x)
            except:
                sys.stderr.write("Class[%s] was not defined\n" % x)
                continue
            if ( not cl.ignore() ):
                cl.writeClassMacro(fout)
        fout.write("#endif // EXPAND_CLASS_MACROS\n")






    def writeHeaderIncludes(self,fout):
        allSorted = self.toposort()
        fout.write("#ifdef HEADER_INCLUDES\n")
        uniqueHeaders = set()
        for x in allSorted:
            try:
                cl = self.getClass(x)
            except:
                sys.stderr.write("Class[%s] was not defined\n" % x)
                continue
            if ( not cl.ignore() ):
                fn = cl.getFileName()
                if ( fn not in uniqueHeaders ):
                    fout.write("""#include "%s"\n""" % os.path.abspath(cl.getFileName()))
                    uniqueHeaders.add(fn)
        fout.write("#endif // HEADER_INCLUDES\n")
        fout.write("#undef HEADER_INCLUDES\n")
        

    def writeHandInitializeCode(self,fout):
        allSorted = self.toposort()
        fout.write("#if defined(SET_SYMBOL) || defined(ALL_STAGES)\n")
        fout.write("// requires LOOKUP_SYMBOL(pkg,symbolName) be defined\n")
        for x in allSorted:
            try:
                cl = self.getClass(x)
            except:
                sys.stderr.write("Class[%s] was not defined\n" % x)
                continue
            if ( not cl.ignore() ):
                fout.write("#ifdef Use_%s\n" % cl.getDirectoryPackageName() )
                fout.write("%(OCLASS)s::___set_static_ClassSymbol(LOOKUP_SYMBOL(%(OCLASS)s::static_packageName(),%(OCLASS)s::static_className()));\n" % {"OCLASS":cl.getClassName()})
                fout.write("#endif\n")
        fout.write("#endif // SET_SYMBOL\n")
        fout.write("#undef SET_SYMBOL\n")
        fout.write("#if defined(CREATE_CLASS) || defined(ALL_STAGES)\n")
        for x in allSorted:
            try:
                cl = self.getClass(x)
            except:
                sys.stderr.write("Class[%s] was not defined\n" % x)
                continue
            args = make_argument_dict(cl)
            if ( not cl.ignore() ):
                fout.write("#ifdef Use_%s\n" % cl.getDirectoryPackageName() )
                fout.write("""    LOG(BF("Creating class[%(CLASSNAME)s]"));
    %(METACLASSNAME)s_sp %(CLASSNAME)s = %(METACLASSNAME)s_O::createUncollectable();
    %(CLASSNAME)s->__setup_stage1_with_sharedPtr_lisp_sid(%(CLASSNAME)s,_lisp,%(OCLASS)s::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<%(OCLASS)s>::id,%(OCLASS)s::static_classSymbol());
    %(OCLASS)s::___staticClass = %(CLASSNAME)s;
#ifdef USE_MPS
    %(OCLASS)s::static_Kind = gctools::GCKind<%(OCLASS)s>::Kind;
#endif
    core::core__setf_find_class(%(CLASSNAME)s,%(OCLASS)s::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        gctools::tagged_pointer<core::LispObjectCreator<%(OCLASS)s>> cb = gctools::ClassAllocator<core::LispObjectCreator<%(OCLASS)s>>::allocateClass();
        %(OCLASS)s::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%%s) to %%X")%% %(OCLASS)s::static_className() %% (void*)(%(OCLASS)s::static_allocator) );
    %(CLASSNAME)s->setCreator(%(OCLASS)s::static_creator);
    {
        LOG(BF("Created nil for class[%%s]") %% %(OCLASS)s::static_className() );
    }
    /* ----- the class and its nil are now defined and so is %(CLASSNAME)s::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */
""" % args)
                fout.write("#endif\n")

        fout.write("#endif // CREATE_CLASS\n")
        fout.write("#undef CREATE_CLASS\n")
        fout.write("#ifdef DUMP_INFO_CLASS // {\n")
        fout.write("// Depends on nothing\n")
        for x in allSorted:
            try:
                cl = self.getClass(x)
            except:
                sys.stderr.write("Class[%s] was not defined\n" % x)
                continue
            args = make_argument_dict(cl)
            if ( not cl.ignore() ):
                fout.write("#ifdef Use_%s\n" % cl.getDirectoryPackageName() )
                fout.write("""
    LOG(BF("---    dump_info   --- className: %(OCLASS)s @ %%X") %% %(CLASSNAME)s.get());
    LOG(BF("%%s::static_classSymbol() = %%d") %% %(OCLASS)s::static_className() %% %(OCLASS)s::static_classSymbol() );
""" % args )
                fout.write("#endif\n")
        fout.write("#endif // } DUMP_INFO_CLASS\n")
        fout.write("#undef DUMP_INFO_CLASS\n")

        fout.write("#if defined(DEFINE_BASE_CLASSES) || defined(ALL_STAGES) // {\n")
        fout.write("// Depends on nothing\n")
        for x in allSorted:
            try:
                cl = self.getClass(x)
            except:
                sys.stderr.write("Class[%s] was not defined\n" % x)
                continue
            bases = cl.getBasesAsArray()
            if (len(bases)>0 and (bases[0] != "core::_RootDummyClass") ):
                args = make_argument_dict(cl)
                args["BASE1"] = ("%s::static_classSymbol()"%bases[0])
                if ( len(bases)>1 ):
                    args["BASE2"] = ("%s::static_classSymbol()"%bases[1])
                if ( not cl.ignore() ):
                    fout.write("#ifdef Use_%s\n" % cl.getDirectoryPackageName() )
                    fout.write("%(CLASSNAME)s->addInstanceBaseClassDoNotCalculateClassPrecedenceList(%(BASE1)s);\n" % args)
#### This is where I ignore the second base class if it is provided!
#### Only the first class will be defined using addInstanceBaseClass...
#### See https://github.com/drmeister/clasp/wiki/Clasp-developers
#                    if ( len(cl.getBases())> 1):
#                        fout.write("%(CLASSNAME)s->addInstanceBaseClassDoNotCalculateClassPrecedenceList(%(BASE2)s);\n" % args)
                    fout.write("#endif\n")
        fout.write("#endif // } DEFINE_BASE_CLASSES\n")
        fout.write("#undef DEFINE_BASE_CLASSES\n")

        fout.write("#if defined(DEFINE_CLASS_NAMES) || defined(ALL_STAGES) // {\n")
#        fout.write(" core::T_sp _curPkg = _lisp->findPackage(CurrentPkg,true);\n")
        fout.write("// Depends on nothing\n")
        for x in allSorted:
            try:
                cl = self.getClass(x)
            except:
                sys.stderr.write("Class[%s] was not defined\n" % x)
                continue
            args = make_argument_dict(cl)
            if ( not cl.ignore() ):
                fout.write("#ifdef Use_%s\n" % cl.getDirectoryPackageName() )
                fout.write("""
    %(CLASSNAME)s->__setupStage3NameAndCalculateClassPrecedenceList(%(OCLASS)s::static_classSymbol());
""" % args)
                fout.write("#endif\n")
        fout.write("#endif // } DEFINE_CLASS_NAMES\n")
        fout.write("#undef DEFINE_CLASS_NAMES\n")










class	InitializerGroup:
    def __init__(self,prefix):
        self._Prefix = prefix
	self._AllPredicates = {}

    def iterInitializers(self):
        return self._AllPredicates.itervalues()

    def getPrefix(self):
        return self._Prefix

    def getFunction(self,x):
        return self._AllPredicates[x]


    def createOneFunction(self,aFunctionName,requirements,fn,ln,ignoreMe=False):
	one = OneInitializer(self,aFunctionName,requirements,fn,ln,ignoreMe)
        sys.stderr.write( "createOnFunction@%s:%d --> %s\n" % (fn, ln, aFunctionName))
	self._AllPredicates[aFunctionName] = one



    def addNamespace(self,s):
        """If the name doesn't have a namespace then add the current namespace"""
        global CurrentNamespaceStack
        if ( s in self._AllPredicates ):
            return s
        cp = s.rfind("::")
        if ( cp > 0 ):
            return s
        nsName = ""
        for n in CurrentNamespaceStack:
            nsName = nsName + n + "::"
            nsName = nsName + s
        return nsName

        


    def writeCode(self,fout):
        # Perform a topological sort on all of my members
        tree = {}
        sawRequirements = False
        for c in self._AllPredicates.itervalues():
            tree[c.getTarget()] = c.getRequirements()
            sawRequirements = True
        allSorted = []
        if ( sawRequirements ):
            for oneLevel in toposort2(tree):
                for oneClass in oneLevel:
                    allSorted.append(oneClass)
        else:
            for c in self._AllPredicates.itervalues():
                allSorted.append(c.getTarget())

        # write out the sorted initializers
        fout.write("#ifdef EXPOSE_TO_PYTHON\n")
	for x in allSorted:
	    cl = self.getFunction(x)
            if ( not cl.ignore() ):
                cl.registerPython(fout)
        fout.write("#endif // EXPOSE_TO_PYTHON\n")











configure_symbol_scrape_re = re.compile('^packageName\s*=\s*"([\w]*)"')
namespacePackageAssociation = re.compile('^NAMESPACE_PACKAGE_ASSOCIATION\(\s*([\w]*)\s*,\s*([\w]*)\s*,\s*("[\w\-]*")\s*\)')
namespaceDeclaration = re.compile('^\s*namespace\s*([\w]*)')
namespaceSetInIncFiles = re.compile('.*set namespace to ([\w]*)')
lispOtherPackage = re.compile('LISP_OTHER_PACKAGE\(\s*([\w:]*)\s*,\s*([\w:]*)\s*,\s*([\w:]*)\s*\)')
lispTemplateBase1 = re.compile('\s*LISP_TEMPLATE_BASE1\(\s*([\w:_]*)<([\w_]*)>\s*\)')
lispBase1 = re.compile('\s*LISP_BASE1\(\s*([\w:]*)\s*\)')
lispBase2 = re.compile('\s*LISP_BASE2\(\s*([\w:]*)\s*,\s*([\w:]*)\s*\)')
lispMetaClass = re.compile('\s*LISP_META_CLASS\(\s*([\w_:]*)\s*\);')
lispClass = re.compile('\s*LISP_CLASS\(\s*[\w]*\s*,\s*([\w]*)\s*,\s*([\w:]*)\s*,\s*("[\w-]*")\s*\)')
lispVirtualClass = re.compile('\s*LISP_VIRTUAL_CLASS\(\s*[\w]*\s*,\s*([\w]*)\s*,\s*([\w:]*)\s*,\s*("[\w-]*")\s*\)')
cppClassDefinition = re.compile('^\s*class\s*([\w]*_O)\s*:\s*public\s*([\w:]*)')
cppTemplateClassDefinition = re.compile('^\s*template\s*<\s*class\s*\w*>\s*class\s*([\w]*_O)\s*:\s*public\s*([\w:]*)')
cppClassDefinition_templateBase = re.compile('^\s*class\s*([\w]*_O)\s*:\s*public\s*([\w_]*)<([\w_]*)>')
cppTemplateClassDefinition_templateBase = re.compile('^\s*template\s*<\s*class\s*\w*>\s*class\s*([\w]*_O)\s*:\s*public\s*([\w:]*)<([\w_]*)>')
cppClassDefinitionVirtualBase = re.compile('^\s*class\s*([\w]*_O)\s*:\s*virtual\s*public\s*([\w:]*)')
cppClassDefinition_2Bases = re.compile('^\s*class\s*([\w]*_O)\s*:\s*public\s*([\w:]*)\s*,\s*public\s*([\w:]*)')
#put more cppclass definitions here
externalClassDef = re.compile('^\s*LISP_EXTERNAL_CLASS\(\s*[\w]*\s*,\s*([\w]*)\s*,\s*([\w\:<>])*\s*,\s*([\w_]*)\s*,\s*([\w]*)\s*,\s*([\w\:]*)\s*\)')
initPythonDef = re.compile('\s*__INITIALIZE_PYTHON\(\s*([\w]*)\s*\)')
initPythonAfter1Def = re.compile('\s*__INITIALIZE_PYTHON_AFTER1\(\s*([\w:]*)\s*,\s*(\w*)\s*\)')
initDef = re.compile('\s*__INITIALIZE\(\s*([\w]*)\s*,\s*([\w:]*)\s*\)')
#initAfterDef = re.compile('\s*__INITIALIZE_AFTER\(\s*([\w:]*)\s*,\s*(\w*)\s*\)')

fout = StringIO.StringIO()

fout.write("// start\n" )
fout.write("// define cpp macros: SET_SYMBOL, CREATE_CLASS, SET_CLASS, DEFINE_BASE_CLASSES, DEFINE_CLASS_NAMES, EXPOSE_TO_CANDO \n")
fout.write("// define cpp macro: ALL_STAGES to get the effect of defining all of the macros above\n")
fout.write("// define cpp macro: EXPOSE_PYTHON to expose python\n")
fout.flush()


classInitializers = ClassGroup("Register_")
pythonInitializers = InitializerGroup("")
afterInitializers = InitializerGroup("")

#classInitializers.createOneClass("","_RootDummyClass",[],"core/object.h",0,True)
#classInitializers.createOneClass("CorePkg","core::T_O",["_RootDummyClass"],"core/object.h",0,False)
pushNamespace("core","-nofile-",0)
#classInitializers.createOneClass("CorePkg","core::Model_O",["core::T_O"],"core/model.h",0,False)
#classInitializers.createOneClass("CorePkg","core::Iterator_O",["core::T_O"],"core/iterator.h",0,False)
#classInitializers.createOneClass("CorePkg","core::StandardObject_O",["core::T_O"],"core/standardObject.h",0,False)
#classInitializers.createOneClass("CorePkg","core::ExternalObject_O",["core::T_O"],"core/object.h",0,False)


for fileName in fileNames:
    sys.stderr.write( "!!!!! Reading fileName(%s)\n" % fileName )
    clearNamespaceStack()
    if (not (os.path.exists(fileName)) and ("*" in fileName) ) :
        sys.stderr.write("Skipping file[%s] - it doesn't exist\n" % fileName)
        continue
    fin = open(fileName,"r")
    ln = 0
    for l in fin.readlines():
	ln += 1
	line = l.strip().rstrip()
#        print( "READ: %s" % line)
# Check for new way of defining LISP classes
        match = namespacePackageAssociation.match(l)
        if ( match != None ):
	    gr = match.groups()
	    namespaceName = gr[0]
	    packageName = gr[1]
#            sys.stderr.write( "!!!!! Associating namespace(%s) with package(%s)\n" % (namespaceName,packageName))
#            fout.write("// Associating namespace(%s) with package(%s)\n" % (namespaceName,packageName))
            if ( packageName in NamespaceForPackage ):
                if ( namespaceName != NamespaceForPackage[packageName] ):
                    raise Exception("At %s:%d you are redefining a namespace/package association with a different association - this should never happen")
            NamespaceForPackage[packageName] = namespaceName
            PackageForNamespace[namespaceName] = packageName
	    continue

        match = configure_symbol_scrape_re.match(l)
        if ( match != None ):
            gr = match.groups()
            directoryPackageName = gr[0]
#            sys.stderr.write( "!!!!! Switched directoryPackageName = %s\n" % directoryPackageName)

        match = namespaceDeclaration.match(l)
        if ( match != None ):
            gr = match.groups()
            pushNamespace(gr[0],fileName,ln)

        match = namespaceSetInIncFiles.match(l)
        if ( match != None ):
            gr = match.groups()
            sys.stderr.write("Setting namespace[%s] in .inc file[%s:%s]\n" % (gr[0],fileName,ln))
            pushNamespace(gr[0],fileName,ln)


        match = cppClassDefinition_2Bases.match(l)
        if ( match!=None):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed cppClassDefinition_2Bases line: %s\n"%(fileName,line))
            fout.write(" //%s\n" % line)
            gr = match.groups()
            className = classInitializers.addNamespace(gr[0])
            base1ClassName = classInitializers.addNamespace(gr[1])
            base2ClassName = classInitializers.addNamespace(gr[2])
            classInitializers.createOneClass(currentPackage(),className,[base1ClassName,base2ClassName],fileName,ln,directoryPackageName=directoryPackageName)
            continue

        match = cppClassDefinitionVirtualBase.match(l)
	if ( match!= None ):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed cppClassDefinitionVirtualBase line: %s\n"%(fileName,line))
	    fout.write(" // %s\n"%line)
	    gr = match.groups()
	    className = classInitializers.addNamespace(gr[0])
	    baseClassName = classInitializers.addNamespace(gr[1])
            classInitializers.createOneClass(currentPackage(),className,[baseClassName],fileName,ln,directoryPackageName=directoryPackageName)
	    continue

        match = cppTemplateClassDefinition_templateBase.match(l)
	if ( match!= None ):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed cppTemplateClassDefinition_template_ line: %s\n"%(fileName,line))
	    fout.write(" // %s\n"%line)
	    gr = match.groups()
	    className = classInitializers.addNamespace(gr[0])
	    baseClassName = classInitializers.addNamespace(gr[1])
            specializeOn = classInitializers.addNamespace(gr[2])
            if ( className == "" ):
                raise Exception("%s:%s No namespace was defined for class[%s] - if you are including a .inc file then add a comment to set the namespace for all of the definitions" % (fileName,ln,gr[0]))
            pkg = currentPackage()
            classInitializers.createOneClass(pkg,className,["%s<%s>"%(baseClassName,specializeOn)],fileName,ln,ignoreMe=False,classType=ClassType.templateOnTemplate,specializeOn=specializeOn,directoryPackageName=directoryPackageName)
	    continue


        match = cppTemplateClassDefinition.match(l)
	if ( match!= None ):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed cppTemplateClassDefinition_template_ line: %s\n"%(fileName,line))
	    fout.write(" // %s\n"%line)
	    gr = match.groups()
	    className = classInitializers.addNamespace(gr[0])
	    baseClassName = classInitializers.addNamespace(gr[1])
            if ( className == "" ):
                raise Exception("%s:%s No namespace was defined for class[%s] - if you are including a .inc file then add a comment to set the namespace for all of the definitions" % (fileName,ln,gr[0]))
            pkg = currentPackage()
            classInitializers.createOneClass(pkg,className,[baseClassName],fileName,ln,ignoreMe=False,classType=ClassType.template,directoryPackageName=directoryPackageName)
	    continue

        match = cppClassDefinition_templateBase.match(l)
	if ( match!= None ):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed cppClassDefinition_templateBase line: %s\n"%(fileName,line))
	    fout.write(" // %s\n"%line)
	    gr = match.groups()
	    className = classInitializers.addNamespace(gr[0])
	    baseClassName = classInitializers.addNamespace(gr[1])
            specializeOn = classInitializers.addNamespace(gr[2])
            if ( className == "" ):
                raise Exception("%s:%s No namespace was defined for class[%s] - if you are including a .inc file then add a comment to set the namespace for all of the definitions" % (fileName,ln,gr[0]))
            pkg = currentPackage()
            classInitializers.createOneClass(pkg,className,["%s<%s>"%(baseClassName,specializeOn)],fileName,ln,ignoreMe=False,directoryPackageName=directoryPackageName)
	    continue

        match = cppClassDefinition.match(l)
	if ( match!= None ):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed cppClassDefinition line: %s\n"%(fileName,line))
	    fout.write(" // %s\n"%line)
	    gr = match.groups()
	    className = classInitializers.addNamespace(gr[0])
	    baseClassName = classInitializers.addNamespace(gr[1])
            if ( className == "" ):
                raise Exception("%s:%s No namespace was defined for class[%s] - if you are including a .inc file then add a comment to set the namespace for all of the definitions" % (fileName,ln,gr[0]))
            pkg = currentPackage()
            classInitializers.createOneClass(pkg,className,[baseClassName],fileName,ln,directoryPackageName=directoryPackageName)
	    continue




        # Sometimes we create classes that have a base class in a previously defined package
        # Use LISP_OTHER_PACKAGE to define them
        # eg: LISP_OTHER_PACKAGE(CorePkg,core::HighlightedObject_O,T_O) to define them
        # just make sure that you include the LISP_OTHER_PACKAGE declarations before the
        # other package classes are referenced
        # Do this by creating a "otherPackageClasses.h" in each package and putting the LISP_OTHER_PACKAGE
        # declarations in there and "otherPackageClasses.h" will be read by registerClasses.py before
        # any others
        match = lispOtherPackage.match(l)
	if ( match!= None ):
	    fout.write(" // %s\n"%line)
	    gr = match.groups()
            packageName = gr[0]
	    className = gr[1]
	    baseClassName = gr[2]
            classInitializers.createOneClass(packageName,className,[baseClassName],fileName,ln,ignoreMe=True,directoryPackageName=directoryPackageName)
	    continue

        match = lispTemplateBase1.match(l)
        if ( match != None ):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed lispTemplateBase1 line: %s\n"%(fileName,line))
            gr = match.groups()
            base1ClassName = classInitializers.addNamespace(gr[0])
            specializedOn = classInitializers.addNamespace(gr[1])
            classInitializers.errorIfLispBasesDontMatchLatestClassBases(["%s<%s>"%(base1ClassName,specializedOn)],fileName,ln)
            



        match = lispBase1.match(l)
        if ( match != None ):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed lispBase1 line: %s\n"%(fileName,line))
            gr = match.groups()
            base1ClassName = classInitializers.addNamespace(gr[0])
            classInitializers.errorIfLispBasesDontMatchLatestClassBases([base1ClassName],fileName,ln)


        match = lispBase2.match(l)
        if ( match != None ):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed lispBase2 line: %s\n"%(fileName,line))
            gr = match.groups()
            base1ClassName = classInitializers.addNamespace(gr[0])
            base2ClassName = classInitializers.addNamespace(gr[1])
            classInitializers.errorIfLispBasesDontMatchLatestClassBases([base1ClassName,base2ClassName],fileName,ln)

                   
        match = lispClass.match(l)
        if ( match != None ):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed lispClass line: %s\n"%(fileName,line))
	    gr = match.groups()
            packageName = gr[0]
            className = classInitializers.addNamespace(gr[1])
            lispClassName = gr[2]
            classInitializers.errorIfClassNameDoesntMatchLatestClass(className)
            classInitializers.updateLatestClass(packageName,className,lispClassName)
            
        match = lispVirtualClass.match(l)
        if ( match != None ):
	    if __debug__:
	        sys.stderr.write( "In fileName: %s parsed lispVirtualClass line: %s\n"%(fileName,line))
	    gr = match.groups()
            packageName = gr[0]
            className = classInitializers.addNamespace(gr[1])
            lispClassName = gr[2]
            classInitializers.errorIfClassNameDoesntMatchLatestClass(className)
            classInitializers.updateLatestClass(packageName,className,lispClassName)
            
            
#	    classInitializers.updateClassName(className)
	    continue


        match = lispMetaClass.match(l)
        if ( match!=None):
            if __debug__:
                sys.stderr.write( "Got lispMetaClass line: %s\n" % line)
	    gr = match.groups()
            sys.stderr.write("lispMetaClass gr=%s\n" % repr(gr))
            metaClassName = gr[0]
            sys.stderr.write("Found metaClassName[%s]\n" % metaClassName)
            classInitializers.updateLatestClassMetaClass(metaClassName)
            continue
        

	match = externalClassDef.match(l)
	if ( match!= None ):
	    if __debug__:
	        sys.stderr.write( "Got line: %s\n"%line)
	    gr = match.groups()
	    packageName = gr[0]
	    className = classInitializers.addNamespace(gr[2])
	    baseClassName = gr[4]
            sys.stderr.write("EXTERNAL: %s - %s - %s\n" % (packageName,className,baseClassName))
	    o = classInitializers.createOneClass(packageName,className,[baseClassName],fileName,ln,False,directoryPackageName=directoryPackageName)
	    continue


        match = initPythonDef.match(l)
        if ( match != None ):
            sys.stderr.write( "Got initPythonDef: %s\n" % l)
            gr = match.groups()
            functionName = gr[0]
            pythonInitializers.createOneFunction(functionName,[],fileName,ln)
            continue

        match = initPythonAfter1Def.match(l)
        if ( match != None ):
            sys.stderr.write( "Got initPythonAfter1Def: %s\n" % l)
            gr = match.groups()
            functionName = gr[0]
            predecessor1 = gr[1]
            pythonInitializers.createOneFunction(functionName,[predecessor1],fileName,ln)
            continue



    fin.close()


sys.stderr.write("-------- loaded everything -------- now processing\n")


#
# Perform a topological sort on the classe
#




sys.stderr.write( "============= Writing classInitializers\n")
classInitializers.writeHeaderIncludes(fout)
classInitializers.writeHandInitializeCode(fout)
classInitializers.writeCode(fout)
fout.write("#undef ALL_STAGES\n")
sys.stderr.write( "------------- Done classInitializers\n")




fout.close



fout.write("// ---------------- after class initializers\n")
sys.stderr.write( "===============  afterInitializers\n")
afterInitializers.writeCode(fout)
newResult = fout.getvalue()
fout.close()

try:
    fin = open(classesFileName,"r")
    previousResult = fin.read()
    fin.close()
except IOError:
    previousResult = ""

if ( previousResult != newResult ):
    fout = open(classesFileName,"w")
    fout.write(newResult)
    fout.close
    sys.stdout.write( "Wrote new output to: %s\n" % classesFileName)
else:
    sys.stdout.write( "Classes haven't changed - no output written\n")

#open(classesFileName,"w")
