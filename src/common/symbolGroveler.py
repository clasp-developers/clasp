import glob
import sys
import os.path
from os import getcwd

import StringIO
import re
from string import maketrans

NamespaceForPackage = { None: "::", }
PackageForNamespace = { "": None }




def uniquePackages(symbols):
    unique = set()
    for x in symbols:
        unique.add(x.package())
    return unique



def accumulate_symbols(symbols,lambda_list):
    mode = "required"
    for x in lambda_list:
        if ( x.__class__ == Symbol ):
            if ( x.lispName()[0] == '&' ):
                if ( x.lispName() == "&optional" ):
                    mode = "optional"
                    continue
                elif ( x.lispName() == "&rest"):
                    mode = "rest"
                    continue
                elif ( x.lispName() == "&va-rest"):
                    mode = "va-rest"
                    continue
                elif ( x.lispName() == "&body"):
                    mode = "rest"
                    continue
                elif ( x.lispName() == "&key"):
                    mode = "key"
                    continue
                elif ( x.lispName() == "&allow-other-keys" ):
                    continue
                elif ( x.lispName() == "&aux" ):
                    m
                else:
                    raise Exception, "Unknown amp symbol[%s]" % x.lispName() 
            else:
                symbols.append(x)
        elif (x.__class__ == list ):
            accumulate_symbols(symbols,x)
        else:
            continue

        
def dump_symbol_list(symbols):
    sys.stdout.write("    symbols----> ")
    for x in symbols:
        sys.stdout.write(x.lispName())
        sys.stdout.write(" ")
    sys.stdout.write("\n")

def tokenize(s):
    "Convert a string into a list of tokens."
    return s.replace('(',' ( ').replace(')',' ) ').split()

def read_from(tokens):
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        L = []
        while tokens[0] != ')':
            L.append(read_from(tokens))
        tokens.pop(0) # pop off ')'
        return L
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:
        return atom(token)

def atom(token):
    "Numbers become numbers; every other token is a symbol."
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            return Symbol("CurrentPkg","lambda-list",-1,name=toCName(token),lispName=toLispName(token))

def parse_lambda_list_from_string(s):
    tokens = tokenize(s)
    try:
        l = read_from(tokens)
    except IndexError:
        raise SyntaxError("Hit a syntax error in: %s" % s)
        
    return l



existingSymbols = {}
existingExportedSymbols = set()   # a set of exported symbols read from the symbol table
scrapedSymbols = {}
symbolTable = []


def dump_symbolTable(fileName):
    fout = open(fileName,"w")
    fout.write("// symbolTable\n" )
    fout.flush()
    for s in symbolTable:
        fout.write("%s\n" % s.symbol_table_repr())
    fout.close



def resize_symbolTable( newsize, filling=None):
    global symbolTable
    if newsize > len(symbolTable):
        symbolTable.extend([filling for x in xrange(len(symbolTable), newsize)])
    else:
        del symbolTable[newsize:]

def addExistingSymbol(s):
    global existingSymbols
    if (s.fullName() in existingSymbols):
        s.mergeSymbol(existingSymbols[s.fullName()])
    existingSymbols[s.fullName()] = s



#
# Add the scraped symbol to the scraped symbols dictionary if its new
# if the scraped symbol is exported then merge that with the
# scrapedSymbols dictionary entry definition
def addScrapedSymbol(s):
    global scrapedSymbols
    if (s.fullName() in scrapedSymbols):
        existing = scrapedSymbols[s.fullName()]
#        if ( s.export() ):
#            existing.setExport(True)
        return
    scrapedSymbols[s.fullName()] = s


# Put the symbol in the first empty slot in the symbol table or append it
def add_to_symbolTable(s):
    global symbolTable
    for i in range(len(symbolTable)):
        entry = symbolTable[i]
        if ( entry == None ):
            s.set_rsid(i)
            symbolTable[i] = s
            return
    s.set_rsid(len(symbolTable))
    symbolTable.append(s)
    

def buildSymbolTable():
    global symbolTable, existingSymbols, scrapedSymbols
    setOfExistingSymbols = set(existingSymbols.iterkeys())
    setOfScrapedSymbols = set(scrapedSymbols.iterkeys())
    print("There are %d existing symbols"% len(setOfExistingSymbols) )
    print("There are %d scraped symbols" % len(setOfScrapedSymbols) )
    setOfRemovedSymbols = setOfExistingSymbols.difference(setOfScrapedSymbols)
    print("There are %d removed symbols" % len(setOfRemovedSymbols))
    setOfAddedSymbols = setOfScrapedSymbols.difference(setOfExistingSymbols)
    print("There are %d added symbols" % len(setOfAddedSymbols))
    symbolTableSymbols = set()
    symbolTable = []
#    for s in existingSymbols.itervalues():
#        if ( s.cName() not in setOfRemovedSymbols ):
#            s.set_rsid(len(symbolTable))
#            symbolTable.append(s)
#            symbolTableSymbols.add(s.cName())
    for s in scrapedSymbols.itervalues():
        if ( s.fullName() not in symbolTableSymbols ):
            s.set_rsid(len(symbolTable))
            symbolTable.append(s)
            symbolTableSymbols.add(s.fullName())
    return (setOfAddedSymbols,setOfRemovedSymbols)


            





def pkg(nonKwPkgName,sym):
    if ( sym.isKeyword() ):
        return "KeywordPkg"
    else:
        return nonKwPkgName

    
class SymbolBase:
    def __init__(self,pkgName,bareName,cName,lispName,fn,ln,export=False,rsid=None):
        self._packageName = pkgName
        self._bareName = bareName
        self._cName = cName
        if ( lispName == "" ):
            raise Exception, "Missing lispName[%s] for cName[%s]" % ( lispName, cName )
        self._lispName = lispName
        self._fileName = fn
        self._lineNumber = ln
        self._isExported = export
        self._rsid = rsid

    def mergeSymbol(self,other):
        if ( self.packageName != other.packageName ):
            raise Exception("Merging symbol %s mismatch in packageName")
        if ( self._bareName != other._bareName ):
            raise Exception("Merging symbol %s mismatch in bareName")
        if ( self._cName != other._cName ):
            raise Exception("Merging symbol %s mismatch in cName")
        if ( self._lispName != other._lispName ):
            raise Exception("Merging symbol %s mismatch in lispName")
        if ( self._isExported or other._isExported ):
            self._isExported = true

    def fullName(self):
        return "%s::%s"%(self._packageName,self._bareName)

    def package(self):
        return self._packageName

    def bareName(self):
        return self._bareName

    def cName(self):
        return self._cName

    def lispName(self):
        return self._lispName

    def setExport(self,v):
        self._isExported = v

    def set_rsid(self,v):
        self._rsid = v

    def rsid(self):
        return self._rsid

    def repr(self):
        return "(%s %s %s:%d %d %s)"%(self.package(),self.cName(),self._fileName,self._lineNumber,self.rsid(), self.export_repr())

    def symbol_table_repr(self):
        return "// SYMBOL_TABLE_ENTRY %12s %4d %-30s %-30s %s ; cName=%s lispName=%s" % (self.package(), self._rsid, self.bareName(), self.lispName(), self.export_repr(), self.cName(), self.lispName())

class Symbol(SymbolBase):
    def __init__(self,fn,ln,package,name=None,lispName=None,rsid=None,export=False):
        if ( lispName == None ):
            lispName = toLispName(name)
#        if ( lispName == None ):
#            lispName = toLispName(name).upper()
#        else:
#            lispName = lispName.upper()
#           print("Read symbol with lisp-name=%s"%gr[2])
        SymbolBase.__init__(self,package,name,toCSymbolName(name),lispName,fn,ln,rsid=rsid,export=export)

    def isKeyword(self):
        if ( self.package() == "KeywordPkg" ):
            return True
        return False

    def export(self):
        if ( self.package() == "KeywordPkg" ):
            return True
        return self._isExported

    def exportString(self):
        if ( self.export() ):
            return "true"
        return "false"

    def export_repr(self):
        if (self.export()):
            exportStr = "export"
        else:
            exportStr = "private"
        return exportStr






lispTranslateTable = maketrans("_","-")
conversions = [
    ("_EQ_","="),
    ("_NE_","/="),
    ("_LT_","<"),
    ("_GT_",">"),
    ("_LE_","<="),
    ("_GE_",">="),
    ("PERCENT","%"),
    ("STAR","*"),
    ("AMP","&"),
    ("DOT","."),
    ("_PLUS_","+"),
    ("_MINUS_","-"),
    ("_TIMES_","*"),
    ("_DIVIDE_","/"),
    ("_SLASH_","/"),
    ("_","-") ]

def toLispName(s):
    return s
    for i in conversions:
        news = s.replace(i[0],i[1])
        s = news
    str = s
    output = StringIO.StringIO()
    for x in range(0,len(str)-1):
        if ( str[x].islower() and str[x+1].isupper() ):
            output.write("%c-" % str[x].upper())
            continue
        output.write("%c" % str[x].upper())
    if ( len(str) > 0 ):
        output.write("%c" % str[-1].upper())
    res = output.getvalue()
    output.close()
    return res

def toCName(lispName):
    dashReplaced = lispName.lower().replace("-","_")
    starsReplaced = dashReplaced.replace("*","STAR")
    ampsReplaced = starsReplaced.replace("&","AMP")
    dotsReplaced = ampsReplaced.replace(".","DOT")
    return dotsReplaced


    # output = StringIO.StringIO()
    # for c in s:
    #     if ( c=='S'):
            
    #         output.write("STAR")
    #     elif ( c=="_"):
    #         output.write("-")
    #     else:
    #         output.write(c)
    # contents = output.getvalue()
    # output.close()
    # return contents
    
            
    return s.translate(lispTranslateTable)

def toCSymbolName(s):
    return '_sym_'+s

def toCKeywordName(s):
    return '_kw_'+s
    
configure_symbol_scrape_re = re.compile('^packageName\s*=\s*"([\w]*)"')
namespacePackageAssociation_re = re.compile('^NAMESPACE_PACKAGE_ASSOCIATION\(\s*([\w]*)\s*,\s*([\w]*)\s*,\s*("[\w\-]*")\s*\)')
symbol_table_re = re.compile('\s*//\s*SYMBOL_TABLE_ENTRY\s*([\w_]*)\s*([\d]*)\s*([\w_]*)\s*([<=>%/&\w_\*-.]*)\s*(export|private)')
bad_existing_symbol_table_entry_re = re.compile('\s*//\s*SYMBOL_TABLE_ENTRY')
# recognize: 	LISP_CLASS(CorePkg,GraphicsObject_O,"GraphicsObject");
lisp_class_re = re.compile('\s*LISP_CLASS\s*\(\s*[\w]*\s*,\s*([\w]*)\s*,\s*([\w_]*)\s*,\s*"([\w_-]*)"\s*\);')
lisp_virtual_class_re = re.compile('\s*LISP_VIRTUAL_CLASS\s*\(\s*[\w]*\s*,\s*([\w]*)\s*,\s*([\w_]*)\s*,\s*"([\w_-]*)"\s*\);')
bad_lisp_class_re = re.compile("\s*(LISP_CLASS|LISP_VIRTUAL_CLASS)")
# recognize:  LISP_EXTERNAL_CLASS(LlvmoPkg,llvm::LLVMContext,LLVMContext_O,"llvm-context",core::ExternalObject_O);
# arguments: (pkgname, externalWrappedPtrClass, classo-name, lisp-name, base-classo-name)
lisp_external_class_re = re.compile('\s*LISP_EXTERNAL_CLASS\s*\(\s*[\w]*\s*,\s*([\w]*)\s*,\s*([\w:_<>]*)\s*,\s*([\w_]*)\s*,\s*"([\w-]*)"\s*,\s*([\w:_]*)\s*\);')
bad_lisp_external_class_re = re.compile("\s*LISP_EXTERNAL_CLASS")
intern_re = re.compile('.*INTERN_\(\s*([\w_]*)\s*,\s*([\w_]*)\s*\).*')
symbol_re = re.compile('\s*SYMBOL_SC_\(\s*([\w_]*)\s*,\s*([\w_]*)\s*\);')
symbol_export_re = re.compile('\s*SYMBOL_EXPORT_SC_\(\s*([\w_]*)\s*,\s*([\w_]*)\s*\);')
symbol_expose_re = re.compile('\s*SYMBOL_EXPOSE_SC_\(')
argument_re = re.compile('\s*ARGUMENT_SC_\(\s*([\w_]*)\s*\);')
argument_export_re = re.compile('\s*ARGUMENT_EXPORT_SC_\(\s*([\w_]*)\s*\);')
defaccessors_re = re.compile('\s*DEFACCESSORS\(\s*([\w_]*)\s*\);')
defaccessors_export_re = re.compile('\s*DEFACCESSORS_EXPORT\(\s*([\w_]*)\s*\);')
defun_defgeneric_re = re.compile('^\s*(DEFGENERIC|DEFUN)\(\s*([\w])*\s*,\s*([\w_]*)\s*\);')
defun_defgeneric_export_re = re.compile('\s*(DEFGENERIC_EXPORT|DEFUN_EXPORT)\(\s*([\w])*\s*,\s*([\w_]*)\s*\);')
defun_defgeneric_name_re = re.compile('^\s*(DEFGENERIC_NAME|DEFUN_NAME)\(\s*([\w])*\s*,\s*([\w_]*)\s*,\s*(".*")\s*\);')
defun_defgeneric_name_export_re = re.compile('\s*(DEFGENERIC_NAME_EXPORT|DEFUN_NAME_EXPORT)\(\s*([\w])*\s*,\s*([\w_]*)\s*,\s*(".*")\s*\);')
# define_args_re = re.compile('^\s*#define\s*ARGS_([\w_]*)\s*"([\s\w*:()&.\\\'\-]*)"$')
define_args_re = re.compile('^\s*#define\s*ARGS_([\w_]*)\s*"(.*)"$')
bad_macros_re = re.compile('^\s*(#define\s*ARGS_|SYMBOL_EXPORT_SC_|SYMBOL_SC_|ARGUMENT_EXPORT_SC_|ARGUMENT_SC_|DEFGENERIC|DEFUN|DEFGENERIC_EXPORT|DEFUN_EXPORT|DEFACCESSORS|DEFACCESSORS_EXPORT)\s*')

symbolsFileName = sys.argv[1]
fileNames = [symbolsFileName]
directories = sys.argv[2:]
for dir in directories:
    fileNames.extend(glob.glob("%s/configure_symbol_scrape.py" % dir))
    fileNames.extend(glob.glob("%s/include/*.h" % dir))
    fileNames.extend(glob.glob("%s/*.cc" % dir))

print("symbols file name = %s" % symbolsFileName)
print("Groveling directories = %s" % directories)
saw_symbol_table = False
for fileName in fileNames:
    if (not os.path.exists(fileName) ):
        print("Skipping file[%s] - it doesn't exist" % fileName)
        continue
    fin = open(fileName,"r")
    ln = 0
    for l in fin.readlines():
	ln += 1
	line = l.strip().rstrip()
#        print( "READ: %s" % line)

        match = configure_symbol_scrape_re.match(l)
        if ( match != None ):
            gr = match.groups()
            packageName = gr[0]
#            sys.stderr.write( "!!!!! Switched packageName = %s\n" % packageName)

        match = symbol_table_re.match(l)
        if ( match != None ):
            saw_symbol_table = True
            gr = match.groups()
            packageName = gr[0]
            symbol_rsid = int(gr[1])
            symbol_name = gr[2]
            lispName = gr[3]
            exported = (gr[4]=="export")
            s = Symbol(fileName,ln,packageName,name=symbol_name,lispName=lispName,rsid=symbol_rsid,export=exported)
#            print("existing symbol[%s]"% s.repr())
            addExistingSymbol(s)
            if ( exported ):
                existingExportedSymbols.add(s.lispName())
            continue

        match = namespacePackageAssociation_re.match(l)
        if ( match != None ):
	    gr = match.groups()
	    namespaceName = gr[0]
	    packageName = gr[1]
#            sys.stderr.write( "!!!!! Associating namespace(%s) with package(%s)\n" % (namespaceName,packageName))
            if ( packageName in NamespaceForPackage ):
                if ( namespaceName != NamespaceForPackage[packageName] ):
                    raise Exception("At %s:%d you are redefining a namespace/package association with a different association - this should never happen")
            NamespaceForPackage[packageName] = namespaceName
            PackageForNamespace[namespaceName] = packageName
	    continue

        match = bad_existing_symbol_table_entry_re.match(l)
        if ( match != None):
            print("BAD existing symbol table match: %s" % l),
            continue

        match = lisp_class_re.match(l)
        if ( match != None ):
            gr = match.groups()
            s = Symbol(fileName,ln,gr[0],name=gr[1],lispName=toLispName(gr[2]),export=True)
            addScrapedSymbol(s)
            continue

        match = lisp_virtual_class_re.match(l)
        if ( match != None ):
            gr = match.groups()
            s = Symbol(fileName,ln,gr[0],name=gr[1],lispName=toLispName(gr[2]),export=True)
            addScrapedSymbol(s)
            continue

        match = bad_lisp_class_re.match(l)
        if ( match != None ):
            print("BAD LISP_CLASS match: %s" % l),
            continue
            



        match = lisp_external_class_re.match(l)
        if ( match != None ):
            gr = match.groups()
            s = Symbol(fileName,ln,gr[0],name=gr[2],lispName=toLispName(gr[3]),export=True)
            addScrapedSymbol(s)
            continue

        match = bad_lisp_external_class_re.match(l)
        if ( match != None ):
            print("BAD LISP_EXTERNAL_CLASS match: %s" % l),
            continue
            

        match = define_args_re.match(l)
        if ( match != None ):
            gr = match.groups()
#            print("%s:%d Matched #define ARGS_ ---> %s" % (fileName,ln, l ))
            if ( len(gr)>0 ):
#                print( "     match[0] = %s" % gr[0])
                pass
            if ( len(gr)>1 ):
                if ( gr[1]== ""):
#                    print("    match[1] = -empty-")
                    l = None
                else:
#                    print( "     match[1] = %s" % gr[1])
                    l = parse_lambda_list_from_string(gr[1])
                    symbols = []
                    accumulate_symbols(symbols,l)
#                    print( "     symbols ---> ")
#                    dump_symbol_list(symbols)
            continue


        match = argument_re.match(l)
        if ( match != None ):
            gr = match.groups()
            s = Symbol(fileName,ln,"CurrentPkg",name=gr[0])
            addScrapedSymbol(s)
            continue

        match = argument_export_re.match(l)
        if ( match != None ):
            gr = match.groups()
            s = Symbol(fileName,ln,"CurrentPkg",name=gr[0],export=True)
            addScrapedSymbol(s)
            continue

        match = intern_re.match(l)
        if ( (match != None) and (l[0:7] != "#define") ):
            gr = match.groups()
            namespace = gr[0]
            if ( namespace in PackageForNamespace ):
                pkgName = PackageForNamespace[namespace]
            else:
                raise Exception("Error!!!! Reading %s - unknown namespace - make sure the NAMESPACE_PACKAGE_ASSOCIATION macro for this namespace (%s) is defined" % (fileName, namespace))
            s = Symbol(fileName,ln,pkgName,name=gr[1],export=False)
            addScrapedSymbol(s)
            continue

        match = symbol_re.match(l)
        if ( match != None ):
            gr = match.groups()
            s = Symbol(fileName,ln,gr[0],name=gr[1],export=False)
            addScrapedSymbol(s)
            continue

        match = symbol_export_re.match(l)
        if ( match != None ):
#            print("!!!!!! Matched: %s", l)
            gr = match.groups()
            s = Symbol(fileName,ln,gr[0],name=gr[1],export=True)
            addScrapedSymbol(s)
            continue

        match = symbol_expose_re.match(l)
        if ( match != None ):
            print("!!!!!! hit SYMBOL_EXPOSE_SC_  !!!!! USE SYMBOL_EXPORT_SC_")
            raise Exception, "!!!!!! hit SYMBOL_EXPOSE_SC_  !!!!! USE SYMBOL_EXPORT_SC_"
            continue


        match = defun_defgeneric_re.match(l)
        if ( match != None ):
            gr = match.groups()
            op = gr[0]
            pkgName = gr[1]
            symbolName = gr[2]
            s = Symbol(fileName,ln,pkgName,name=symbolName)
            addScrapedSymbol(s)
            continue


        match = defun_defgeneric_name_export_re.match(l)
        if ( match != None ):
            gr = match.groups()
            op = gr[0]
            pkgName = gr[1]
            symbolName = gr[2]
            lispName = gr[3][1:-1]
            s = Symbol(fileName,ln,pkgName,name=symbolName,lispName=toLispName(lispName),export=True)
            addScrapedSymbol(s)
            continue



        match = defun_defgeneric_name_re.match(l)
        if ( match != None ):
            gr = match.groups()
            op = gr[0]
            pkgName = gr[1]
            symbolName = gr[2]
            lispName = gr[3][1:-1]
            s = Symbol(fileName,ln,pkgName,name=symbolName,lispName=toLispName(lispName))
            addScrapedSymbol(s)
            continue


        match = defun_defgeneric_export_re.match(l)
        if ( match != None ):
            gr = match.groups()
            op = gr[0]
            pkgName = gr[1]
            symbolName = gr[2]
            s = Symbol(fileName,ln,pkgName,name=symbolName,export=True)
            addScrapedSymbol(s)
            continue





        match = defaccessors_re.match(l)
        if ( match != None ):
            gr = match.groups()
            symbolName = gr[0]
            print("Hit DEFACCESSOR[%s]" % symbolName)
            s = Symbol(fileName,ln,"CurrentPkg",name=symbolName)
            addScrapedSymbol(s)
            s = Symbol(fileName,ln,"CurrentPkg",name="setf_"+symbolName)
            addScrapedSymbol(s)
            continue


        match = defaccessors_export_re.match(l)
        if ( match != None ):
            gr = match.groups()
            symbolName = gr[0]
            s = Symbol(fileName,ln,"CurrentPkg",name=symbolName,export=True)
            addScrapedSymbol(s)
            s = Symbol(fileName,ln,"CurrentPkg",name="setf_"+symbolName,export=True)
            addScrapedSymbol(s)
            continue

        match = bad_macros_re.match(l)
        if ( match != None ):
            raise Exception, ("In %s:%d found a scraping keyword[%s] with that didn't properly match - fix it" % (fileName, ln , l))
    fin.close()


(addedSymbols, removedSymbols) = buildSymbolTable()
dump_symbolTable("include/generated/_symbolTableAfterBuild.txt")

# Figure out if the exported symbols have changed
# existingExportedSymbols is a set of symbol names that were
# exported according to the symbols_scraped symbol table

scrapedExportedSymbols = set()
for s in symbolTable:
    if ( s != None and s.export() ):
        scrapedExportedSymbols.add(s.lispName())

#for x in scrapedExportedSymbols:
#    print("scrapedExportedSymbol: %s" % x)
#for x in existingExportedSymbols:
#    print("existingExportedSymbol: %s" % x)



if ( len(addedSymbols)>0 or len(removedSymbols)>0 or (not saw_symbol_table) or (scrapedExportedSymbols!=existingExportedSymbols)):
    print ("!--------------------------------------------------")
    print ("!")
    print ("!   changes were seen - rebuilding symbol table %s" % symbolsFileName )
    print ("!")
    print ("! Added symbols: %s" % addedSymbols )
    print ("!")
    print ("! Removed symbols: %s" % removedSymbols )

    newScrapedExportedSymbols = scrapedExportedSymbols.difference(existingExportedSymbols)
    oldScrapedExportedSymbols = existingExportedSymbols.difference(scrapedExportedSymbols)
    if ( len(newScrapedExportedSymbols)>0 ):
        print ("!")
        print("! New scraped exported symbols: %s" % newScrapedExportedSymbols )
    if ( len(oldScrapedExportedSymbols)>0 ):
        print ("!")
        print("! Scraped symbols no longer exported: %s" % oldScrapedExportedSymbols )
    print ("!--------------------------------------------------")

    fout = open(symbolsFileName,"w")
    fout.write("// Symbol table\n" )
    fout.flush()
    scrapeFlagPrefix = os.path.basename(getcwd())
#    fout.write("#include \"%s_scrape_flag.h\"\n"%scrapeFlagPrefix)
    for s in symbolTable:
        if ( s != None ):
            fout.write("%s\n" % s.symbol_table_repr())

    packages = uniquePackages(symbolTable)
    for package in packages:
        fout.write("#ifdef %s_SYMBOLS\n"%package)
        for s in symbolTable:
            if ( (s.package() == package) and (s != None) ):
                fout.write("DO_SYMBOL(%s,%s,%s,\"%s\",%s);\n"%(s.cName(),s.rsid(),s.package(),s.lispName(),s.exportString()))
        fout.write("#endif\n")
    fout.close()

else:
    print ("!--------------------------------------------------")
    print ("!")
    print ("!   No changes to symbols were seen")
    print ("!")
    print ("!--------------------------------------------------")
    sys.exit(0)

