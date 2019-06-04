#-*- mode: python; coding: utf-8-unix -*-

from cleavir_file_list import cleavir_file_list

SRC_CORE_FILES = \
    [
        'dummy',
        'mpPackage',
        'nativeVector',
        'environment',
        'activationFrame',
        'evaluator',
        'functor',
        'creator',
        'queue',
        'sharpEqualWrapper',
        'stacks',
        'weakKeyMapping',
        'weakHashTable',
        'weakPointer',
        'compiler',
        'genericFunction',
        'instance',
        'funcallableInstance',
        'cache',
        'float_to_string',
        'primitives',
        'random',
#        'cxxClass',
        'record',
        'debugger',
        'smallMap',
        'smallMultimap',
        'hashTable',
        'hashTableEq',
        'hashTableEql',
        'hashTableEqual',
        'hashTableEqualp',
        'numbers',
        'numerics',
        'num_arith',
        'numberToString',
        'num_co',
        'load',
        'bignum',
        'write_object',
        'write_array',
        'print',
        'sourceFileInfo',
        'symbolToEnumConverter',
        'core_globals',
        'externalObject',
        'myReadLine',
        'specialForm',
        'unixfsys',
        'lispList',
        'multiStringBuffer',
        'candoOpenMp',
        'foundation',
        'lambdaListHandler',
        'lispStream',
        'bits',
        'write_symbol',
        'corePackage',
        'lisp',
        'bundle',
        'write_ugly',
        'wrappedPointer',
        'serialize',
#        'sexpLoadArchive',
#        'sexpSaveArchive',
        'readtable',
        'float_to_digits',
        'pathname',
        'commandLineOptions',
        'exceptions',
        'commonLispUserPackage',
        'metaClass',
        'multipleValues',
        'testing',
        'predicates',
        'write_list',
        'package',
        'commonLispPackage',
        'allClSymbols',
        'keywordPackage',
        'extensionPackage',
        'array',
        'grayPackage',
        'closPackage',
        'cleavirPrimopsPackage',
        'cleavirIrPackage',
        'compPackage',
        'bootStrapCoreSymbolMap',
        'cons',
        'symbol',
        'object',
        'arguments',
        'pointer',
        'iterator',
        'sysprop',
        'bformat',
        'backquote',
        'documentation',
        'lispReader',
        'singleDispatchGenericFunction',
        'singleDispatchMethod',
        'derivableCxxObject',
        'null',
        'character',
        'designators',
        'sequence',
        'loadTimeValues',
#        'reader',
        'lightProfiler',
        'fileSystem',
        'intArray',
        'posixTime',
        'hwinfo',
        'clasp_ffi_package',
        'fli',
    ]

def collect_source_file(bld, path, name, extension = '.cc'):
    fullName = path + name
    if '.' not in name:
        fullName = fullName + extension
    node = bld.path.find_node(fullName)
    assert node != None, "Could not find %s" % (fullName)
    return node

def collect_c_source_files(bld, path, files, extension = '.cc'):
    result = []
    for name in files:
        result.append(collect_source_file(bld, path, name, extension = extension))
    return result

def collect_clasp_c_source_files(bld):
    result = collect_c_source_files(bld, 'src/gctools/', [
                 'gc_interface',
                 'boehmGarbageCollection',
                 'mpsGarbageCollection',
                 'hardErrors',
                 'source_info',
                 'threadlocal',
                 'gc_boot',
                 'interrupt',
                 'gcFunctions',
                 'gctoolsPackage',
                 'globals',
                 'gcStack',
                 'gcalloc',
                 'gcweak',
                 'memoryManagement',
                 'mygc.c']) + \
             collect_c_source_files(bld, 'src/clbind/', [
                 'adapter',
                 'class_rep',
                 'open',
                 'class_registry',
                 'link_compatibility',
                 'scope',
                 'inheritance',
                 'clbind',
                 'clbindPackage',
                 'class',
                 'derivable_class']) + \
             collect_c_source_files(bld, 'src/serveEvent/', [
                 'serveEvent',
                 'serveEventPackage']) + \
             collect_c_source_files(bld, 'src/sockets/', [
                 'sockets',
                 'socketsPackage']) + \
             collect_c_source_files(bld, 'src/llvmo/', [
                 'debugInfoExpose',
                 'debugLoc',
                 'llvmoDwarf',
                 'link_intrinsics',
                 'builtins',
                 'intrinsics',
                 'insertPoint',
                 'irtests',
                 'llvmoExpose',
                 'llvmoPackage',
                 'clbindLlvmExpose']) + \
             collect_c_source_files(bld, 'src/mpip/', [
                 'claspMpi'
             ]) + \
             collect_c_source_files(bld, 'src/asttooling/', [
                 'astVisitor',
                 'astExpose',
                 'clangTooling',
                 'asttoolingPackage',
                 'clangCompiler']) + \
             collect_c_source_files(bld, 'src/main/', ['main']) + \
             collect_c_source_files(bld, 'src/core/', SRC_CORE_FILES)

    return result

def collect_aclasp_lisp_files(wrappers = True):
    result = [
        "src/lisp/kernel/tag/start" ]
    result = result + ["src/lisp/kernel/lsp/prologue"]
    # Don't use wrappers for now - the direct CALL to THROW-FUNCTION needs to use INVOKE and the landing-pad
    # otherwise the exception handling will break because cleanup forms in the wrapper won't be evaluated
    # This is a problem if cc_push_InvocationHistoryFrame/cc_pull_InvocationHistoryFrame are used
    if wrappers:
        result = result + [
            "src/lisp/kernel/lsp/direct-calls",
            "generated/cl-wrappers"
        ]
    result = result + [
        "src/lisp/kernel/tag/min-start",
        "src/lisp/kernel/init",
        "src/lisp/kernel/tag/after-init",
        "src/lisp/kernel/cmp/runtime-info" ]
    result = result + ["src/lisp/kernel/lsp/sharpmacros" ]
    result = result + [
        "src/lisp/kernel/cmp/jit-setup",
        "src/lisp/kernel/clsymbols",
        "src/lisp/kernel/lsp/packages",
        "src/lisp/kernel/lsp/foundation",
        "src/lisp/kernel/lsp/export",
        "src/lisp/kernel/lsp/defmacro",
        "src/lisp/kernel/lsp/helpfile"]
    result = result + [
        "src/lisp/kernel/lsp/evalmacros",
        "src/lisp/kernel/lsp/claspmacros",
        "src/lisp/kernel/lsp/source-transformations",
        "src/lisp/kernel/lsp/arraylib",
        "src/lisp/kernel/lsp/setf"]
    result = result + [
        "src/lisp/kernel/lsp/listlib",
        "src/lisp/kernel/lsp/mislib",
        "src/lisp/kernel/lsp/defstruct",
        "src/lisp/kernel/lsp/predlib",
        "src/lisp/kernel/lsp/cdr-5",
        "src/lisp/kernel/lsp/cmuutil",
        "src/lisp/kernel/lsp/seqmacros",
        "src/lisp/kernel/lsp/seq",
        "src/lisp/kernel/lsp/seqlib",
        "src/lisp/kernel/lsp/iolib",
#        "src/lisp/kernel/lsp/sharpmacros",
        "src/lisp/kernel/lsp/backtrace",
        "src/lisp/kernel/lsp/trace",
        "src/lisp/kernel/cmp/cmpexports",
        "src/lisp/kernel/cmp/cmpsetup",
        "src/lisp/kernel/cmp/cmpglobals",
        "src/lisp/kernel/cmp/cmputil",
        "src/lisp/kernel/cmp/cmpintrinsics",
        "src/lisp/kernel/cmp/primitives",
        "src/lisp/kernel/cmp/cmpir",
        "src/lisp/kernel/cmp/cmpeh",
        "src/lisp/kernel/cmp/debuginfo",
        "src/lisp/kernel/cmp/codegen-vars",
        "src/lisp/kernel/cmp/arguments",
        "src/lisp/kernel/cmp/cmplambda",
        "src/lisp/kernel/cmp/cmprunall",
        "src/lisp/kernel/cmp/cmpliteral",
        "src/lisp/kernel/cmp/typeq",
        "src/lisp/kernel/cmp/codegen-special-form",
        "src/lisp/kernel/cmp/codegen",
        "src/lisp/kernel/cmp/compile",
        "src/lisp/kernel/cmp/codegen-toplevel",
        "src/lisp/kernel/cmp/compile-file",
        "src/lisp/kernel/cmp/external-clang",
        "src/lisp/kernel/cmp/cmpname",
        "src/lisp/kernel/cmp/cmpbundle",
        "src/lisp/kernel/cmp/cmprepl",
        "src/lisp/kernel/tag/min-pre-epilogue",
        "src/lisp/kernel/lsp/epilogue-aclasp",
        "src/lisp/kernel/tag/min-end"]
    return result

def collect_bclasp_lisp_files(**kwargs):
    files = collect_aclasp_lisp_files(**kwargs) + [
        "src/lisp/kernel/tag/bclasp-start",
        "src/lisp/kernel/cmp/cmpwalk",
        "src/lisp/kernel/lsp/assert",
        "src/lisp/kernel/lsp/numlib",
        "src/lisp/kernel/lsp/describe",
        "src/lisp/kernel/lsp/module",
        "src/lisp/kernel/lsp/loop2",
        "src/lisp/kernel/cmp/disassemble",
        "src/lisp/kernel/cmp/opt", # need loop
        "src/lisp/kernel/cmp/opt-character",
        "src/lisp/kernel/cmp/opt-number",
        "src/lisp/kernel/cmp/opt-type",
        "src/lisp/kernel/cmp/opt-control",
        "src/lisp/kernel/cmp/opt-sequence",
        "src/lisp/kernel/cmp/opt-cons",
        "src/lisp/kernel/cmp/opt-array",
        "src/lisp/kernel/cmp/opt-object",
        "src/lisp/kernel/cmp/opt-condition",
        "src/lisp/kernel/lsp/shiftf-rotatef",
        "src/lisp/kernel/lsp/assorted",
        "src/lisp/kernel/lsp/packlib",
        "src/lisp/kernel/lsp/defpackage",
        "src/lisp/kernel/lsp/format",
        "src/lisp/kernel/lsp/mp",
        "src/lisp/kernel/clos/package",
        "src/lisp/kernel/clos/static-gfs/package",
        "src/lisp/kernel/clos/static-gfs/flag",
        "src/lisp/kernel/clos/static-gfs/constructor",
        "src/lisp/kernel/clos/hierarchy",
        "src/lisp/kernel/clos/cpl",
        "src/lisp/kernel/clos/std-slot-value",
        "src/lisp/kernel/clos/slot",
        "src/lisp/kernel/clos/boot",
        "src/lisp/kernel/clos/kernel",
        "src/lisp/kernel/clos/dtree",
        "src/lisp/kernel/clos/cmpfastgf",
        "src/lisp/kernel/clos/closfastgf",
        "src/lisp/kernel/clos/satiation",
        "src/lisp/kernel/clos/method",
        "src/lisp/kernel/clos/combin",
        "src/lisp/kernel/clos/std-accessors",
        "src/lisp/kernel/clos/defclass",
        "src/lisp/kernel/clos/slotvalue",
        "src/lisp/kernel/clos/standard",
        "src/lisp/kernel/clos/builtin",
        "src/lisp/kernel/clos/change",
        "src/lisp/kernel/clos/stdmethod",
        "src/lisp/kernel/clos/generic",
        "src/lisp/kernel/clos/fixup",
        "src/lisp/kernel/clos/static-gfs/cell",
        "src/lisp/kernel/clos/static-gfs/effective-method",
        "src/lisp/kernel/clos/static-gfs/svuc",
        "src/lisp/kernel/clos/static-gfs/shared-initialize",
        "src/lisp/kernel/clos/static-gfs/initialize-instance",
        "src/lisp/kernel/clos/static-gfs/allocate-instance",
        "src/lisp/kernel/clos/static-gfs/make-instance",
        "src/lisp/kernel/clos/static-gfs/compute-constructor",
        "src/lisp/kernel/clos/static-gfs/dependents",
        "src/lisp/kernel/clos/static-gfs/compiler-macros",
        "src/lisp/kernel/lsp/source-location",
        "src/lisp/kernel/lsp/defvirtual",
        "src/lisp/kernel/clos/conditions",
        "src/lisp/kernel/clos/print",
        "src/lisp/kernel/clos/streams",
        "src/lisp/kernel/lsp/pprint",
        "src/lisp/kernel/cmp/compiler-conditions",
        "src/lisp/kernel/lsp/packlib2",
        "src/lisp/kernel/clos/inspect",
        "src/lisp/kernel/lsp/fli",
        "src/lisp/modules/sockets/sockets",
        "src/lisp/kernel/lsp/top",
        "src/lisp/kernel/tag/pre-epilogue-bclasp",
        "src/lisp/kernel/lsp/epilogue-bclasp",
        "src/lisp/kernel/tag/bclasp"
    ]
    return files

def collect_cclasp_lisp_files(**kwargs):
    return collect_bclasp_lisp_files(**kwargs) + cleavir_file_list + [
        "src/lisp/kernel/lsp/queue",
        "src/lisp/kernel/cmp/compile-file-parallel",
        "src/lisp/kernel/tag/pre-epilogue-cclasp",
        "src/lisp/kernel/lsp/epilogue-cclasp",
        "src/lisp/kernel/tag/cclasp" ]
