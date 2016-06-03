import os
import StringIO

top = '.'
out = 'wbuild'
APPNAME = 'clasp'
VERSION = '0.0'


def all_files():
    names = []
    for name in [
            'gc_interface',
            'gc_boot',
            'gcFunctions',
            'gctoolsPackage',
            'globals',
            'gcStack',
            'telemetry',
            'gcalloc',
            'gcweak',
            'memoryManagement']:
        names.append('src/gctools/%s.cc' % name)
    for name in [
            'clcenv',
            'environment',
            'activationFrame',
            'evaluator',
            'functor',
            'creator',
            'sharpEqualWrapper',
            'vectorDisplaced',
            'stacks',
            'weakKeyMapping',
            'weakHashTable',
            'weakPointer',
            'compiler',
            'genericFunction',
            'instance',
            'cache',
            'float_to_string',
            'primitives',
            'random',
            'cxxObject',
            'cxxClass',
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
            'binder',
            'multiStringBuffer',
            'candoOpenMp',
            'foundation',
            'lambdaListHandler',
            'lispStream',
            'bits',
            'write_symbol',
            'corePackage',
            'lisp',
            'profiler',
            'lispDefinitions',
            'bundle',
            'profile',
            'specializer',
            'write_ugly',
            'regex',
            'userData',
            'wrappedPointer',
            'serialize',
            'sexpLoadArchive',
            'sexpSaveArchive',
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
            'str',
            'package',
            'commonLispPackage',
            'allClSymbols',
            'keywordPackage',
            'extensionPackage',
            'vectorObjectsWithFillPtr',
            'vectorObjects',
            'array',
            'strWithFillPtr',
            'lispMath',
            'grayPackage',
            'closPackage',
            'cleavirPrimopsPackage',
            'cleavirEnvPackage',
            'compPackage',
            'bootStrapCoreSymbolMap',
            'cons',
            'symbol',
            'builtInClass',
            'standardClass',
            'conditions',
            'arrayDisplaced',
            'object',
            'stdClass',
            'metaobject',
            'arguments',
            'pointer',
            'funcallableStandardClass',
            'standardObject',
            'iterator',
            'sysprop',
            'bformat',
            'backquote',
            'documentation',
            'lispReader',
            'singleDispatchGenericFunction',
            'singleDispatchMethod',
            'structureObject',
            'structureClass',
            'null',
            'forwardReferencedClass',
            'character',
            'lispString',
            'designators',
            'sequence',
            'lispVector',
            'loadTimeValues',
            'arrayObjects',
            'reader',
            'lightProfiler',
            'fileSystem',
            'intArray',
            'bitVector',
            'posixTime']:
        names.append('src/core/%s.cc' % name)
    for name in [
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
            'derivable_class']:
        names.append('src/clbind/%s.cc' % name)
    for name in [
	    'cffi',
	    'cffiPackage']:
        names.append('src/cffi/%s.cc' % name)
    for name in [
            'serveEvent',
            'serveEventPackage']:
        names.append('src/serveEvent/%s.cc' % name)
    for name in [
            'sockets',
            'socketsPackage']:
        names.append('src/sockets/%s.cc' % name)
    for name in [
            'debugInfoExpose',
            'debugLoc',
            'llvmoDwarf',
            'link_intrinsics',
            'intrinsics',
            'insertPoint',
            'irtests',
            'llvmoExpose',
            'llvmoPackage',
            'clbindLlvmExpose']:
        names.append('src/llvmo/%s.cc' % name)
    for name in [
            'astVisitor',
            'astExpose',
            'clangTooling',
            'asttoolingPackage',
            'clangCompiler']:
        names.append('src/asttooling/%s.cc' % name)
    for name in [ 'main' ]:
        names.append('src/main/%s.cc' % name)
    return names

        



def options(opt):
    opt.load('compiler_cxx')
    opt.load('compiler_c')
#    opt.add_option('--gc', default='boehm', help='Garbage collector');

def configure(cfg):
    cfg.check_waf_version(mini='1.7.5')
    cfg.load('compiler_cxx')
    cfg.load('compiler_c')
#    mysdk = cfg.options.mysdk
#    cfg.env.MY_MYSDK = mysdk
##### Why is this include relative to ./src ????
    cfg.define("USE_BOEHM",1)
    cfg.define("USE_CLASP_DYNAMIC_CAST",1)
    cfg.define("BUILDING_CLASP",1)
    cfg.define("_TARGET_OS_DARWIN",1)
    cfg.define("PROGRAM_CLASP",1)
    cfg.define("CLASP_GIT_COMMIT","ecf5585")
    cfg.define("CLASP_VERSION","0.4.0-622-g9e0535b")
    cfg.define("CLBIND_DYNAMIC_LINK",1)
    cfg.define("DEBUG_CL_SYMBOLS",1)
    cfg.define("DEBUG_DRAG",1)
    cfg.define("DEBUG_TRACE_INTERPRETED_CLOSURES",1)
    cfg.define("EXPAT",1)
    cfg.define("INCLUDED_FROM_CLASP",1)
    cfg.define("INHERITED_FROM_SRC",1)
    cfg.define("LLVM_VERSION","390")
    cfg.define("NDEBUG",1)
    cfg.define("READLINE",1)
    cfg.define("USE_AMC_POOL",1)
    cfg.define("USE_EXPENSIVE_BACKTRACE",1)
    cfg.define("X86",1)
    cfg.define("DEBUG_FUNCTION_CALL_COUNTER",1)
    cfg.define("_ADDRESS_MODEL_64",1)
    cfg.define("_RELEASE_BUILD",1)
    cfg.define("__STDC_CONSTANT_MACROS",1)
    cfg.define("__STDC_FORMAT_MACROS",1)
    cfg.define("__STDC_LIMIT_MACROS",1)
#    cfg.env.append_value('CXXFLAGS', ['-v'] )
    cfg.env.append_value('CXXFLAGS', [ '-std=c++11'])
    cfg.env.append_value('CXXFLAGS', [ '-O3' ])
    cfg.env.append_value('CXXFLAGS', [ '-I../include/'])
    cfg.env.append_value('CXXFLAGS', ['-I../src/main/include'] )
    cfg.env.append_value('CXXFLAGS', os.getenv("CLASP_RELEASE_CXXFLAGS").split() )
    cfg.env.append_value('CXXFLAGS', ['-I/usr/include'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-deprecated-register'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-expansion-to-defined'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-return-type-c-linkage'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-invalid-offsetof'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-#pragma-messages'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-inconsistent-missing-override'] )
    cfg.env.append_value('LINKFLAGS', ['-L/usr/lib'])
    cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_RELEASE_LINKFLAGS").split())
    cfg.env.append_value('LINKFLAGS', ['-Wl,-stack_size,0x1000000'])
    cfg.env.append_value('LINKFLAGS', ['-fvisibility=default'])
    cfg.env.append_value('LINKFLAGS', ['-lc++'])
    cfg.env.append_value('LINKFLAGS', ['-rdynamic'])
    cfg.env.append_value('LINKFLAGS', ['-stdlib=libc++'])
    cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_RELEASE_LINKFLAGS").split())
    cfg.env.append_value('LINKFLAGS', ['-Wl,-stack_size,0x1000000'])
    cfg.env.append_value('LINKFLAGS', ['-fvisibility=default'])
    cfg.env.append_value('LINKFLAGS', ['-lc++'])
    cfg.env.append_value('LINKFLAGS', ['-rdynamic'])
    cfg.env.append_value('LINKFLAGS', ['-stdlib=libc++'])
    cfg.env.append_value('LINKFLAGS', ['-L../build/clasp/Contents/Resources/lib/common/lib'])
    cfg.env.append_value('LINKFLAGS', ['-lclangAnalysis'])
    cfg.env.append_value('LINKFLAGS', ['-lclangARCMigrate'])
    cfg.env.append_value('LINKFLAGS', ['-lclangAST'])
    cfg.env.append_value('LINKFLAGS', ['-lclangASTMatchers'])
    cfg.env.append_value('LINKFLAGS', ['-lclangBasic'])
    cfg.env.append_value('LINKFLAGS', ['-lclangCodeGen'])
    cfg.env.append_value('LINKFLAGS', ['-lclangDriver'])
    cfg.env.append_value('LINKFLAGS', ['-lclangDynamicASTMatchers'])
    cfg.env.append_value('LINKFLAGS', ['-lclangEdit'])
    cfg.env.append_value('LINKFLAGS', ['-lclangFormat'])
    cfg.env.append_value('LINKFLAGS', ['-lclangFrontend'])
    cfg.env.append_value('LINKFLAGS', ['-lclangFrontendTool'])
    cfg.env.append_value('LINKFLAGS', ['-lclangIndex'])
    cfg.env.append_value('LINKFLAGS', ['-lclangLex'])
    cfg.env.append_value('LINKFLAGS', ['-lclangParse'])
    cfg.env.append_value('LINKFLAGS', ['-lclangRewrite'])
    cfg.env.append_value('LINKFLAGS', ['-lclangRewriteFrontend'])
    cfg.env.append_value('LINKFLAGS', ['-lclangSema'])
    cfg.env.append_value('LINKFLAGS', ['-lclangSerialization'])
    cfg.env.append_value('LINKFLAGS', ['-lclangStaticAnalyzerCheckers'])
    cfg.env.append_value('LINKFLAGS', ['-lclangStaticAnalyzerCore'])
    cfg.env.append_value('LINKFLAGS', ['-lclangStaticAnalyzerFrontend'])
    cfg.env.append_value('LINKFLAGS', ['-lclangTooling'])
    cfg.env.append_value('LINKFLAGS', ['-lclangToolingCore'])
    cfg.env.append_value('LINKFLAGS', ['-lboost_filesystem'])
    cfg.env.append_value('LINKFLAGS', ['-lboost_regex'])
    cfg.env.append_value('LINKFLAGS', ['-lboost_date_time'])
    cfg.env.append_value('LINKFLAGS', ['-lboost_program_options'])
    cfg.env.append_value('LINKFLAGS', ['-lboost_system'])
    cfg.env.append_value('LINKFLAGS', ['-lboost_iostreams'])
    cfg.env.append_value('LINKFLAGS', ['-lz'])
    cfg.env.append_value('LINKFLAGS', ['-lncurses'])
    cfg.env.append_value('LINKFLAGS', ['-lreadline'])
    cfg.env.append_value('LINKFLAGS', ['-lgc'])
#    cfg.check(features='cxx cxxprogram', lib=['pthread'], uselib_store='PTHREAD')
#    cfg.env.append_value('LINKFLAGS_MYSDK', mysdk + '/mysdk.so')
#    cfg.env.LINKFLAGS_PYEXT_MYSDK = cfg.env.LINKFLAGS_PYEXT
#    cfg.env.append_value('LINKFLAGS_PYEXT_MYSDK', mysdk + '/mysdk.so')

def build(bld):
#    bld(name='myInclude', export_includes=[bld.env.MY_MYSDK, 'include'])
    print("In build(bld)")
    bld.program(source=all_files(),target="clasp")


from waflib import TaskGen
from waflib import Task

class preprocess(Task.Task):
    run_str = '${CXX} -E -DSCRAPING ${ARCH_ST:ARCH} ${CXXFLAGS} ${CPPFLAGS} ${FRAMEWORKPATH_ST:FRAMEWORKPATH} ${CPPPATH_ST:INCPATHS} ${DEFINES_ST:DEFINES} ${CXX_SRC_F}${SRC} ${CXX_TGT_F}${TGT[0].abspath()}'
    ext_out = ['.i']

class sif(Task.Task):
    clasp_home = os.getenv("CLASP_HOME")
    run_str = 'generate-one-sif ${SRC[0].abspath()} ${TGT[0].abspath()}'
    ext_out = ['.sif']

class generated_headers(Task.Task):
#    run_str = 'generate-headers-from-all-sifs src/main/ ${SRC}'
    ext_out = ['.h']
    def run(self):
        cmd = StringIO.StringIO()
        cmd.write('generate-headers-from-all-sifs src/main')
        for f in self.inputs:
            cmd.write(' %s' % f.abspath())
        print("About to run: %s" % cmd.getvalue())
        return self.exec_command(cmd.getvalue())


# Have all 'cxx' targets have 'include' in their include paths.
@TaskGen.feature('cxx')
@TaskGen.after('process_source')
def preprocess_task_generator(self):
    print("Incoming  self: %s" % self)
    compiled_tasks = self.compiled_tasks
    all_sif_files = []
    for task in self.compiled_tasks:
        if ( task.__class__.__name__ == 'cxx' ):
            for node in task.inputs:
                i_node = node.change_ext('.i')
                sif_node = node.change_ext('.sif')
                self.create_task('preprocess',node,[i_node])
                self.source.append(i_node)
                self.create_task('sif',i_node,[sif_node])
                all_sif_files.append(sif_node)
    generated_headers = [ 'src/main/include/generated/c-wrappers.h',
                          'src/main/include/generated/enum_inc.h',
                          'src/main/include/generated/initClassesAndMethods_inc.h',
                          'src/main/include/generated/initFunctions_inc.h',
                          'src/main/include/generated/initializers_inc.h',
                          'src/main/include/generated/sourceInfo_inc.h',
                          'src/main/include/generated/symbols_scraped_inc.h' ]
    nodes = []
    for x in generated_headers:
        nodes.append(self.path.make_node(x))
    self.create_task('generated_headers',all_sif_files,nodes)
