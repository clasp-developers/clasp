import os
import StringIO
from waflib.extras import clang_compilation_database
from waflib import Utils

top = '.'
out = 'wbuild'
APPNAME = 'clasp'
VERSION = '0.0'


VARIANTS = [ 'boehm_o',
             'boehm_d',
             'boehmdc_o',
             'boehmdc_d',
             'mpsprep_o',
             'mpsprep_d',
             'mps_o',
             'mps_d' ]

class variant():
    stage = 0
    def common_setup(self,cfg):
        cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.executable_name)

def configure_clasp(cfg,variant):
    include_path = "%s/%s/%s/src/include/clasp/main/" % (cfg.path.abspath(),out,variant.variant_dir) #__class__.__name__)
    print("Including from %s" % include_path )
    cfg.env.append_value("CXXFLAGS", ['-I%s' % include_path])
    cfg.define("EXECUTABLE_NAME",variant.executable_name)
    cfg.define("BITCODE_NAME",variant.bitcode_name)
    cfg.define("VARIANT_NAME",variant.variant_name)

class boehm_o(variant):
    variant_dir = 'boehm_o'
    variant_name = 'boehm'
    bitcode_name = '%s-o' % variant_name
    executable_name = 'i%s-o' % variant_name
    def configure(self,cfg,env_copy):
        cfg.setenv(self.variant_dir, env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_BOEHM",1)
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O3', '-g' ])
        cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_RELEASE_LINKFLAGS").split())
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        cfg.env.append_value('LINKFLAGS', ['-lgmp'])
        self.common_setup(cfg)
        cfg.write_config_header('%s/config.h'%self.variant_dir, remove=True)
    def install(self):
        return self.executable_name

class boehm_d(variant):
    variant_dir = 'boehm_d'
    variant_name = 'boehm'
    bitcode_name = '%s-d' % variant_name
    executable_name = 'i%s-d' % variant_name
    def configure(self,cfg,env_copy):
        cfg.setenv("boehm_d", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_BOEHM",1)
        cfg.define("_DEBUG_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_DEBUG_LINKFLAGS").split())
        self.common_setup(cfg)
        cfg.write_config_header('boehm_d/config.h', remove=True)
    def install(self):
        return self.executable_name

class boehmdc_o(variant):
    variant_dir = 'boehmdc_o'
    variant_name = 'boehmdc'
    bitcode_name = '%s-o' % variant_name
    executable_name = 'i%s-o' % variant_name
    def configure(self,cfg,env_copy):
        cfg.setenv("boehmdc_o", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_BOEHM",1)
        cfg.define("USE_CXX_DYNAMIC_CAST",1)
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O3', '-g' ])
        cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_RELEASE_LINKFLAGS").split())
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        cfg.env.append_value('LINKFLAGS', ['-lgmp'])
        self.common_setup(cfg)
        cfg.write_config_header('boehmdc_o/config.h', remove=True)
    def install(self):
        return self.executable_name

class boehmdc_d(variant):
    variant_dir = 'boehmdc_d'
    variant_name = 'boehmdc'
    bitcode_name = '%s-d' % variant_name
    executable_name = 'i%s-d' % variant_name
    def configure(self,cfg,env_copy):
        cfg.setenv("boehmdc_d", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_BOEHM",1)
        cfg.define("USE_CXX_DYNAMIC_CAST",1)
        cfg.define("_DEBUG_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_DEBUG_LINKFLAGS").split())
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        self.common_setup(cfg)
        cfg.write_config_header('boehmdc_d/config.h', remove=True)
    def install(self):
        return self.executable_name

class mpsprep_o(variant):
    variant_dir = 'mpsprep_o'
    variant_name = 'mpsprep'
    bitcode_name = '%s-o' % variant_name
    executable_name = 'i%s-o' % variant_name
    def configure(self,cfg,env_copy):
        cfg.setenv("mpsprep_o", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_MPS",1)
        cfg.define("RUNNING_GC_BUILDER",1)
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3' ])
        cfg.env.append_value('CFLAGS', [ '-O3' ])
        cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_RELEASE_LINKFLAGS").split())
        self.common_setup(cfg)
        cfg.write_config_header('mpsprep_o/config.h', remove=True)
    def install(self):
        return self.executable_name

class mpsprep_d(variant):
    variant_dir = 'mpsprep_d'
    variant_name = 'mpsprep'
    bitcode_name = '%s-d' % variant_name
    executable_name = 'i%s-d' % variant_name
    def configure(self,cfg,env_copy):
        cfg.setenv("mpsprep_d", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_MPS",1)
        cfg.define("RUNNING_GC_BUILDER",1)
        cfg.define("_DEBUG_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_DEBUG_LINKFLAGS").split())
        self.common_setup(cfg)
        cfg.write_config_header('mpsprep_d/config.h', remove=True)
    def install(self):
        return self.executable_name

class mps_o(variant):
    variant_dir = 'mps_o'
    variant_name = 'mps'
    bitcode_name = '%s-o' % variant_name
    executable_name = 'i%s-o' % variant_name
    def configure(self,cfg,env_copy):
        cfg.setenv("mps_o", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_MPS",1)
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3' ])
        cfg.env.append_value('CFLAGS', [ '-O3' ])
        cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_RELEASE_LINKFLAGS").split())
        self.common_setup(cfg)
        cfg.write_config_header('mps_o/config.h', remove=True)
    def install(self):
        return self.executable_name


class mps_d(variant):
    variant_dir = 'mps_d'
    variant_name = 'mps'
    bitcode_name = '%s-d' % variant_name
    executable_name = 'i%s-d' % variant_name
    def configure(self,cfg,env_copy):
        cfg.setenv("mps_d", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_MPS",1)
        cfg.define("_DEBUG_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_DEBUG_LINKFLAGS").split())
        self.common_setup(cfg)
        cfg.write_config_header('mps_d/config.h', remove=True)
    def install(self):
        return self.executable_name

import subprocess

def lisp_source_files(exe,stage):
    print("About to execute: %s" % exe)
    proc = subprocess.Popen([exe, "-I", "-f", "ecl-min", "-e", "(source-files-%s)"%stage, "-e", "(quit)"], stdout=subprocess.PIPE, shell=True)
    (out, err) = proc.communicate()
    print( "source-files-%s: %s" % (stage,out))
    return out


def options(opt):
    opt.load('compiler_cxx')
    opt.load('compiler_c')
#    opt.add_option('--gc', default='boehm', help='Garbage collector');


def build_plugin_headers(root,headers_list):
    print("DEBUG build_plugin_headers directory = %s\n" % root )
    print("DEBUG build_plugin_headers headers_list=%s\n"% headers_list)
    if not os.path.exists(os.path.dirname("%s/generated/"%root)):
        os.makedirs(os.path.dirname("%s/generated/"%root))
    fout = open("%s/generated/plugin_headers.h"%root,"w")
    fout.write("// Generated by wscript build_plugin_headers - Do not modify!!\n" )
    for x in headers_list:
        fout.write("#include <%s>\n" % x)
    fout.close()

def configure(cfg):
    cfg.check_waf_version(mini='1.7.5')
    cfg.load('compiler_cxx')
    cfg.load('compiler_c')
    cfg.plugins_includes = []
    cfg.plugins_headers = []
    cfg.recurse('plugins')
    print("cfg.__dict__.keys() = %s\n" % cfg.__dict__.keys())
    print("DEBUG cfg.plugins_includes = %s" % cfg.plugins_includes)
    print("DEBUG cfg.plugins_headers = %s" % cfg.plugins_headers)
    cfg.env.append_value('CXXFLAGS', ['-I./'])
    if ('program_name' in cfg.__dict__):
        pass
    else:
        cfg.env.append_value('CXXFLAGS', ['-I%s/include/clasp/main/'% (os.getenv("CLASP_HOME"))] )

# Check if GC_enumerate_reachable_objects_inner is available
# If so define  BOEHM_GC_ENUMERATE_REACHABLE_OBJECTS_INNER_AVAILABLE
#
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
    cfg.define("__STDC_CONSTANT_MACROS",1)
    cfg.define("__STDC_FORMAT_MACROS",1)
    cfg.define("__STDC_LIMIT_MACROS",1)
#    cfg.env.append_value('CXXFLAGS', ['-v'] )
#    cfg.env.append_value('CXXFLAGS', ['-I../src/main/'] )
    includes = [ 'include/' ]
    includes = includes + cfg.plugins_includes
    includes_from_build_dir = []
    for x in includes:
        includes_from_build_dir.append("-I%s/%s"%(os.getenv("CLASP_HOME"),x))
    print("DEBUG includes_from_build_dir = %s\n" % includes_from_build_dir)
    cfg.env.append_value('CXXFLAGS', [ '-std=c++11'])
    cfg.env.append_value('CXXFLAGS', includes_from_build_dir )
    cfg.env.append_value('CFLAGS', includes_from_build_dir )
    cfg.env.append_value('CXXFLAGS', '-flto')
    cfg.env.append_value('CFLAGS', '-flto')
    cfg.env.append_value('CXXFLAGS', os.getenv("CLASP_RELEASE_CXXFLAGS").split() )
    cfg.env.append_value('CXXFLAGS', ['-I/usr/include'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-macro-redefined'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-deprecated-register'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-expansion-to-defined'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-return-type-c-linkage'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-invalid-offsetof'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-#pragma-messages'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-inconsistent-missing-override'] )
    lto_library = "%s/libLTO.%s" % (os.getenv("CLASP_RELEASE_LLVM_LIB_DIR"), os.getenv("CLASP_LIB_EXTENSION"))
    cfg.env.append_value('LINKFLAGS',"-Wl,-lto_library,%s" % lto_library)
    cfg.env.append_value('LINKFLAGS', ['-flto'])
    cfg.env.append_value('LINKFLAGS', ['-L/usr/lib'])
    cfg.env.append_value('LINKFLAGS', ['-Wl,-stack_size,0x1000000'])
    cfg.env.append_value('LINKFLAGS', ['-fvisibility=default'])
    cfg.env.append_value('LINKFLAGS', ['-lc++'])
    cfg.env.append_value('LINKFLAGS', ['-rdynamic'])
    cfg.env.append_value('LINKFLAGS', ['-Wl,-export_dynamic'])
#    cfg.env.append_value('LINKFLAGS', ['-Wl,-exported_symbols_list,%s/src/llvmo/intrinsic-api.txt' % os.getenv("CLASP_HOME")] )
    cfg.env.append_value('LINKFLAGS', ['-stdlib=libc++'])
    clasp_build_libraries = [
            '-lclangAnalysis',
            '-lclangARCMigrate',
            '-lclangAST',
            '-lclangASTMatchers',
            '-lclangBasic',
            '-lclangCodeGen',
            '-lclangDriver',
            '-lclangDynamicASTMatchers',
            '-lclangEdit',
            '-lclangFormat',
            '-lclangFrontend',
            '-lclangFrontendTool',
            '-lclangIndex',
            '-lclangLex',
            '-lclangParse',
            '-lclangRewrite',
            '-lclangRewriteFrontend',
            '-lclangSema',
            '-lclangSerialization',
            '-lclangStaticAnalyzerCheckers',
            '-lclangStaticAnalyzerCore',
            '-lclangStaticAnalyzerFrontend',
            '-lclangTooling',
            '-lclangToolingCore',
            '-lboost_filesystem',
            '-lboost_regex',
            '-lboost_date_time',
            '-lboost_program_options',
            '-lboost_system',
            '-lboost_iostreams',
            '-lz',
            '-lncurses',
            '-lreadline' ]
    sep = " "
    cfg.recurse('plugins')
    cfg.define("CLASP_BUILD_LIBRARIES", sep.join(clasp_build_libraries))
    cfg.env.append_value('LINKFLAGS', clasp_build_libraries)
    env_copy = cfg.env.derive()
    for v in VARIANTS:
        vi = eval(v+"()")
        print("Setting up executable_name: %s" % vi.executable_name)
        vi.configure(cfg,env_copy)
        print("cfg.bldnode = %s\n" % cfg.bldnode)
        print("variant = %s\n" % v )
        build_plugin_headers("%s/%s" % (cfg.bldnode,v),cfg.plugins_headers)



def build(bld):
#    bld(name='myInclude', export_includes=[bld.env.MY_MYSDK, 'include'])
    if not bld.variant:
        bld.fatal('Call waf with build_variant')
    print("In build(bld)")
    bld.clasp_source_files = []
    bld.recurse('src')
    bld.plugins_source_files = []
    bld.recurse('plugins')
    bld.recurse('src/main')
    source_files = bld.clasp_source_files + bld.plugins_source_files
#    print("DEBUG recursed source_files = %s" % source_files)
#    bld.program(source=all_files(),target="clasp")
    variant = eval(bld.variant+"()")
    bld.env = bld.all_envs[bld.variant]
    bld.variant_obj = variant
    clasp_executable = bld.path.find_or_declare(variant.executable_name)
    lto_debug_info = bld.path.find_or_declare('%s.lto.o' % variant.executable_name)
    intrinsics_info = bld.path.find_or_declare('%s-intrinsics.lbc'%variant.bitcode_name)
    print("Building with variant = %s" % variant)
    bld.program(source=source_files,target=[clasp_executable,lto_debug_info])
    bld.install_as('${PREFIX}/%s/%s' % (os.getenv("EXECUTABLE_DIR"),variant.executable_name), variant.executable_name, chmod=Utils.O755)

#        files = lisp_source_files(clasp_executable.abspath(),"aclasp")
#        print("aclasp source files: %s" % files)
#        if (variant.stage>=0):
        #     bld.add_group()
        #     aclasp_lisp_sources = ''
        #     bld(rule='i%s -I -f ecl-min -e "(compile-aclasp)" -e "(link-aclasp)"' % variant.bitcode_name,target="a%s"%variant.bitcode_name)
        #     bld.install_as('${PREFIX}/Contents/cxx-bitcode/%s-all.lbc' % variant.bitcode_name, '%s-all.lbc'%variant.bitcode_name)
        #     bld.install_as('${PREFIX}/Contents/cxx-bitcode/%s-intrinsics.lbc' % variant.bitcode_name, '%s-intrinsics.lbc'%variant.bitcode_name)
        # if (variant.stage>=1):
        #     bld.add_group()
        #     bld(rule='a%s -e "(compile-link-bclasp)"' % variant.bitcode_name,target="b%s"%variant.bitcode_name)
        # if (variant.stage>=2):
        #     bld.add_group()
        #     bld(rule='b%s -e "(compile-link-cclasp)"' % variant.bitcode_name,target="c%s"%variant.bitcode_name)
        # install

from waflib import TaskGen
from waflib import Task

class llvm_link(Task.Task):
    def run(self):
        all_inputs = StringIO.StringIO()
        for x in self.inputs:
            all_inputs.write(' %s' % x)
        return self.exec_command('llvm-link %s -o %s' % ( all_inputs.getvalue(), self.outputs[0]) )


class preprocess(Task.Task):
    run_str = '${CXX} -E -DSCRAPING ${ARCH_ST:ARCH} ${CXXFLAGS} ${CPPFLAGS} ${FRAMEWORKPATH_ST:FRAMEWORKPATH} ${CPPPATH_ST:INCPATHS} ${DEFINES_ST:DEFINES} ${CXX_SRC_F}${SRC} ${CXX_TGT_F}${TGT[0].abspath()}'
    ext_out = ['.i']

    def keyword(ctx):
        return "Preprocessing"

class sif(Task.Task):
    clasp_home = os.getenv("CLASP_HOME")
    run_str = 'generate-one-sif ${SRC[0].abspath()} ${TGT[0].abspath()}'
    ext_out = ['.sif']

    def keyword(ctx):
        return "Scraping"

class generated_headers(Task.Task):
    ext_out = ['.h']
    def run(self):
        cmd = StringIO.StringIO()
        cmd.write('generate-headers-from-all-sifs src/main/')
        for f in self.inputs:
            cmd.write(' %s' % f.abspath())
        return self.exec_command(cmd.getvalue())

class link_bitcode(Task.Task):
    ext_out = ['.lbc']
    def run(self):
        cmd = StringIO.StringIO()
        cmd.write('llvm-link ')
        for f in self.inputs:
            cmd.write(' %s' % f.abspath())
        cmd.write(' -o %s' % self.outputs[0])
        return self.exec_command(cmd.getvalue())


# Have all 'cxx' targets have 'include' in their include paths.
@TaskGen.feature('cxx')
@TaskGen.after('process_source')
def preprocess_task_generator(self):
    compiled_tasks = self.compiled_tasks
    all_sif_files = []
    all_o_files = []
    intrinsics_o = None
    for task in self.compiled_tasks:
        if ( task.__class__.__name__ == 'cxx' ):
            for node in task.inputs:
                i_node = node.change_ext('.i')
                sif_node = node.change_ext('.sif')
                self.create_task('preprocess',node,[i_node])
                self.source.append(i_node)
                self.create_task('sif',i_node,[sif_node])
                all_sif_files.append(sif_node)
            for node in task.outputs:
#                print("node = %s" % node.get_src())
                if ( node.get_src().__str__()[:len('intrinsics.cc')] == 'intrinsics.cc' ):
                    intrinsics_o = node
                all_o_files.append(node)
        if ( task.__class__.__name__ == 'c' ):
            for node in task.outputs:
                all_o_files.append(node)
    generated_headers = [ 'generated/c-wrappers.h',
                          'generated/enum_inc.h',
                          'generated/initClassesAndMethods_inc.h',
                          'generated/initFunctions_inc.h',
                          'generated/initializers_inc.h',
                          'generated/sourceInfo_inc.h',
                          'generated/symbols_scraped_inc.h'
#                          'src/include/clasp/main/generated/symbols_scraped_inc.h'
    ]
    nodes = []
    for x in generated_headers:
        nodes.append(self.path.find_or_declare(x))
    self.create_task('generated_headers',all_sif_files,nodes)
    variant = self.bld.variant_obj
    library_node = self.path.find_or_declare('%s-all.lbc' % variant.bitcode_name )
    intrinsics_library_node = self.path.find_or_declare('%s-intrinsics.lbc' % variant.bitcode_name )
    print("library_node = %s" % library_node.abspath())
    self.create_task('link_bitcode',all_o_files,library_node)
    self.create_task('link_bitcode',[intrinsics_o],intrinsics_library_node)


def init(ctx):
    from waflib.Build import BuildContext, CleanContext, InstallContext, UninstallContext
    for x in VARIANTS:
	for y in (BuildContext, CleanContext, InstallContext, UninstallContext):
	    name = y.__name__.replace('Context','').lower()
	    class tmp(y):
		cmd = name + '_' + x
		variant = x

    def buildall(ctx):
	import waflib.Options
	for x in VARIANTS:
            var = 'build_'+x
	    waflib.Options.commands.insert(0, var)
