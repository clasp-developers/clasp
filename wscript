import sys
import os
import StringIO
from waflib.extras import clang_compilation_database
from waflib import Utils

top = '.'
out = 'wbuild'
APPNAME = 'clasp'
VERSION = '0.0'

DARWIN_OS = 'darwin'
LINUX_OS = 'linux'
STAGE_CHARS = [ 'i', 'a', 'b', 'c' ]
GCS = [ 'boehm',
        'boehmdc',
        'mpsprep',
        'mps' ]
DEBUG_CHARS = [ 'o', 'd' ]

def stage_value(ctx,s):
    if ( s == 'i' ):
        sval = 0
    elif ( s == 'a' ):
        sval = 1
    elif ( s == 'b' ):
        sval = 2
    elif ( s == 'c' ):
        sval = 3
    else:
        ctx.fatal("Illegal stage: %s" % s)
    return sval


class variant():
    def executable_name(self,stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        if (not (use_stage>='a' and use_stage <= 'z')):
            raise Exception("Bad stage: %s"% use_stage)
        return '%s%s-%s' % (use_stage,self.gc_name,self.debug_char)
    def variant_dir(self):
        return "%s_%s"%(self.gc_name,self.debug_char)
    def variant_name(self):
        return self.gc_name
    def bitcode_name(self):
        return "%s-%s" % (self.gc_name,self.debug_char)
    def configure_for_release(self,cfg):
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O3', '-g' ])
        cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_RELEASE_LINKFLAGS").split())
    def configure_for_debug(self,cfg):
        cfg.define("_DEBUG_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_DEBUG_LINKFLAGS").split())
    def common_setup(self,cfg):
        cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.executable_name())
        if ( self.debug_char == 'r' ):
            self.configure_for_release(cfg)
        else:
            self.configure_for_debug(cfg)
        cfg.write_config_header("%s/config.h"%self.variant_dir(),remove=True)
        
def configure_clasp(cfg,variant):
    include_path = "%s/%s/%s/src/include/clasp/main/" % (cfg.path.abspath(),out,variant.variant_dir()) #__class__.__name__)
#    print("Including from %s" % include_path )
    cfg.env.append_value("CXXFLAGS", ['-I%s' % include_path])
    cfg.define("EXECUTABLE_NAME",variant.executable_name())
    cfg.define("BITCODE_NAME",variant.bitcode_name())
    cfg.define("VARIANT_NAME",variant.variant_name())
    
class boehm_o(variant):
    gc_name = 'boehm'
    debug_char = 'o'
    def configure(self,cfg,env_copy):
        cfg.setenv(self.variant_dir(), env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_BOEHM",1)
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        self.common_setup(cfg)

class boehm_d(variant):
    gc_name = 'boehm'
    debug_char = 'd'
    def configure(self,cfg,env_copy):
        cfg.setenv("boehm_d", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_BOEHM",1)
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        self.common_setup(cfg)

class boehmdc_o(variant):
    gc_name = 'boehmdc'
    debug_char = 'o'
    def configure(self,cfg,env_copy):
        cfg.setenv("boehmdc_o", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_BOEHM",1)
        cfg.define("USE_CXX_DYNAMIC_CAST",1)
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        self.common_setup(cfg)

    
class boehmdc_d(variant):
    gc_name = 'boehmdc'
    debug_char = 'd'
    def configure(self,cfg,env_copy):
        cfg.setenv("boehmdc_d", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_BOEHM",1)
        cfg.define("USE_CXX_DYNAMIC_CAST",1)
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        self.common_setup(cfg)

    
        
class mpsprep_o(variant):
    gc_name = 'mpsprep'
    debug_char = 'o'
    def configure(self,cfg,env_copy):
        cfg.setenv("mpsprep_o", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_MPS",1)
        cfg.define("RUNNING_GC_BUILDER",1)
        self.common_setup(cfg)
        
class mpsprep_d(variant):
    gc_name = 'mpsprep'
    debug_char = 'd'
    def configure(self,cfg,env_copy):
        cfg.setenv("mpsprep_d", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_MPS",1)
        cfg.define("RUNNING_GC_BUILDER",1)
        self.common_setup(cfg)

class mps_o(variant):
    gc_name = 'mps'
    debug_char = 'o'
    def configure(self,cfg,env_copy):
        cfg.setenv("mps_o", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_MPS",1)
        self.common_setup(cfg)

class mps_d(variant):
    gc_name = 'mps'
    debug_char = 'd'
    def configure(self,cfg,env_copy):
        cfg.setenv("mps_d", env=env_copy.derive())
        configure_clasp(cfg,self)
        cfg.define("USE_MPS",1)
        self.common_setup(cfg)


class iboehm_o(boehm_o):
    stage_char = 'i'
class aboehm_o(boehm_o):
    stage_char = 'a'
class bboehm_o(boehm_o):
    stage_char = 'b'
class cboehm_o(boehm_o):
    stage_char = 'c'
    
class iboehm_d(boehm_d):
    stage_char = 'i'
class aboehm_d(boehm_d):
    stage_char = 'a'
class bboehm_d(boehm_d):
    stage_char = 'b'
class cboehm_d(boehm_d):
    stage_char = 'c'

class iboehmdc_o(boehmdc_o):
    stage_char = 'i'
class aboehmdc_o(boehmdc_o):
    stage_char = 'a'
class bboehmdc_o(boehmdc_o):
    stage_char = 'b'
class cboehmdc_o(boehmdc_o):
    stage_char = 'c'

class iboehmdc_d(boehmdc_d):
    stage_char = 'i'
class aboehmdc_d(boehmdc_d):
    stage_char = 'a'
class bboehmdc_d(boehmdc_d):
    stage_char = 'b'
class cboehmdc_d(boehmdc_d):
    stage_char = 'c'


class imps_o(mps_o):
    stage_char = 'i'
class amps_o(mps_o):
    stage_char = 'a'
class bmps_o(mps_o):
    stage_char = 'b'
class cmps_o(mps_o):
    stage_char = 'c'
    
class imps_d(mps_d):
    stage_char = 'i'
class amps_d(mps_d):
    stage_char = 'a'
class bmps_d(mps_d):
    stage_char = 'b'
class cmps_d(mps_d):
    stage_char = 'c'

class impsprep_o(mpsprep_o):
    stage_char = 'i'
class ampsprep_o(mpsprep_o):
    stage_char = 'a'
class bmpsprep_o(mpsprep_o):
    stage_char = 'b'
class cmpsprep_o(mpsprep_o):
    stage_char = 'c'

class impsprep_d(mpsprep_d):
    stage_char = 'i'
class ampsprep_d(mpsprep_d):
    stage_char = 'a'
class bmpsprep_d(mpsprep_d):
    stage_char = 'b'
class cmpsprep_d(mpsprep_d):
    stage_char = 'c'
    
import subprocess

def lisp_source_files(exe,stage):
    print("About to execute: %s" % exe)
    proc = subprocess.Popen([exe, "-I", "-f", "ecl-min", "-e", "(source-files-%s)"%stage_name(stage), "-e", "(quit)"], stdout=subprocess.PIPE, shell=True)
    (out, err) = proc.communicate()
    print( "source-files-%s: %s" % (stage_name(stage),out))
    return out

def options(opt):
    opt.load('compiler_cxx')
    opt.load('compiler_c')
#    opt.add_option('--gc', default='boehm', help='Garbage collector');


def build_plugin_headers(root,headers_list):
#    print("DEBUG build_plugin_headers directory = %s\n" % root )
#    print("DEBUG build_plugin_headers headers_list=%s\n"% headers_list)
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
    if (cfg.env['DEST_OS'] == DARWIN_OS ):
        cfg.define("_TARGET_OS_DARWIN",1)
    elif (cfg.env['DEST_OS'] == LINUX_OS ):
        cfg.define("_TARGET_OS_LINUX",1);
    else:
        raise Exception("Unknown OS %s"%cfg.env['DEST_OS'])
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
    for gc in GCS:
        for debug_char in DEBUG_CHARS:
            variant = gc+"_"+debug_char
            variant_instance = eval("i"+variant+"()")
            print("Setting up variant: %s" % variant_instance.variant_dir())
            variant_instance.configure(cfg,env_copy)
            build_plugin_headers("%s/%s" % (cfg.bldnode,variant),cfg.plugins_headers)

def build(bld):
#    bld(name='myInclude', export_includes=[bld.env.MY_MYSDK, 'include'])
    if not bld.variant:
        bld.fatal('Call waf with build_variant')
    stage = bld.stage
    stage_val = stage_value(bld,stage)
    print("Building stage --> %s" % stage)
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
    clasp_executable = bld.path.find_or_declare(variant.executable_name(stage='i'))
    lto_debug_info = bld.path.find_or_declare('%s.lto.o' % variant.executable_name(stage='i'))
    intrinsics_info = bld.path.find_or_declare('%s-intrinsics.lbc'%variant.bitcode_name())
    print("Building with variant = %s" % variant)
    bld.program(source=source_files,target=[clasp_executable,lto_debug_info])
    if (bld.env['DEST_OS'] == DARWIN_OS):
        iclasp_dsym = bld.path.find_or_declare("%s.dSYM"%variant.executable_name(stage='i'))
    if (stage_val >= 1):
        print("About to add compile_aclasp")
        cmp_aclasp = compile_aclasp(env=bld.env)
        cmp_aclasp.set_inputs(clasp_executable)
        aclasp_executable = bld.path.find_or_declare(variant.executable_name(stage='a'))
        cmp_aclasp.set_outputs(aclasp_executable)
        bld.add_to_group(cmp_aclasp)
        if (stage_val >= 2):
            print("About to add compile_bclasp")
            cmp_bclasp = compile_bclasp(env=bld.env)
            cmp_bclasp.set_inputs(aclasp_executable)
            bclasp_executable = bld.path.find_or_declare(variant.executable_name(stage='b'))
            cmp_bclasp.set_outputs(bclasp_executable)
            bld.add_to_group(cmp_bclasp)
            if (stage_val >= 3):
                print("About to add compile_cclasp")
                cmp_cclasp = compile_cclasp(env=bld.env)
                cmp_cclasp.set_inputs(bclasp_executable)
                cclasp_executable = bld.path.find_or_declare(variant.executable_name(stage='c'))
                cmp_cclasp.set_outputs(cclasp_executable)
                bld.add_to_group(cmp_cclasp)

#        bld.program(features='dsymutil',source=[clasp_executable],target=[iclasp_dsym])
#    bld.install_as('${PREFIX}/%s/%s' % (os.getenv("EXECUTABLE_DIR"),variant.executable_name(stage=0), variant.executable_name, chmod=Utils.O755)
        
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

class dsymutil(Task.Task):
    run_str = '${DSYMUTIL} ${SRC}'
    color = 'BLUE';
    after = [ 'cprogram', 'cxxprogram' ]
    before = ['inst']

@TaskGen.feature('dsymutil')
@TaskGen.after('apply_link')
def add_dsymutil_task(self):
    try:
        link_task = self.link_task
        print("add_dsymutil_task link_task.inputs = %s" % link_task.inputs)
        print("add_dsymutil_task link_task.outputs = %s" % link_task.outputs)
    except AttributeError:
        return
    self.create_task('dsymutil',link_task.outputs[0])

class compile_aclasp(Task.Task):
    def run(self):
        print("In compile_aclasp %s -> %s" % (self.inputs[0].abspath(),self.outputs[0].abspath()))
        cmd = '%s -I -f ecl-min -N -e "(compile-aclasp)" -e "(quit)"' % self.inputs[0].abspath()
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_aclasp, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile aclasp using... '

class compile_bclasp(Task.Task):
    def run(self):
        print("In compile_bclasp %s -> %s" % (self.inputs[0].abspath(),self.outputs[0].abspath()))
        cmd = '%s -N -e "(compile-bclasp)" -e "(quit)"' % self.inputs[0].abspath()
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_bclasp, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile bclasp using... '

class compile_cclasp(Task.Task):
    def run(self):
        print("In compile_cclasp %s -> %s" % (self.inputs[0].abspath(),self.outputs[0].abspath()))
        cmd = '%s -N -e "(compile-cclasp)" -e "(quit)"' % self.inputs[0].abspath()
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_cclasp, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile cclasp using... '
    

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
    library_node = self.path.find_or_declare('%s-all.lbc' % variant.bitcode_name() )
    intrinsics_library_node = self.path.find_or_declare('%s-intrinsics.lbc' % variant.bitcode_name() )
    print("library_node = %s" % library_node.abspath())
    self.create_task('link_bitcode',all_o_files,library_node)
    self.create_task('link_bitcode',[intrinsics_o],intrinsics_library_node)


def init(ctx):
    from waflib.Build import BuildContext, CleanContext, InstallContext, UninstallContext
    for s in STAGE_CHARS:
        for gc in GCS:
            for debug_char in DEBUG_CHARS:
	        for y in (BuildContext, CleanContext, InstallContext, UninstallContext):
	            name = y.__name__.replace('Context','').lower()
	            class tmp(y):
                        variant = gc+'_'+debug_char
		        cmd = name + '_' + s + variant
                        stage = s

    def buildall(ctx):
	import waflib.Options
        for s in STAGE_CHARS:
	    for gc in GCS:
                for debug_char in DEBUG_CHARS:
                    var = 'build_'+s+x+'_'+debug_char
	            waflib.Options.commands.insert(0, var)

