import subprocess
from waflib.Tools.compiler_cxx import cxx_compiler
from waflib.Tools.compiler_c import c_compiler
#cxx_compiler['linux'] = ['clang++']
#c_compiler['linux'] = ['clang']

import sys
import os
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO
from waflib.extras import clang_compilation_database
from waflib.Errors import ConfigurationError
from waflib import Utils

top = '.'
out = 'wbuild'
APP_NAME = 'clasp'
VERSION = '0.0'
DARWIN_OS = 'darwin'
LINUX_OS = 'linux'
    
STAGE_CHARS = [ 's', 'i', 'a', 'b', 'c' ]

GCS = [ 'boehm',
        'boehmdc',
        'mpsprep',
        'mps' ]
DEBUG_CHARS = [ 'o', 'd' ]

LLVM_LIBRARIES = []

CLANG_LIBRARIES = [
            'clangASTMatchers',
            'clangDynamicASTMatchers',
            'clangIndex',
            'clangTooling',
            'clangFormat',
            'clangToolingCore',
            'clangBasic',
            'clangCodeGen',
            'clangDriver',
            'clangFrontend',
            'clangFrontendTool',
            'clangCodeGen',
            'clangRewriteFrontend',
            'clangARCMigrate',
            'clangStaticAnalyzerFrontend',
            'clangFrontend',
            'clangDriver',
            'clangParse',
            'clangSerialization',
            'clangSema',
            'clangEdit',
            'clangStaticAnalyzerCheckers',
            'clangStaticAnalyzerCore',
            'clangAnalysis',
            'clangAST',
            'clangRewrite',
            'clangLex',
            'clangBasic',
 ]

BOOST_LIBRARIES = [
            'boost_filesystem',
            'boost_regex',
            'boost_date_time',
            'boost_program_options',
            'boost_system',
            'boost_iostreams']

def libraries_as_link_flags(fmt,libs):
    all_libs = StringIO()
    for x in libs:
        all_libs.write(" ")
        all_libs.write(fmt % x)
    return all_libs.getvalue()

def generate_dsym_files(name,path):
    info_plist = path.find_or_declare("Contents/Info.plist")
    dwarf_file = path.find_or_declare("Contents/Resources/DWARF/%s"%name)
    print("info_plist = %s" % info_plist)
    print("dwarf_file = %s" % dwarf_file)
    return [info_plist,dwarf_file]

def stage_value(ctx,s):
    if ( s == 's' ):
        sval = -1
    elif ( s == 'i' ):
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

def configure_clasp(cfg,variant):
    include_path = "%s/%s/%s/src/include/clasp/main/" % (cfg.path.abspath(),out,variant.variant_dir()) #__class__.__name__)
#    print("Including from %s" % include_path )
    cfg.env.append_value("CXXFLAGS", ['-I%s' % include_path])
    cfg.env.append_value("CFLAGS", ['-I%s' % include_path])
    cfg.define("EXECUTABLE_NAME",variant.executable_name())
    cfg.define("APP_NAME",APP_NAME)
    cfg.define("BITCODE_NAME",variant.bitcode_name())
    cfg.define("VARIANT_NAME",variant.variant_name())
    cfg.define("BUILD_STLIB", libraries_as_link_flags(cfg.env.STLIB_ST,cfg.env.STLIB))
    cfg.define("BUILD_LIB", libraries_as_link_flags(cfg.env.LIB_ST,cfg.env.LIB))
    cfg.define("BUILD_LINKFLAGS", ' '.join(cfg.env.LINKFLAGS))
#    cfg.define("DEBUG_STARTUP",1)

def strip_libs(libs):
    result = []
    split_libs = libs.split()
    for lib in split_libs:
        result.append("%s" % str(lib[2:]))
    return result 
    
class variant(object):
    def executable_name(self,stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        if (not (use_stage>='a' and use_stage <= 'z')):
            raise Exception("Bad stage: %s"% use_stage)
        return '%s%s-%s-%s' % (use_stage,APP_NAME,self.gc_name,self.debug_char)
    def fasl_name(self,stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        if (not (use_stage>='a' and use_stage <= 'z')):
            raise Exception("Bad stage: %s"% use_stage)
        return '%s%s-%s-%s-image.fasl' % (use_stage,APP_NAME,self.gc_name,self.debug_char)
    def common_lisp_bitcode_name(self,stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        if (not (use_stage>='a' and use_stage <= 'z')):
            raise Exception("Bad stage: %s"% use_stage)
        return '%s%s-%s-common-lisp.bc' % (use_stage,APP_NAME,self.gc_name)
    def variant_dir(self):
        return "%s_%s"%(self.gc_name,self.debug_char)
    def variant_name(self):
        return self.gc_name
    def bitcode_name(self):
        return "%s-%s" % (self.gc_name,self.debug_char)
    def cxx_all_bitcode_name(self):
        return '%s-all-cxx.a' % self.bitcode_name()
    def intrinsics_bitcode_name(self):
        return '%s-intrinsics-cxx.a' % self.bitcode_name()
    def configure_for_release(self,cfg):
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O3', '-g' ])
        if (os.getenv("CLASP_RELEASE_CXXFLAGS") != None):
            cfg.env.append_value('CXXFLAGS', os.getenv("CLASP_RELEASE_CXXFLAGS").split() )
        if (os.getenv("CLASP_RELEASE_LINKFLAGS") != None):
            cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_RELEASE_LINKFLAGS").split())
    def configure_for_debug(self,cfg):
        cfg.define("_DEBUG_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O0', '-g' ])
        if (os.getenv("CLASP_DEBUG_CXXFLAGS") != None):
            cfg.env.append_value('CXXFLAGS', os.getenv("CLASP_DEBUG_CXXFLAGS").split() )
        if (os.getenv("CLASP_DEBUG_LINKFLAGS") != None):
            cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_DEBUG_LINKFLAGS").split())
    def common_setup(self,cfg):
        if ( self.debug_char == 'o' ):
            self.configure_for_release(cfg)
        else:
            self.configure_for_debug(cfg)
        configure_clasp(cfg,self)
        cfg.write_config_header("%s/config.h"%self.variant_dir(),remove=True)

class boehm(variant):
    def configure_variant(self,cfg,env_copy):
        cfg.define("USE_BOEHM",1)
        if (cfg.env['DEST_OS'] == DARWIN_OS ):
            cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.executable_name())
        print("Setting up boehm library cfg.env.STLIB_BOEHM = %s " % cfg.env.STLIB_BOEHM)
        print("Setting up boehm library cfg.env.LIB_BOEHM = %s" % cfg.env.LIB_BOEHM)
        if (cfg.env.LIB_BOEHM == [] ):
            cfg.env.append_value('STLIB',cfg.env.STLIB_BOEHM)
        else:
            cfg.env.append_value('LIB',cfg.env.LIB_BOEHM)
        self.common_setup(cfg)        
    
class boehm_o(boehm):
    gc_name = 'boehm'
    debug_char = 'o'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv(self.variant_dir(), env=env_copy.derive())
        super(boehm_o,self).configure_variant(cfg,env_copy)

class boehm_d(boehm):
    gc_name = 'boehm'
    debug_char = 'd'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("boehm_d", env=env_copy.derive())
        super(boehm_d,self).configure_variant(cfg,env_copy)

class boehmdc_o(boehm):
    gc_name = 'boehmdc'
    debug_char = 'o'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("boehmdc_o", env=env_copy.derive())
        cfg.define("USE_CXX_DYNAMIC_CAST",1)
        super(boehmdc_o,self).configure_variant(cfg,env_copy)

class boehmdc_d(boehm):
    gc_name = 'boehmdc'
    debug_char = 'd'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("boehmdc_d", env=env_copy.derive())
        cfg.define("USE_CXX_DYNAMIC_CAST",1)
        super(boehmdc_d,self).configure_variant(cfg,env_copy)

class mps(variant):
    def configure_variant(self,cfg,env_copy):
        cfg.define("USE_MPS",1)
        if (cfg.env['DEST_OS'] == DARWIN_OS ):
            cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.executable_name())
        print("Setting up boehm library cfg.env.STLIB_BOEHM = %s " % cfg.env.STLIB_BOEHM)
        print("Setting up boehm library cfg.env.LIB_BOEHM = %s" % cfg.env.LIB_BOEHM)
        if (cfg.env.LIB_BOEHM == [] ):
            cfg.env.append_value('STLIB',cfg.env.STLIB_BOEHM)
        else:
            cfg.env.append_value('LIB',cfg.env.LIB_BOEHM)
        self.common_setup(cfg)
        
class mpsprep_o(mps):
    gc_name = 'mpsprep'
    debug_char = 'o'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mpsprep_o", env=env_copy.derive())
        cfg.define("RUNNING_GC_BUILDER",1)
        super(mpsprep_o,self).configure_variant(cfg,env_copy)
        
class mpsprep_d(mps):
    gc_name = 'mpsprep'
    debug_char = 'd'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mpsprep_d", env=env_copy.derive())
        cfg.define("RUNNING_GC_BUILDER",1)
        super(mpsprep_d,self).configure_variant(cfg,env_copy)

class mps_o(mps):
    gc_name = 'mps'
    debug_char = 'o'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mps_o", env=env_copy.derive())
        super(mps_o,self).configure_variant(cfg,env_copy)

class mps_d(mps):
    gc_name = 'mps'
    debug_char = 'd'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mps_d", env=env_copy.derive())
        super(mps_d,self).configure_variant(cfg,env_copy)

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

def options(cfg):
    cfg.load('compiler_cxx')
    cfg.load('compiler_c')

def configure(cfg):
    global LLVM_LIBRARIES
    cfg.check_waf_version(mini='1.7.5')
    path = os.getenv("PATH").split(":")
    externals_clasp_dir = os.getenv("EXTERNALS_CLASP_DIR")
    if (externals_clasp_dir != None):
        externals_clasp_bin_dir = "%s/build/release/bin" % externals_clasp_dir
        path.insert(0,externals_clasp_bin_dir)
        print(" Inserted %s into the start of path" % externals_clasp_bin_dir)
    print( "path = %s" % path)
    llvm_config = cfg.find_program("llvm-config",var="LLVM_CONFIG",path_list=path)
    print( "llvm_config = %s" % cfg.env.LLVM_CONFIG)
#    print("llvm_config = %s" % llvm_config[0])
    llvm_libs_bytes = subprocess.Popen([llvm_config[0], "--libs"], stdout=subprocess.PIPE).communicate()[0]
    LLVM_LIBRARIES = strip_libs(llvm_libs_bytes.decode("ASCII",'ignore'))
#    clang_bin_dir_bytes = subprocess.Popen([llvm_config[0], "--bindir"], stdout=subprocess.PIPE).communicate()[0]
#    clang_bin_dir = str(clang_bin_dir_bytes.decode("ASCII",'ignore').split()[0])
#    print("clang_bin_dir = %s" % clang_bin_dir)
    global cxx_compiler, c_compiler
    cxx_compiler['linux'] = ["clang++"]
    c_compiler['linux'] = ["clang"]
    cfg.load('compiler_cxx')
    cfg.load('compiler_c')
    llvm_release_lib_dir_bytes = subprocess.Popen([llvm_config[0], "--libdir"], stdout=subprocess.PIPE).communicate()[0]
    llvm_release_lib_dir = str(llvm_release_lib_dir_bytes.decode("ASCII",'ignore').split()[0])
    print("llvm_release_lib_dir = %s" % llvm_release_lib_dir )
### Uncommenting these checks causes problems-- AttributeError: 'BuildContext' object has no attribute 'variant_obj'
    cfg.check_cxx(lib='gmpxx gmp'.split(), cflags='-Wall', uselib_store='GMP')
    try:
        cfg.check_cxx(stlib='gc', cflags='-Wall', uselib_store='BOEHM')
    except ConfigurationError:
        cfg.check_cxx(lib='gc', cflags='-Wall', uselib_store='BOEHM')
    cfg.check_cxx(stlib='z', cflags='-Wall', uselib_store='Z')
    if (cfg.env['DEST_OS'] == LINUX_OS ):
        cfg.check_cxx(lib='dl', cflags='-Wall', uselib_store='DL')
    cfg.check_cxx(lib='ncurses', cflags='-Wall', uselib_store='NCURSES')
    cfg.check_cxx(lib='m', cflags='-Wall', uselib_store='M')
    cfg.check_cxx(stlib=BOOST_LIBRARIES, cflags='-Wall', uselib_store='BOOST')
    cfg.plugins_include_dirs = []
    cfg.plugins_gcinterface_include_files = []
    cfg.plugins_stlib = []
    cfg.plugins_lib = []
    cfg.recurse('plugins')
    link_flag = "-L%s" % llvm_release_lib_dir
    print("link_flag = %s" % link_flag )
    cfg.env.append_value('LINKFLAGS', [link_flag])
    cfg.check_cxx(stlib=LLVM_LIBRARIES, cflags='-Wall', uselib_store='LLVM', stlibpath = llvm_release_lib_dir )
    cfg.check_cxx(stlib=CLANG_LIBRARIES, cflags='-Wall', uselib_store='CLANG', stlibpath = llvm_release_lib_dir )
    cfg.env.append_value('CXXFLAGS', ['-I./'])
    cfg.env.append_value('CFLAGS', ['-I./'])
    if ('program_name' in cfg.__dict__):
        pass
    else:
        cfg.env.append_value('CXXFLAGS', ['-I%s/include/clasp/main/'% cfg.path.abspath() ])
# Check if GC_enumerate_reachable_objects_inner is available
# If so define  BOEHM_GC_ENUMERATE_REACHABLE_OBJECTS_INNER_AVAILABLE
#
    cfg.define("USE_CLASP_DYNAMIC_CAST",1)
    cfg.define("BUILDING_CLASP",1)
    print("cfg.env['DEST_OS'] == %s\n" % cfg.env['DEST_OS'])
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
#    cfg.define("EXPAT",1)
    cfg.define("INCLUDED_FROM_CLASP",1)
    cfg.define("INHERITED_FROM_SRC",1)
    cfg.define("LLVM_VERSION_X100",390)
    cfg.define("LLVM_VERSION","3.9")
    cfg.define("NDEBUG",1)
#    cfg.define("READLINE",1)
    cfg.define("USE_AMC_POOL",1)
    cfg.define("USE_EXPENSIVE_BACKTRACE",1)
    cfg.define("X86_64",1)
    cfg.define("DEBUG_FUNCTION_CALL_COUNTER",1)
    cfg.define("_ADDRESS_MODEL_64",1)
    cfg.define("__STDC_CONSTANT_MACROS",1)
    cfg.define("__STDC_FORMAT_MACROS",1)
    cfg.define("__STDC_LIMIT_MACROS",1)
#    cfg.env.append_value('CXXFLAGS', ['-v'] )
#    cfg.env.append_value('CFLAGS', ['-v'] )
    includes = [ 'include/' ]
    includes = includes + cfg.plugins_include_dirs
    includes_from_build_dir = []
    for x in includes:
        includes_from_build_dir.append("-I%s/%s"%(cfg.path.abspath(),x))
#    print("DEBUG includes_from_build_dir = %s\n" % includes_from_build_dir)
    cfg.env.append_value('CXXFLAGS', [ '-std=c++11'])
#    cfg.env.append_value('CXXFLAGS', ["-D_GLIBCXX_USE_CXX11_ABI=1"])
    cfg.env.append_value('CXXFLAGS', includes_from_build_dir )
    cfg.env.append_value('CFLAGS', includes_from_build_dir )
    cfg.env.append_value('CXXFLAGS', '-flto=thin')
    cfg.env.append_value('CFLAGS', '-flto=thin')
    if (cfg.env['DEST_OS'] == LINUX_OS ):
        cfg.env.append_value('LINKFLAGS', '-fuse-ld=gold')
        cfg.env.append_value('LINKFLAGS', ['-stdlib=libstdc++'])
        cfg.env.append_value('LINKFLAGS', ['-lstdc++'])
        cfg.env.append_value('LINKFLAGS', '-pthread')
    elif (cfg.env['DEST_OS'] == DARWIN_OS ):
        cfg.env.append_value('LINKFLAGS', ['-Wl,-export_dynamic'])
        cfg.env.append_value('LINKFLAGS', ['-Wl,-stack_size,0x1000000'])
        lto_library_name = cfg.env.cxxshlib_PATTERN % "LTO"  # libLTO.<os-dep-extension>
        lto_library = "%s/%s" % ( llvm_release_lib_dir, lto_library_name)
        cfg.env.append_value('LINKFLAGS',"-Wl,-lto_library,%s" % lto_library)
        cfg.env.append_value('LINKFLAGS', ['-lc++'])
        cfg.env.append_value('LINKFLAGS', ['-stdlib=libc++'])
    cfg.env.append_value('INCLUDES', ['/usr/include'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-macro-redefined'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-deprecated-register'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-expansion-to-defined'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-return-type-c-linkage'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-invalid-offsetof'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-#pragma-messages'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-inconsistent-missing-override'] )
    cfg.env.append_value('LINKFLAGS', ['-flto=thin'])
    cfg.env.append_value('LIBPATH', ['/usr/lib'])
    cfg.env.append_value('LINKFLAGS', ['-fvisibility=default'])
    cfg.env.append_value('LINKFLAGS', ['-rdynamic'])
    sep = " "
    cfg.env.append_value('STLIB', cfg.plugins_stlib)
    cfg.env.append_value('LIB', cfg.plugins_lib)
    cfg.env.append_value('STLIB', cfg.env.STLIB_CLANG)
    cfg.env.append_value('STLIB', cfg.env.STLIB_LLVM)
    cfg.env.append_value('STLIB', cfg.env.STLIB_BOOST)
    cfg.env.append_value('STLIB', cfg.env.STLIB_Z)
    if (cfg.env['DEST_OS'] == LINUX_OS ):
        cfg.env.append_value('LIB', cfg.env.LIB_DL)
    cfg.env.append_value('LIB', cfg.env.LIB_NCURSES)
    cfg.env.append_value('LIB', cfg.env.LIB_M)
    cfg.env.append_value('LIB', cfg.env.LIB_GMP)
    print("cfg.env.STLIB = %s" % cfg.env.STLIB)
    print("cfg.env.LIB = %s" % cfg.env.LIB)
    env_copy = cfg.env.derive()
    for gc in GCS:
        for debug_char in DEBUG_CHARS:
            variant = gc+"_"+debug_char
            variant_instance = eval("i"+variant+"()")
            print("Setting up variant: %s" % variant_instance.variant_dir())
            variant_instance.configure_variant(cfg,env_copy)
            build_plugin_headers("%s/%s" % (cfg.bldnode,variant),cfg.plugins_gcinterface_include_files)

def copy_tree(bld,src,dest):
    print("Copy tree src = %s" % src)
    print("          dest= %s" % dest)

def build(bld):
#    bld(name='myInclude', export_includes=[bld.env.MY_MYSDK, 'include'])
    if not bld.variant:
        bld.fatal('Call waf with build_variant')
    stage = bld.stage
    stage_val = stage_value(bld,stage)
    print("Building stage --> %s" % stage)
    bld.clasp_source_files = []
    bld.recurse('src')
    bld.plugins_include_files = []
    bld.plugins_source_files = []
    bld.recurse('plugins')
    bld.recurse('src/main')
    source_files = bld.clasp_source_files + bld.plugins_source_files
#    print("DEBUG recursed source_files = %s" % source_files)
    bld.install_files('${PREFIX}/Contents/Resources/source-code/',source_files,relative_trick=True,cwd=bld.path)
#    print("bld.path = %s"%bld.path)
    clasp_headers = bld.path.ant_glob("include/clasp/**/*.h")
#    print("clasp_headers = %s" % clasp_headers)
    bld.install_files('${PREFIX}/Contents/Resources/source-code/',clasp_headers,relative_trick=True,cwd=bld.path)
#    lisp_source = bld.path.ant_glob("src/lisp/**/*.l*")
#    bld.install_files('${PREFIX}/Contents/Resources/source-code/',lisp_source,relative_trick=True,cwd=bld.path)
    variant = eval(bld.variant+"()")
    bld.env = bld.all_envs[bld.variant]
    bld.variant_obj = variant
#    test_linking_source = bld.clasp_test_linking_source_file
#    test_linking_executable = bld.path.find_or_declare('test_linking')
#    print("test_linking_source = %s" % test_linking_source)
#    print("test_linking_executable = %s" % test_linking_executable)
#    bld.program(source=[test_linking_source],target=test_linking_executable)
    print("Building with variant = %s" % variant)
    if (stage_val >= 0):
        iclasp_executable = bld.path.find_or_declare(variant.executable_name(stage='i'))
        if (bld.env['DEST_OS'] == LINUX_OS ):
            executable_dir = "bin"
            bld.program(source=source_files,target=[iclasp_executable],install_path='${PREFIX}/bin')
        elif (bld.env['DEST_OS'] == DARWIN_OS ):
            iclasp_lto_o = bld.path.find_or_declare('%s.lto.o' % variant.executable_name(stage='i'))
            executable_dir = "MacOS"
            bld.program(source=source_files,target=[iclasp_executable],install_path='${PREFIX}/MacOS')
            iclasp_dsym = bld.path.find_or_declare("%s.dSYM"%variant.executable_name(stage='i'))
            iclasp_dsym_files = generate_dsym_files(variant.executable_name(stage='i'),iclasp_dsym)
            dsymutil_iclasp = dsymutil(env=bld.env)
            dsymutil_iclasp.set_inputs([iclasp_executable,iclasp_lto_o])
            dsymutil_iclasp.set_outputs(iclasp_dsym_files)
            bld.add_to_group(dsymutil_iclasp)
            bld.install_files('${PREFIX}/%s/%s'%(executable_dir,iclasp_dsym.name),iclasp_dsym_files,relative_trick=True,cwd=iclasp_dsym)
        if (stage_val >= 1):
            print("About to add compile_aclasp")
            intrinsics_bitcode_node = bld.path.find_or_declare(variant.intrinsics_bitcode_name())
            cmp_aclasp = compile_aclasp(env=bld.env)
            cmp_aclasp.set_inputs([iclasp_executable,intrinsics_bitcode_node])
            aclasp_product = bld.path.find_or_declare(variant.fasl_name(stage='a'))
            cmp_aclasp.set_outputs(aclasp_product)
            bld.add_to_group(cmp_aclasp)
            bld.install_as('${PREFIX}/%s/%s' % (executable_dir, aclasp_product.name), aclasp_product)
            aclasp_common_lisp_bitcode = bld.path.find_or_declare(variant.common_lisp_bitcode_name(stage='a'))
            bld.install_as('${PREFIX}/Contents/Resources/lib/%s' % variant.common_lisp_bitcode_name(stage='a'), aclasp_common_lisp_bitcode)
            if (stage_val >= 2):
                print("About to add compile_bclasp")
                cmp_bclasp = compile_bclasp(env=bld.env)
                cmp_bclasp.set_inputs([iclasp_executable,aclasp_product,intrinsics_bitcode_node])
                bclasp_product = bld.path.find_or_declare(variant.fasl_name(stage='b'))
                cmp_bclasp.set_outputs(bclasp_product)
                bld.add_to_group(cmp_bclasp)
                bld.install_as('${PREFIX}/%s/%s' % (executable_dir, bclasp_product.name), bclasp_product)
                bclasp_common_lisp_bitcode = bld.path.find_or_declare(variant.common_lisp_bitcode_name(stage='b'))
                bld.install_as('${PREFIX}/Contents/Resources/lib/%s' % variant.common_lisp_bitcode_name(stage='b'), aclasp_common_lisp_bitcode)
                if (stage_val >= 3):
                    print("About to add compile_cclasp")
                    # Build cclasp
                    cmp_cclasp = compile_cclasp(env=bld.env)
                    cxx_all_bitcode_node = bld.path.find_or_declare(variant.cxx_all_bitcode_name())
                    cmp_cclasp.set_inputs([iclasp_executable,bclasp_product,cxx_all_bitcode_node])
                    cclasp_executable = bld.path.find_or_declare(variant.executable_name(stage='c'))
                    cclasp_lto_o = bld.path.find_or_declare('%s.lto.o' % variant.executable_name(stage='i'))
                    cmp_cclasp.set_outputs(cclasp_executable)
                    bld.add_to_group(cmp_cclasp)
                    cclasp_dsym = bld.path.find_or_declare("%s.dSYM"%variant.executable_name(stage='c'))
                    cclasp_dsym_files = generate_dsym_files(variant.executable_name(stage='c'),cclasp_dsym)
                    print("cclasp_dsym_files = %s" % cclasp_dsym_files)
                    dsymutil_cclasp = dsymutil(env=bld.env)
                    dsymutil_cclasp.set_inputs([cclasp_executable,cclasp_lto_o])
                    dsymutil_cclasp.set_outputs(cclasp_dsym_files)
                    bld.add_to_group(dsymutil_cclasp)
                    bld.install_files('${PREFIX}/%s/%s'%(executable_dir,cclasp_dsym.name),cclasp_dsym_files,relative_trick=True,cwd=cclasp_dsym)
                    bld.install_as('${PREFIX}/%s/%s' % (executable_dir, cclasp_executable.name), cclasp_executable, chmod=Utils.O755)
                    cclasp_common_lisp_bitcode = bld.path.find_or_declare(variant.common_lisp_bitcode_name(stage='c'))
                    bld.install_as('${PREFIX}/Contents/Resources/lib/%s' % variant.common_lisp_bitcode_name(stage='c'), aclasp_common_lisp_bitcode)
                    cmp_addons = compile_addons(env=bld.env)
                    cmp_addons.set_inputs(cclasp_executable)
                    asdf_fasl = bld.path.find_or_declare("cclasp-boehmdc/modules/asdf/asdf.fasl")
                    cmp_addons.set_outputs(asdf_fasl)
                    bld.add_to_group(cmp_addons)
                    bld.install_as('${PREFIX}/Contents/Resources/lib/cclasp-boehmdc/modules/asdf/asdf.fasl',asdf_fasl)


from waflib import TaskGen
from waflib import Task

class dsymutil(Task.Task):
    color = 'BLUE';
    def run(self):
        cmd = 'dsymutil %s' % self.inputs[0]
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)

#@TaskGen.feature('dsymutil')
#@TaskGen.after('apply_link')
#def add_dsymutil_task(self):
#    try:
#        link_task = self.link_task
#    except AttributeError:
#        return
#    self.create_task('dsymutil',link_task.outputs[0])

class compile_aclasp(Task.Task):
    def run(self):
        print("In compile_aclasp %s -> %s" % (self.inputs[0].abspath(),self.outputs[0].abspath()))
        cmd = '%s -I -f ecl-min -f debug-run-clang -N -e "(compile-aclasp :link-type :fasl)" -e "(quit)"' % self.inputs[0].abspath()
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_aclasp, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile aclasp using... '

#
# Use the aclasp fasl file
class compile_bclasp(Task.Task):
    def run(self):
        print("In compile_bclasp %s %s -> %s" % (self.inputs[0].abspath(),self.inputs[1].abspath(),self.outputs[0].abspath()))
#        cmd = '%s -N -e "(compile-bclasp)" -e "(quit)"' % self.inputs[0].abspath()
        cmd = '%s -i %s -N -f debug-run-clang -e "(compile-bclasp :link-type :fasl)" -e "(quit)"' % (self.inputs[0].abspath(), self.inputs[1].abspath())
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_bclasp, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile bclasp using... '

class compile_cclasp(Task.Task):
    def run(self):
        print("In compile_cclasp %s %s -> %s" % (self.inputs[0].abspath(),self.inputs[1].abspath(),self.outputs[0].abspath()))
#        cmd = '%s -N -e "(compile-cclasp)" -e "(quit)"' % self.inputs[0].abspath()
        cmd = '%s -i %s -f debug-run-clang -N -e "(compile-cclasp :link-type :executable)" -e "(quit)"' % (self.inputs[0].abspath(), self.inputs[1].abspath())
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_cclasp, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile cclasp using... '
    

class compile_addons(Task.Task):
    def run(self):
        print("In compile_addons %s -> %s" % (self.inputs[0].abspath(),self.outputs[0].abspath()))
        cmd = '%s -N -e "(core:compile-addons)" -e "(core:link-addons)" -e "(quit)"' % self.inputs[0].abspath()
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_addons, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile addons using... '
    

#class llvm_link(Task.Task):
#    def run(self):
#        all_inputs = StringIO()
#        for x in self.inputs:
#            all_inputs.write(' %s' % x)
#        return self.exec_command('llvm-ar a %s %s' % ( self.outputs[0], all_inputs.getvalue()) )

class link_bitcode(Task.Task):
    ext_out = ['.a']
    def run(self):
        all_inputs = StringIO()
        for f in self.inputs:
            all_inputs.write(' %s' % f.abspath())
        cmd = "llvm-ar q %s %s" % (self.outputs[0], all_inputs.getvalue())
#        print("link_bitcode command: %s" % cmd )
        return self.exec_command(cmd)

class preprocess(Task.Task):
    run_str = '${CXX} -E -DSCRAPING ${ARCH_ST:ARCH} ${CXXFLAGS} ${CPPFLAGS} ${FRAMEWORKPATH_ST:FRAMEWORKPATH} ${CPPPATH_ST:INCPATHS} ${DEFINES_ST:DEFINES} ${CXX_SRC_F}${SRC} ${CXX_TGT_F}${TGT[0].abspath()}'
    ext_out = ['.i']

    def keyword(ctx):
        return "Preprocessing"
    
class sif(Task.Task):
    run_str = 'generate-one-sif ${SRC[0].abspath()} ${TGT[0].abspath()}'
    ext_out = ['.sif']

    def keyword(ctx):
        return "Scraping"

class generated_headers(Task.Task):
    ext_out = ['.h']
    def run(self):
        cmd = StringIO()
        cmd.write('generate-headers-from-all-sifs src/main/')
        for f in self.inputs:
            cmd.write(' %s' % f.abspath())
        return self.exec_command(cmd.getvalue())

# Have all 'cxx' targets have 'include' in their include paths.
@TaskGen.feature('cxx')
@TaskGen.after('process_source')
def preprocess_task_generator(self):
    if ( not 'variant_obj' in self.bld.__dict__ ):
        return
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
#                print("node.name = %s" % node.name)
                if ( node.name[:len('intrinsics.cc')] == 'intrinsics.cc' ):
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
                          'generated/symbols_scraped_inc.h']
    nodes = []
    for x in generated_headers:
        nodes.append(self.path.find_or_declare(x))
    self.create_task('generated_headers',all_sif_files,nodes)
    self.bld.install_files('${PREFIX}/Contents/Resources/source-code/include/', nodes)
    variant = self.bld.variant_obj
    cxx_all_bitcode_node = self.path.find_or_declare(variant.cxx_all_bitcode_name())
    intrinsics_bitcode_node = self.path.find_or_declare(variant.intrinsics_bitcode_name())
    self.create_task('link_bitcode',all_o_files,cxx_all_bitcode_node)
    self.create_task('link_bitcode',[intrinsics_o],intrinsics_bitcode_node)
    self.bld.install_files('${PREFIX}/Contents/Resources/lib/%s'%variant.intrinsics_bitcode_name(),intrinsics_bitcode_node)
    self.bld.install_files('${PREFIX}/Contents/Resources/lib/%s'%variant.cxx_all_bitcode_name(),cxx_all_bitcode_node)


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

