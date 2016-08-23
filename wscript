import subprocess
from waflib.Tools import c_preproc
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
out = 'build'
APP_NAME = 'clasp'
VERSION = '0.0'
DARWIN_OS = 'darwin'
LINUX_OS = 'linux'
    
STAGE_CHARS = [ 's', 'i', 'a', 'b', 'c' ]

GCS = [ 'boehm',
        'boehmdc',
        'mpsprep',
        'mps' ]
# DEBUG_CHARS None == optimized
DEBUG_CHARS = [ None, 'd' ]

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
#    print("info_plist = %s" % info_plist)
#    print("dwarf_file = %s" % dwarf_file)
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
    elif ( s == 'rebuild' ):
        sval = 0
    elif ( s == 'dangerzone' ):
        sval = 0
    else:
        ctx.fatal("Illegal stage: %s" % s)
    return sval

def yadda(cfg):
    print("In Yadda")

def configure_clasp(cfg,variant):
#    include_path = "%s/%s/%s/src/include/clasp/main/" % (cfg.path.abspath(),out,variant.variant_dir()) #__class__.__name__)
#    cfg.env.append_value("CXXFLAGS", ['-I%s' % include_path])
#    cfg.env.append_value("CFLAGS", ['-I%s' % include_path])
    cfg.define("EXECUTABLE_NAME",variant.executable_name())
    cfg.define("CLASP_CLANG_PATH",os.getenv("CLASP_CLANG_PATH"))
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

def fix_lisp_paths(bld_path,out,variant,paths):
    nodes = []
    for p in paths:
        if ( p[:4] == "src/" ):
            file_name = p
            lsp_name = "%s.lsp"%file_name
            lsp_res = bld_path.find_resource(lsp_name)
            if (lsp_res == None):
                lsp_name = "%s.lisp"%file_name
                lsp_res = bld_path.find_resource(lsp_name)
#            print("Looking for file_name with .lsp or .lisp: %s  --> %s" % (file_name,lsp_res))
            assert lsp_res!=None, "lsp_res could not be resolved for file %s" % lsp_name
        else: # generated files
#            lsp_name = "%s/%s/%s.lisp"%(out,variant.variant_dir(),p)
            lsp_name = "%s.lisp"%(p)
            lsp_res = bld_path.find_or_declare(lsp_name)
#            print("Looking for generated file with .lisp: %s  --> %s" % (lsp_name,lsp_res))
            assert lsp_res!=None, "lsp_res could not be resolved for file %s" % lsp_name
        nodes.append(lsp_res)
    return nodes

def debug_ext(c):
    if (c):
        return "-%s"%c
    return ""
def debug_dir_ext(c):
    if (c):
        return "_%s"%c
    return ""

class variant(object):
    def debug_extension(self):
        return debug_ext(self.debug_char)
    def debug_dir_extension(self):
        return debug_dir_ext(self.debug_char)
    def executable_name(self,stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        if (not (use_stage>='a' and use_stage <= 'z')):
            raise Exception("Bad stage: %s"% use_stage)
        return '%s%s-%s%s' % (use_stage,APP_NAME,self.gc_name,self.debug_extension())
    def extension_headers_node(self,bld):
        return bld.path.find_or_declare("generated/extension_headers.h")
    def fasl_name(self,stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        if (not (use_stage>='a' and use_stage <= 'z')):
            raise Exception("Bad stage: %s"% use_stage)
        return '%s%s-%s%s-image.fasl' % (use_stage,APP_NAME,self.gc_name,self.debug_extension())
    def fasl_dir(self,stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        return 'fasl/%s%s-%s' % (use_stage,APP_NAME,self.gc_name)
    def common_lisp_bitcode_name(self,stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        if (not (use_stage>='a' and use_stage <= 'z')):
            raise Exception("Bad stage: %s"% use_stage)
        return '%s%s-%s-common-lisp.bc' % (use_stage,APP_NAME,self.gc_name)
    def variant_dir(self):
        return "%s%s"%(self.gc_name,self.debug_dir_extension())
    def variant_name(self):
        return self.gc_name
    def bitcode_name(self):
        return "%s%s"%(self.gc_name,self.debug_extension())
    def cxx_all_bitcode_name(self):
        return 'fasl/%s-all-cxx.a' % self.bitcode_name()
    def intrinsics_bitcode_name(self):
        return 'fasl/%s-intrinsics-cxx.a' % self.bitcode_name()
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
        if ( self.debug_char == None ):
            self.configure_for_release(cfg)
        else:
            self.configure_for_debug(cfg)
        configure_clasp(cfg,self)
        cfg.write_config_header("%s/config.h"%self.variant_dir(),remove=True)

class boehm_base(variant):
    def configure_variant(self,cfg,env_copy):
        cfg.define("USE_BOEHM",1)
        if (cfg.env['DEST_OS'] == DARWIN_OS ):
            cfg.env.append_value('LDFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.executable_name())
        print("Setting up boehm library cfg.env.STLIB_BOEHM = %s " % cfg.env.STLIB_BOEHM)
        print("Setting up boehm library cfg.env.LIB_BOEHM = %s" % cfg.env.LIB_BOEHM)
        if (cfg.env.LIB_BOEHM == [] ):
            cfg.env.append_value('STLIB',cfg.env.STLIB_BOEHM)
        else:
            cfg.env.append_value('LIB',cfg.env.LIB_BOEHM)
        self.common_setup(cfg)        
    
class boehm(boehm_base):
    gc_name = 'boehm'
    debug_char = None
    def configure_variant(self,cfg,env_copy):
        cfg.setenv(self.variant_dir(), env=env_copy.derive())
        super(boehm,self).configure_variant(cfg,env_copy)

class boehm_d(boehm_base):
    gc_name = 'boehm'
    debug_char = 'd'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("boehm_d", env=env_copy.derive())
        super(boehm_d,self).configure_variant(cfg,env_copy)

class boehmdc(boehm_base):
    gc_name = 'boehmdc'
    debug_char = None
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("boehmdc", env=env_copy.derive())
        cfg.define("USE_CXX_DYNAMIC_CAST",1)
        super(boehmdc,self).configure_variant(cfg,env_copy)

class boehmdc_d(boehm_base):
    gc_name = 'boehmdc'
    debug_char = 'd'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("boehmdc_d", env=env_copy.derive())
        cfg.define("USE_CXX_DYNAMIC_CAST",1)
        super(boehmdc_d,self).configure_variant(cfg,env_copy)

class mps_base(variant):
    def configure_variant(self,cfg,env_copy):
        cfg.define("USE_MPS",1)
        if (cfg.env['DEST_OS'] == DARWIN_OS ):
            cfg.env.append_value('LDFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.executable_name())
#        print("Setting up boehm library cfg.env.STLIB_BOEHM = %s " % cfg.env.STLIB_BOEHM)
#        print("Setting up boehm library cfg.env.LIB_BOEHM = %s" % cfg.env.LIB_BOEHM)
#        if (cfg.env.LIB_BOEHM == [] ):
#            cfg.env.append_value('STLIB',cfg.env.STLIB_BOEHM)
#        else:
#            cfg.env.append_value('LIB',cfg.env.LIB_BOEHM)
        self.common_setup(cfg)
        
class mpsprep(mps_base):
    gc_name = 'mpsprep'
    debug_char = None
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mpsprep", env=env_copy.derive())
        cfg.define("RUNNING_GC_BUILDER",1)
        super(mpsprep,self).configure_variant(cfg,env_copy)
        
class mpsprep_d(mps_base):
    gc_name = 'mpsprep'
    debug_char = 'd'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mpsprep_d", env=env_copy.derive())
        cfg.define("RUNNING_GC_BUILDER",1)
        super(mpsprep_d,self).configure_variant(cfg,env_copy)

class mps(mps_base):
    gc_name = 'mps'
    debug_char = None
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mps", env=env_copy.derive())
        super(mps,self).configure_variant(cfg,env_copy)

class mps_d(mps_base):
    gc_name = 'mps'
    debug_char = 'd'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mps_d", env=env_copy.derive())
        super(mps_d,self).configure_variant(cfg,env_copy)

class iboehm(boehm):
    stage_char = 'i'
class aboehm(boehm):
    stage_char = 'a'
class bboehm(boehm):
    stage_char = 'b'
class cboehm(boehm):
    stage_char = 'c'
    
class iboehm_d(boehm_d):
    stage_char = 'i'
class aboehm_d(boehm_d):
    stage_char = 'a'
class bboehm_d(boehm_d):
    stage_char = 'b'
class cboehm_d(boehm_d):
    stage_char = 'c'

class iboehmdc(boehmdc):
    stage_char = 'i'
class aboehmdc(boehmdc):
    stage_char = 'a'
class bboehmdc(boehmdc):
    stage_char = 'b'
class cboehmdc(boehmdc):
    stage_char = 'c'

class iboehmdc_d(boehmdc_d):
    stage_char = 'i'
class aboehmdc_d(boehmdc_d):
    stage_char = 'a'
class bboehmdc_d(boehmdc_d):
    stage_char = 'b'
class cboehmdc_d(boehmdc_d):
    stage_char = 'c'


class imps(mps):
    stage_char = 'i'
class amps(mps):
    stage_char = 'a'
class bmps(mps):
    stage_char = 'b'
class cmps(mps):
    stage_char = 'c'
    
class imps_d(mps_d):
    stage_char = 'i'
class amps_d(mps_d):
    stage_char = 'a'
class bmps_d(mps_d):
    stage_char = 'b'
class cmps_d(mps_d):
    stage_char = 'c'

class impsprep(mpsprep):
    stage_char = 'i'
class ampsprep(mpsprep):
    stage_char = 'a'
class bmpsprep(mpsprep):
    stage_char = 'b'
class cmpsprep(mpsprep):
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
    cfg.extensions_include_dirs = []
    cfg.extensions_gcinterface_include_files = []
    cfg.extensions_stlib = []
    cfg.extensions_lib = []
    cfg.extensions_names = []
    cfg.recurse('extensions')
    print("cfg.extensions_names before sort = %s" % cfg.extensions_names)
    cfg.extensions_names = sorted(cfg.extensions_names)
    print("cfg.extensions_names after sort = %s" % cfg.extensions_names)
    clasp_gc_filename = "clasp_gc.cc"
    if (len(cfg.extensions_names)>0):
        clasp_gc_filename = "clasp_gc_%s.cc" % ("_".join(cfg.extensions_names))
    print("clasp_gc_filename = %s"%clasp_gc_filename)
    cfg.define("CLASP_GC_FILENAME",clasp_gc_filename)
    link_flag = "-L%s" % llvm_release_lib_dir
    print("link_flag = %s" % link_flag )
    cfg.env.append_value('LINKFLAGS', [link_flag])
    cfg.check_cxx(stlib=LLVM_LIBRARIES, cflags='-Wall', uselib_store='LLVM', stlibpath = llvm_release_lib_dir )
    cfg.check_cxx(stlib=CLANG_LIBRARIES, cflags='-Wall', uselib_store='CLANG', stlibpath = llvm_release_lib_dir )
    cfg.env.append_value('CXXFLAGS', ['-I./'])
    cfg.env.append_value('CFLAGS', ['-I./'])
#    if ('program_name' in cfg.__dict__):
#        pass
#    else:
#        cfg.env.append_value('CXXFLAGS', ['-I%s/include/clasp/main/'% cfg.path.abspath() ])
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
#    includes = [ 'include/' ]
#    includes = includes + cfg.plugins_include_dirs
#    includes_from_build_dir = []
#    for x in includes:
#        includes_from_build_dir.append("-I%s/%s"%(cfg.path.abspath(),x))
#    cfg.env.append_value('CXXFLAGS', includes_from_build_dir )
#    cfg.env.append_value('CFLAGS', includes_from_build_dir )
#    print("DEBUG includes_from_build_dir = %s\n" % includes_from_build_dir)
    cfg.env.append_value('CXXFLAGS', [ '-std=c++11'])
#    cfg.env.append_value('CXXFLAGS', ["-D_GLIBCXX_USE_CXX11_ABI=1"])
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
    cfg.env.append_value('STLIB', cfg.extensions_stlib)
    cfg.env.append_value('LIB', cfg.extensions_lib)
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
            if (debug_char==None):
                variant = gc
            else:
                variant = gc+"_"+debug_char
            variant_instance = eval("i"+variant+"()")
            print("Setting up variant: %s" % variant_instance.variant_dir())
            variant_instance.configure_variant(cfg,env_copy)

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
    bld.clasp_aclasp = []
    bld.clasp_bclasp = []
    bld.clasp_cclasp = []
    bld.recurse('src')
    bld.extensions_include_dirs = []
    bld.extensions_include_files = []
    bld.extensions_source_files = []
    bld.extensions_gcinterface_include_files = []
    bld.recurse('extensions')
    bld.recurse('src/main')
    source_files = bld.clasp_source_files + bld.extensions_source_files
    bld.install_files('${PREFIX}/Contents/Resources/source-code/',source_files,relative_trick=True,cwd=bld.path)
    print("bld.path = %s"%bld.path)
    clasp_headers = bld.path.ant_glob("include/clasp/**/*.h")
    bld.install_files('${PREFIX}/Contents/Resources/source-code/',clasp_headers,relative_trick=True,cwd=bld.path)
    variant = eval(bld.variant+"()")
    bld.env = bld.all_envs[bld.variant]
    bld.variant_obj = variant
    include_dirs = ['.']
    include_dirs.append("%s/src/main/" % bld.path.abspath())
    include_dirs.append("%s/include/" % (bld.path.abspath()))
    include_dirs.append("%s/%s/%s/generated/" % (bld.path.abspath(),out,variant.variant_dir()))
    include_dirs = include_dirs + bld.extensions_include_dirs
    print("include_dirs = %s" % include_dirs)
    print("Building with variant = %s" % variant)
    wscript_node = bld.path.find_resource("wscript")
    extension_headers_node = variant.extension_headers_node(bld)
    print("extension_headers_node = %s" % extension_headers_node.abspath())
    extensions_task = build_extension_headers(env=bld.env)
    inputs = [wscript_node] + bld.extensions_gcinterface_include_files
    extensions_task.set_inputs(inputs)
    extensions_task.set_outputs([extension_headers_node])
    bld.add_to_group(extensions_task)
    # Always build the C++ code
    iclasp_executable = bld.path.find_or_declare(variant.executable_name(stage='i'))
    intrinsics_bitcode_node = bld.path.find_or_declare(variant.intrinsics_bitcode_name())
    if (bld.env['DEST_OS'] == LINUX_OS ):
        executable_dir = "bin"
        bld_task = bld.program(source=source_files,
                               includes=include_dirs,
                               target=[iclasp_executable],install_path='${PREFIX}/bin')
    elif (bld.env['DEST_OS'] == DARWIN_OS ):
        iclasp_lto_o = bld.path.find_or_declare('%s.lto.o' % variant.executable_name(stage='i'))
        executable_dir = "MacOS"
        bld_task = bld.program(source=source_files,
                               includes=include_dirs,
                               target=[iclasp_executable],install_path='${PREFIX}/MacOS')
        iclasp_dsym = bld.path.find_or_declare("%s.dSYM"%variant.executable_name(stage='i'))
        iclasp_dsym_files = generate_dsym_files(variant.executable_name(stage='i'),iclasp_dsym)
        dsymutil_iclasp = dsymutil(env=bld.env)
        dsymutil_iclasp.set_inputs([iclasp_executable,iclasp_lto_o])
        dsymutil_iclasp.set_outputs(iclasp_dsym_files)
        bld.add_to_group(dsymutil_iclasp)
        bld.install_files('${PREFIX}/%s/%s'%(executable_dir,iclasp_dsym.name),iclasp_dsym_files,relative_trick=True,cwd=iclasp_dsym)
    if (stage_val >= 1):   
        print("About to add compile_aclasp")
        cmp_aclasp = compile_aclasp(env=bld.env)
#        print("clasp_aclasp as nodes = %s" % fix_lisp_paths(bld.path,out,variant,bld.clasp_aclasp))
        cmp_aclasp.set_inputs([iclasp_executable,intrinsics_bitcode_node]+fix_lisp_paths(bld.path,out,variant,bld.clasp_aclasp))
        aclasp_common_lisp_bitcode = bld.path.find_or_declare(variant.common_lisp_bitcode_name(stage='a'))
        cmp_aclasp.set_outputs(aclasp_common_lisp_bitcode)
        bld.add_to_group(cmp_aclasp)
        lnk_aclasp = link_fasl(env=bld.env)
        lnk_aclasp.set_inputs([intrinsics_bitcode_node,aclasp_common_lisp_bitcode])
        aclasp_link_product = bld.path.find_or_declare(variant.fasl_name(stage='a'))
        lnk_aclasp.set_outputs([aclasp_link_product])
        bld.add_to_group(lnk_aclasp)
        bld.install_as('${PREFIX}/%s/%s' % (executable_dir, aclasp_link_product.name), aclasp_link_product)
        bld.install_as('${PREFIX}/Contents/Resources/lib/%s' % variant.common_lisp_bitcode_name(stage='a'), aclasp_common_lisp_bitcode)
    if (stage_val >= 2):
        print("About to add compile_bclasp")
        cmp_bclasp = compile_bclasp(env=bld.env)
        cmp_bclasp.set_inputs([iclasp_executable,aclasp_link_product,intrinsics_bitcode_node]+fix_lisp_paths(bld.path,out,variant,bld.clasp_bclasp))
        bclasp_common_lisp_bitcode = bld.path.find_or_declare(variant.common_lisp_bitcode_name(stage='b'))
        cmp_bclasp.set_outputs(bclasp_common_lisp_bitcode)
        bld.add_to_group(cmp_bclasp)
        bclasp_link_product = bld.path.find_or_declare(variant.fasl_name(stage='b'))
        lnk_bclasp = link_fasl(env=bld.env)
        lnk_bclasp.set_inputs([intrinsics_bitcode_node,bclasp_common_lisp_bitcode])
        lnk_bclasp.set_outputs([bclasp_link_product])
        bld.add_to_group(lnk_bclasp)
        bld.install_as('${PREFIX}/%s/%s' % (executable_dir, bclasp_link_product.name), bclasp_link_product)
        bld.install_as('${PREFIX}/Contents/Resources/lib/%s' % variant.common_lisp_bitcode_name(stage='b'), aclasp_common_lisp_bitcode)
    if (stage_val >= 3):
        print("About to add compile_cclasp")
        # Build cclasp
        cmp_cclasp = compile_cclasp(env=bld.env)
        cxx_all_bitcode_node = bld.path.find_or_declare(variant.cxx_all_bitcode_name())
        cmp_cclasp.set_inputs([iclasp_executable,bclasp_link_product,cxx_all_bitcode_node]+fix_lisp_paths(bld.path,out,variant,bld.clasp_cclasp))
        cclasp_common_lisp_bitcode = bld.path.find_or_declare(variant.common_lisp_bitcode_name(stage='c'))
        cmp_cclasp.set_outputs([cclasp_common_lisp_bitcode])
        bld.add_to_group(cmp_cclasp)
    if (stage == 'rebuild' or stage == 'dangerzone'):
        print("!------------------------------------------------------------")
        print("!   You have entered the dangerzone!  ")
        print("!   While you wait...  https://www.youtube.com/watch?v=kyAn3fSs8_A")
        print("!------------------------------------------------------------")
        # Build cclasp
        recmp_cclasp = recompile_cclasp(env=bld.env)
        recmp_cclasp.set_inputs(fix_lisp_paths(bld.path,out,variant,bld.clasp_cclasp))
        cclasp_common_lisp_bitcode = bld.path.find_or_declare(variant.common_lisp_bitcode_name(stage='c'))
        recmp_cclasp.set_outputs([cclasp_common_lisp_bitcode])
        bld.add_to_group(recmp_cclasp)
    if (stage == 'dangerzone' or stage == 'rebuild' or stage_val >= 3):
        cclasp_fasl = bld.path.find_or_declare(variant.fasl_name(stage='c'))
        lnk_cclasp_fasl = link_fasl(env=bld.env)
        lnk_cclasp_fasl.set_inputs([intrinsics_bitcode_node,cclasp_common_lisp_bitcode])
        lnk_cclasp_fasl.set_outputs([cclasp_fasl])
        bld.add_to_group(lnk_cclasp_fasl)
        bld.install_as('${PREFIX}/%s/%s' % (executable_dir, cclasp_fasl.name), cclasp_fasl)
    if (stage == 'rebuild' or stage_val >= 3):
        lnk_cclasp_exec = link_executable(env=bld.env)
        cxx_all_bitcode_node = bld.path.find_or_declare(variant.cxx_all_bitcode_name())
        lnk_cclasp_exec.set_inputs([cxx_all_bitcode_node,cclasp_common_lisp_bitcode])
        cclasp_executable = bld.path.find_or_declare(variant.executable_name(stage='c'))
        if ( bld.env['DEST_OS'] == DARWIN_OS ):
            cclasp_lto_o = bld.path.find_or_declare('%s.lto.o' % variant.executable_name(stage='c'))
            lnk_cclasp_exec.set_outputs([cclasp_executable,cclasp_lto_o])
        elif (bld.env['DEST_OS'] == LINUX_OS ):
            cclasp_lto_o = None
            lnk_cclasp_exec.set_outputs(cclasp_executable)
        bld.add_to_group(lnk_cclasp_exec)
        if ( bld.env['DEST_OS'] == DARWIN_OS ):
            cclasp_dsym = bld.path.find_or_declare("%s.dSYM"%variant.executable_name(stage='c'))
            cclasp_dsym_files = generate_dsym_files(variant.executable_name(stage='c'),cclasp_dsym)
            print("cclasp_dsym_files = %s" % cclasp_dsym_files)
            dsymutil_cclasp = dsymutil(env=bld.env)
            dsymutil_cclasp.set_inputs([cclasp_executable,cclasp_lto_o])
            dsymutil_cclasp.set_outputs(cclasp_dsym_files)
            bld.add_to_group(dsymutil_cclasp)
            bld.install_files('${PREFIX}/%s/%s'%(executable_dir,cclasp_dsym.name),cclasp_dsym_files,relative_trick=True,cwd=cclasp_dsym)
        bld.install_as('${PREFIX}/%s/%s' % (executable_dir, cclasp_executable.name), cclasp_executable, chmod=Utils.O755)
        bld.symlink_as('${PREFIX}/%s/clasp' % executable_dir, '%s' % cclasp_executable.name)
        cclasp_common_lisp_bitcode = bld.path.find_or_declare(variant.common_lisp_bitcode_name(stage='c'))
        bld.install_as('${PREFIX}/Contents/Resources/lib/%s' % variant.common_lisp_bitcode_name(stage='c'), cclasp_common_lisp_bitcode)
        # Build serve-event
        serve_event_fasl = bld.path.find_or_declare("%s/src/lisp/modules/serve-event/serve-event.fasl" % variant.fasl_dir(stage='c'))
        cmp_serve_event = compile_module(env=bld.env)
        cmp_serve_event.set_inputs([cclasp_executable] + fix_lisp_paths(bld.path,out,variant,["src/lisp/modules/serve-event/serve-event"]))
        cmp_serve_event.set_outputs(serve_event_fasl)
        bld.add_to_group(cmp_serve_event)
        bld.install_as('${PREFIX}/Contents/Resources/lib/%s/src/lisp/modules/serve-event/serve-event.fasl'%variant.fasl_dir(stage="c"),serve_event_fasl)
        # Build ASDF
        asdf_fasl = bld.path.find_or_declare("%s/src/lisp/modules/asdf/asdf.fasl" % variant.fasl_dir(stage='c'))
        cmp_asdf = compile_module(env=bld.env)
        cmp_asdf.set_inputs([cclasp_executable] + fix_lisp_paths(bld.path,out,variant,["src/lisp/modules/asdf/build/asdf"]))
        cmp_asdf.set_outputs(asdf_fasl)
        bld.add_to_group(cmp_asdf)
        bld.install_as('${PREFIX}/Contents/Resources/lib/%s/src/lisp/modules/asdf/asdf.fasl'%variant.fasl_dir(stage="c"),asdf_fasl)
        build_node = bld.path.find_dir(out)
        print("build_node = %s" % build_node)
        clasp_symlink_node = build_node.make_node("clasp")
        print("clasp_symlink_node =  %s" % clasp_symlink_node)
        if (os.path.islink(clasp_symlink_node.abspath())):
            os.unlink(clasp_symlink_node.abspath())
        os.symlink(cclasp_executable.abspath(),clasp_symlink_node.abspath())

from waflib import TaskGen
from waflib import Task

class dsymutil(Task.Task):
    color = 'BLUE';
    def run(self):
        cmd = 'dsymutil %s' % self.inputs[0]
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)

class link_fasl(Task.Task):
    def run(self):
        if (self.env['DEST_OS'] == DARWIN_OS ):
            cmd = "%s %s %s -flto=thin -flat_namespace -undefined suppress -bundle -o %s" % (self.env.CXX[0],self.inputs[0].abspath(),self.inputs[1].abspath(),self.outputs[0].abspath())
        elif (self.env['DEST_OS'] == LINUX_OS ):
            cmd = "%s %s %s -flto=thin -fuse-ld=gold -shared -o %s" % (self.env.CXX[0],self.inputs[0].abspath(),self.inputs[1].abspath(),self.outputs[0].abspath())
        else:
            self.fatal("Illegal DEST_OS: %s" % self.env['DEST_OS'])
        print(" link_fasl cmd: %s\n" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(link_fasl, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Link fasl using... '

class link_executable(Task.Task):
    def run(self):
        if (self.env['DEST_OS'] == DARWIN_OS ):
            cmd = "%s %s %s %s %s %s -flto=thin -o %s -Wl,-object_path_lto,%s" % (self.env.CXX[0],self.inputs[0].abspath(),self.inputs[1].abspath(),' '.join(self.env['LINKFLAGS']),libraries_as_link_flags(self.env.STLIB_ST,self.env.STLIB), libraries_as_link_flags(self.env.LIB_ST,self.env.LIB),self.outputs[0].abspath(),self.outputs[1].abspath())
        elif (self.env['DEST_OS'] == LINUX_OS ):
            cmd = "%s %s %s %s %s %s -flto=thin -fuse-ld=gold -o %s" % (self.env.CXX[0],self.inputs[0].abspath(),self.inputs[1].abspath(),' '.join(self.env['LINKFLAGS']),libraries_as_link_flags(self.env.STLIB_ST,self.env.STLIB), libraries_as_link_flags(self.env.LIB_ST,self.env.LIB),self.outputs[0].abspath())
        else:
            self.fatal("Illegal DEST_OS: %s" % self.env['DEST_OS'])
        print(" link_executable cmd: %s\n" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(link_executable, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Link executable using... '

    
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
        print("In compile_aclasp %s -> %s" % (self.inputs[0],self.outputs[0]))
        cmd = [self.inputs[0].abspath(),
               "-I",
               "-f", "ecl-min",
               "-f", "clasp-builder",
               "-f", "debug-run-clang",
               "-N",
               "-e", "(compile-aclasp :output-file #P\"%s\")" % self.outputs[0],
               "-e", "(quit)",
               "--" ] + self.bld.clasp_aclasp
        print("  compile_aclasp cmd: %s" % cmd)
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
        print("In compile_bclasp %s %s -> %s" % (self.inputs[0],self.inputs[1],self.outputs[0]))
#        cmd = '%s -i %s -N -f clasp-builder -f debug-run-clang -e "(compile-bclasp :link-type :bc)" -e "(quit)"' % (self.inputs[0].abspath(), self.inputs[1].abspath())
        cmd = [self.inputs[0].abspath(),
               "-i", self.inputs[1].abspath(),
               "-N",
               "-f", "clasp-builder",
               "-f", "debug-run-clang",
               "-e", "(compile-bclasp :output-file #P\"%s\")" % self.outputs[0],
               "-e", "(quit)",
               "--" ] + self.bld.clasp_bclasp
        print("  compile_bclasp cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_bclasp, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile bclasp using... '

class compile_cclasp(Task.Task):
    def run(self):
        print("In compile_cclasp %s %s -> %s" % (self.inputs[0].abspath(),self.inputs[1].abspath(),self.outputs[0].abspath()))
        cmd = [self.inputs[0].abspath(),
               "-i", self.inputs[1].abspath(),
               "-N",
               "-f", "clasp-builder",
               "-f", "debug-run-clang",
               "-e", "(compile-cclasp :output-file #P\"%s\")" % self.outputs[0],
               "-e", "(quit)",
               "--" ] + self.bld.clasp_cclasp
        print("  compile_cclasp cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_cclasp, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile cclasp using... '

class recompile_cclasp(Task.Task):
    def run(self):
        print("In recompile_cclasp -> %s" % self.outputs[0].abspath())
#        cclasp_exe = self.bld.find_program("cclasp")
#        print("cclasp_exe = %s"%cclasp_exe)
#        cmd = 'cclasp -f debug-run-clang -N -R %s/%s/%s -e "(recompile-cclasp :link-type :bc)" -e "(quit)"' % (self.bld.path.abspath(),out,self.bld.variant_obj.variant_dir())
        cmd = ["cclasp",
               "-N",
               "-f", "clasp-builder",
               "-f", "debug-run-clang",
               "-f", "ignore-extensions",
               "-R", "%s/%s/%s" % (self.bld.path.abspath(),out,self.bld.variant_obj.variant_dir()),
               "-e", "(recompile-cclasp :output-file #P\"%s\")" % self.outputs[0],
               "-e", "(quit)",
               "--" ] + self.bld.clasp_cclasp
        print(" recompile_clasp cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(recompile_cclasp, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Recompile cclasp using... '

class compile_addons(Task.Task):
    def run(self):
        print("In compile_addons %s -> %s" % (self.inputs[0].abspath(),self.outputs[0].abspath()))
        cmd = '%s -f ignore-extensions -f clasp-builder -f debug-run-clang -N -e "(core:compile-addons)" -e "(core:link-addons)" -e "(quit)"' % self.inputs[0].abspath()
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_addons, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile addons using... '

# Generate bitcode for module
# inputs = [cclasp_executable,source-code]
# outputs = [fasl_file]
class compile_module(Task.Task):
    def run(self):
        print("In compile_module %s -> %s" % (self.inputs[0].abspath(),self.outputs[0].abspath()))
        cmd = [self.inputs[0].abspath(),
               "-f", "ignore-extensions",
               "-f", "debug-run-clang",
               "-e", "(compile-file #P\"%s\" :output-file #P\"%s\" :output-type :fasl)" % (self.inputs[1], self.outputs[0]),
               "-e", "(quit)"]
        print("  cmd: %s" % cmd)
        return self.exec_command(cmd)
    def exec_command(self, cmd, **kw):
        kw['stdout'] = sys.stdout
        return super(compile_module, self).exec_command(cmd, **kw)
    def keyword(self):
        return 'Compile module using... '
                               

#class llvm_link(Task.Task):
#    def run(self):
#        all_inputs = StringIO()
#        for x in self.inputs:
#            all_inputs.write(' %s' % x)
#        return self.exec_command('llvm-ar a %s %s' % ( self.outputs[0], all_inputs.getvalue()) )

class build_extension_headers(Task.Task):
#    print("DEBUG build_extension_headers directory = %s\n" % root )
#    print("DEBUG build_extension_headers headers_list=%s\n"% headers_list)
    def run(self):
        fout = open(self.outputs[0].abspath(),"w")
        fout.write("// Generated by wscript build_extension_headers - Do not modify!!\n" )
        for x in self.inputs[1:]:
            if ( x != None ):
                fout.write("#include \"%s\"\n" % x.abspath())
        fout.close()
                               
class link_bitcode(Task.Task):
    ext_out = ['.a']
    def run(self):
        all_inputs = StringIO()
        for f in self.inputs:
            all_inputs.write(' %s' % f.abspath())
        cmd = "llvm-ar ru %s %s" % (self.outputs[0], all_inputs.getvalue())
#        print("link_bitcode command: %s" % cmd )
        return self.exec_command(cmd)
    def __str__(self):
        return "link_bitcode - linking all object(bitcode) files."
#        master = self.generator.bld.producer
#        return "[%d/%d] Processing link_bitcode - all object files\n" % (master.processed-1,master.total)
    

class scrape_with_preproc_scan(Task.Task):
    run_str = 'preprocess-to-sif ${TGT[0].abspath()} ${CXX} -E -DSCRAPING ${ARCH_ST:ARCH} ${CXXFLAGS} ${CPPFLAGS} ${FRAMEWORKPATH_ST:FRAMEWORKPATH} ${CPPPATH_ST:INCPATHS} ${DEFINES_ST:DEFINES} ${CXX_SRC_F}${SRC}'
    ext_out = ['.sif']
    shell = False

    def scan(self):
        saved_env = self.env
        self.env = self.env.derive()
        self.env.DEFINES = list(self.env.DEFINES)+["SCRAPING=1"]
        scan_result = c_preproc.scan(self)
        self.env = saved_env
        return scan_result
                               
    def keyword(ctx):
        return "Scraping with preproc.scan"

class generated_headers(Task.Task):
#    ext_out = ['.h']
    def run(self):
        cmd = StringIO()
        cmd.write('generate-headers-from-all-sifs src/main/')
        for f in self.inputs:
            cmd.write(' %s' % f.abspath())
        return self.exec_command(cmd.getvalue())

    def __str__(self):
        return "generating headers from all sif files."
    
#    def display(self):
#        master = self.generator.bld.producer
#        return "[%d/%d] Generating headers from all sif files\n" % (master.processed-1,master.total)
    
# Have all 'cxx' targets have 'include' in their include paths.
@TaskGen.feature('cxx')
@TaskGen.after('process_source')
def scrape_task_generator(self):
    if ( not 'variant_obj' in self.bld.__dict__ ):
        return
    compiled_tasks = self.compiled_tasks
    all_sif_files = []
    all_o_files = []
    intrinsics_o = None
    for task in self.compiled_tasks:
        if ( task.__class__.__name__ == 'cxx' ):
            for node in task.inputs:
                sif_node = node.change_ext('.sif')
                self.create_task('scrape_with_preproc_scan',node,[sif_node])
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
#    self.bld.install_files('${PREFIX}/Contents/Resources/lib/%s'%variant.intrinsics_bitcode_name(),intrinsics_bitcode_node)
#    self.bld.install_files('${PREFIX}/Contents/Resources/lib/%s'%variant.cxx_all_bitcode_name(),cxx_all_bitcode_node)
    self.bld.install_files('${PREFIX}/Contents/Resources/lib/',intrinsics_bitcode_node)
    self.bld.install_files('${PREFIX}/Contents/Resources/lib/',cxx_all_bitcode_node)


def init(ctx):
    from waflib.Build import BuildContext, CleanContext, InstallContext, UninstallContext
    for gc in GCS:
        for debug_char in DEBUG_CHARS:
            for y in (BuildContext, CleanContext, InstallContext, UninstallContext):
                name = y.__name__.replace('Context','').lower()
                for s in STAGE_CHARS:
                    class tmp(y):
                        if (debug_char==None):
                            variant = gc
                        else:
                            variant = gc+'_'+debug_char
                        cmd = name + '_' + s + variant
                        stage = s
            class tmp(BuildContext):
                if (debug_char==None):
                    variant = gc
                else:
                    variant = gc+'_'+debug_char
                cmd = 'rebuild_c'+variant
                stage = 'rebuild'
            class tmp(BuildContext):
                if (debug_char==None):
                    variant = gc
                else:
                    variant = gc+'_'+debug_char
                cmd = 'dangerzone_c'+variant
                stage = 'dangerzone'

#def buildall(ctx):
#    import waflib.Options
#    for s in STAGE_CHARS:
#        for gc in GCS:
#            for debug_char in DEBUG_CHARS:
#                var = 'build_'+s+x+'_'+debug_char
#                waflib.Options.commands.insert(0, var)

