#-*- mode: python; coding: utf-8-unix -*-
#
# USAGE:
#   ./waf distclean configure
#   ./waf [action]_[stage-char][gc-name][_d for debug build]
#   [action] is one of ( build | clean | install )
#   [stage-char] is one of ( i | a | b | c )
#   [gc-name] is one of ( boehm | mps )  --- mps needs special support
#   [_d]  Sets up debug build
#
#  examples:
#    ./waf build_cboehm      # build cboehm
#    ./waf install_cboehm    # build and install cboehm
#    ./waf build_aboehm      # useful for debugging build system - build only aclasp
#    ./waf build_iboehm      # useful to build C++ without rebuilding CL code -
#                            # If done carefully this can be used to quickly test C++ code
#
#   ./waf --jobs 2 install_cboehm  # will build and install cclasp
#   to run with low priority, you can prefix with:
#     nice -n 19 ionice --class 3 ./waf --jobs 2 ...
#
#
#   ./waf build_fboehm # will build most of clasp,
#                        except the most memory hungry linking tasks at the end
#
# NOTE: please observe the following best practices:
#
# - do *not* use waf's ant_glob (you can shoot yourself in the feet
#          with it, leading to tasks getting redone unnecessarily)
# - in emacs, you may want to: (add-to-list 'auto-mode-alist '("wscript\\'" . python-mode))
# - waf constructs have strange names; it's better not to assume that you know what something is or does based solely on its name.
#   e.g. node.change_ext() returns a new node instance... you've been warned!

import os, sys, logging
import time, datetime

try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

from waflib import Utils, Logs, Task, TaskGen
import waflib.Options
from waflib.Tools import c_preproc
# clang_compilation_database is needed by the static analyzer
from waflib.extras import clang_compilation_database
from waflib.Tools.compiler_cxx import cxx_compiler
from waflib.Tools.compiler_c import c_compiler
from waflib.Errors import ConfigurationError

sys.path.append('tools-for-build/')
sys.dont_write_bytecode = True   # avoid littering the dirs with .pyc files

from build_file_lists import *
from wscript_utils import *

# Let's not depend on the locale setting of the host, set it explicitly.
os.environ['LC_ALL'] = os.environ['LANG'] = "C"

# This function enables extra command line options for ./waf --help
def options(ctx):
    ctx.recurse('extensions')
    ctx.load('compiler_cxx')
    ctx.load('compiler_c')
    ctx.add_option('-d', '--debug', default = False, action = 'store_true', dest = 'DEBUG_WHILE_BUILDING',
                   help = 'Enable debugging during the build itself.')
    ctx.add_option('--commands', '--print-commands', '--dump-commands', action = 'store_true', dest = 'PRINT_EXTERNAL_COMMANDS',
                   help = 'Print the copy-paste ready command line of the external programs that the build process spawns.')
    ctx.add_option('--noscrape', '--no-scrape', default = True, action = 'store_false', dest = 'RUN_THE_SCRAPER',
                   help = 'Skip the running of the scraper.')
    ctx.add_option('--load-cclasp', action = 'store_true', dest = 'LOAD_CCLASP',
                   help = '? Probably some debugging helper to start a REPL with cclasp loaded...')
    ctx.add_option('--enable-mpi', action = 'store_true', dest = 'enable_mpi',
                   help = 'Build OpenMPI version of iclasp and cclasp')
    ctx.add_option('--mpi-path', action = 'store', dest = 'mpi_path',
                   help = 'Build OpenMPI version of iclasp and cclasp, provide the path to mpicc and mpic++')
    ctx.add_option('--enable-mpi', action = 'store_true', dest = 'enable_mpi',
                   help = 'Build OpenMPI version of iclasp and cclasp')

#
# Global variables for the build
#
top = '.'
out = 'build'
APP_NAME = 'clasp'
CLANG_VERSION = 6
CLANG_SPECIFIC_VERSION = "6.0.1"

STAGE_CHARS = [ 'r', 'i', 'a', 'b', 'f', 'c', 'd' ]
# Full LTO  -flto
# thin LTO  -flto=thin
LTO_OPTION = "-flto=thin"
GCS_NAMES = [ 'boehm',
              'mpsprep',
              'mps' ]

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
            'boost_date_time',
            'boost_program_options',
            'boost_system',
            'boost_iostreams']

VALID_OPTIONS = [
    # point to the llvm-config executable - this tells the build system which clang to use
    # Default on macOS = /usr/local/opt/llvm@%s/bin/llvm-config'%CLANG_VERSION   - brew installed
    # Default on linux = it searches your path
    "LLVM_CONFIG_BINARY",
    # To link to the debug versions of the LLVM libraries, set a path here to the llvm-config binary of the LLVM debug build
    
    "LLVM_CONFIG_BINARY_FOR_LIBS",
    # point to where you want to install clasp - this has to be defined before ./waf configure
    "PREFIX",
    # Path to sbcl
    "SBCL",
    # What do you want to call clasp?
    "CLASP",
    # Build clasp in parallel
    # default = True
    "USE_PARALLEL_BUILD",
    # build with fork redirecting output: default = True
    "USE_BUILD_FORK_REDIRECT_OUTPUT",
    # Use lld only on Linux when CLASP_BUILD_MODE is "bitcode" - it's faster than ld
    # default = True
    "USE_LLD",
    # Add additional includes 
    "INCLUDES",
    # Add additional link flags
    "LINKFLAGS",
    # Define how clasp is built - can be one of "bitcode" or "object".
    # If "bitcode" then C++ and CL compiles to bitcode and thinLTO is used to link everything.
    #   this gives the fastest product but linking takes a long time.
    # If "object" then C++ and CL produce object files and regular linking is used.
    #   This is probably not as fast as bitcode (maybe a few percent slower) but it links fast.
    #   This is good for development.
    # Default = "object"
    "CLASP_BUILD_MODE",
    # Set the version name of clasp - this is used when building the docker image to give a predictable
    # version name.  Usually the version is calculated from the git hash
    "CLASP_VERSION",
    # Set if on macOS libffi is required.   On macOS we can build with brew installed llvm
    # but brew installed llvm has libffi as a dependency and doesn't report that when you invoke llvm-config --libs!!!
    # This is a bug in llvm-config and has to be fixed upstream.
    # Default = True
    "REQUIRE_LIBFFI",
    # If waf doesn't recognize the OS then use this option (darwin|linux|freebsd)
    "DEST_OS",
    # Turn on debug options
    "DEBUG_OPTIONS",
    # Turn on address sanitizer
    "ADDRESS_SANITIZER",
    # Link libraries statically vs dynamically
    "LINK_STATIC"
]

DEBUG_OPTIONS = [
    "DEBUG_DTREE_INTERPRETER", # Generate dtree interpreter log
    "DEBUG_DTRACE_LOCK_PROBE", # Add a Dtrace probe for mutex lock acquisition
    "DEBUG_STACKMAPS", # print messages about stackmap registration
    "DEBUG_ASSERT", # Turn on DEBUG_ASSERT
    "DEBUG_ASSERT_TYPE_CAST", # Turn on type checking when passing arguments
    "SOURCE_DEBUG", # Allow LOG messages to print - works with CLASP_DEBUG environment variable
    "DEBUG_JIT_LOG_SYMBOLS", # Generate a log of JITted symbols in /tmp/clasp-symbols-<pid>
    "DEBUG_GUARD", # Add guards around allocated objects
    "DEBUG_GUARD_VALIDATE", # add simple checks of guards (fast)
    "DEBUG_GUARD_EXHAUSTIVE_VALIDATE", #add exhaustive, slow, checks of guards
    "DEBUG_TRACE_INTERPRETED_CLOSURES", # Count how many interpreted closures are evaluated
    "DEBUG_ENVIRONMENTS",
    "DEBUG_RELEASE",   # Turn off optimization for a few C++ functions; undef this to optimize everything
    "DEBUG_CACHE",      # Debug the dispatch caches - see cache.cc
    "DEBUG_BITUNIT_CONTAINER",  # prints debug info for bitunit containers
    "DEBUG_LEXICAL_DEPTH", # Generate tests for lexical closure depths
    "DEBUG_FLOW_TRACKER",  # record small backtraces to track flow
    "DEBUG_DYNAMIC_BINDING_STACK",  # dynamic variable binding debugging
    "DEBUG_VALUES",   # turn on printing (values x y z) values when core:*debug-values* is not nil
    "DEBUG_IHS",
    "DEBUG_STACKMAPS", # turn on logging for stackmap code
    "DEBUG_TRACK_UNWINDS",  # Count cc_unwind calls and report in TIME
    "DEBUG_NO_UNWIND",   # debug intrinsics that say they don't unwind but actually do
    "DEBUG_STARTUP",
    ##  Generate per-thread logs in /tmp/dispatch-history/**  of the slow path of fastgf
    "DEBUG_REHASH_COUNT",   # Keep track of the number of times each hash table has been rehashed
    "DEBUG_MONITOR",   # generate logging messages to a file in /tmp for non-hot code
    "DEBUG_MONITOR_SUPPORT",   # Must be enabled with other options - do this automatically?
    "DEBUG_MEMORY_PROFILE",  # Profile memory allocations total size and counter
    "DEBUG_BCLASP_LISP",  # Generate debugging frames for all bclasp code - like declaim
    "DEBUG_CCLASP_LISP",  # Generate debugging frames for all cclasp code - like declaim
    "DEBUG_COUNT_ALLOCATIONS", # count per-thread allocations of instances of classes
    "DEBUG_COMPILER", # Turn on compiler debugging
    "DEBUG_LONG_CALL_HISTORY",   # The GF call histories used to blow up - this triggers an error if they get too long
    "DEBUG_BOUNDS_ASSERT",  # check bounds 
    "DEBUG_GFDISPATCH",  # debug call history manipulation
    "DEBUG_FASTGF",   # generate slow gf dispatch logging and write out dispatch functions to /tmp/dispatch-history-**
    "DEBUG_SLOT_ACCESSORS", # GF accessors have extra debugging added to them
    "DEBUG_THREADS",
    "DEBUG_ENSURE_VALID_OBJECT",  #Defines ENSURE_VALID_OBJECT(x)->x macro - sprinkle these around to run checks on objects
    "DEBUG_QUICK_VALIDATE",    # quick/cheap validate if on and comprehensive validate if not
    "DEBUG_MPS_SIZE",   # check that the size of the MPS object will be calculated properly by obj_skip
    "DEBUG_MPS_UNDERSCANNING",   # Very expensive - does a mps_arena_collect/mps_arena_release for each allocation
    "DEBUG_DONT_OPTIMIZE_BCLASP",  # Optimize bclasp by editing llvm-ir
    "DEBUG_RECURSIVE_ALLOCATIONS",
    "DEBUG_LLVM_OPTIMIZATION_LEVEL_0",
    "DEBUG_SLOW",    # Code runs slower due to checks - undefine to remove checks
    "USE_HUMAN_READABLE_BITCODE",
    "CST", # build the CST version
    "CONFIG_VAR_COOL" # mps setting
]

def build_extension(bld):
    log.pprint('BLUE', "build_extension()")
    bld.recurse("extensions")

def grovel(bld):
    bld.recurse("extensions")

def fetch_git_revision(path, url, revision = "", label = "master"):
    log.info("Git repository %s  url: %s\n     revision: %s  label: %s\n" % (path, url, revision, label))
    ret = os.system("./tools-for-build/fetch-git-revision.sh '%s' '%s' '%s' '%s'" % (path, url, revision, label))
    if ( ret != 0 ):
        raise Exception("Failed to fetch git url %s" % url)

def add_cando_extension_dev(cfg):
    log.pprint('BLUE', 'add_cando_extension_dev')
    fetch_git_revision("extensions/cando",
                       "https://github.com/drmeister/cando.git",
                       label="dev")

def add_cando_extension_testing(cfg):
    log.pprint('BLUE', 'add_cando_extension_testing')
    fetch_git_revision("extensions/cando",
                       "https://github.com/drmeister/cando.git",
                       label="testing")

def add_cando_extension_preview(cfg):
    log.pprint('BLUE', 'add_cando_extension_preview')
    fetch_git_revision("extensions/cando",
                       "https://github.com/drmeister/cando.git",
                       label="preview")

def add_cando_extension_master(cfg):
    log.pprint('BLUE', 'add_cando_extension_master')
    fetch_git_revision("extensions/cando",
                       "https://github.com/drmeister/cando.git",
                       label="master")

def add_cando(cfg):
    add_cando_extension_master(cfg)

    
def update_dependencies(cfg):
    # Specifying only label = "some-tag" will check out that tag into a "detached head", but
    # specifying both label = "master" and revision = "some-tag" will stay on master and reset to that revision.
    log.pprint('BLUE', 'update_dependencies()')
#    fetch_git_revision("src/lisp/kernel/contrib/sicl",
#                       "https://github.com/robert-strandh/SICL.git",
#                       "master")
    fetch_git_revision("src/lisp/kernel/contrib/sicl",
                       "https://github.com/Bike/SICL.git",
                       "8b6c8bc6c1b31b5e0a9fc0554e6a6dc24eccb24f")
    fetch_git_revision("src/lisp/kernel/contrib/Concrete-Syntax-Tree",
                       "https://github.com/robert-strandh/Concrete-Syntax-Tree.git",
                       "8d8c5abf8f1690cb2b765241d81c2eb86d60d77e")
    fetch_git_revision("src/lisp/kernel/contrib/closer-mop",
                       "https://github.com/pcostanza/closer-mop.git",
                       "d4d1c7aa6aba9b4ac8b7bb78ff4902a52126633f")
    fetch_git_revision("src/lisp/kernel/contrib/Acclimation",
                       "https://github.com/robert-strandh/Acclimation.git",
                       "dd15c86b0866fc5d8b474be0da15c58a3c04c45c")
    fetch_git_revision("src/lisp/kernel/contrib/Eclector",
                       "https://github.com/robert-strandh/Eclector.git",
                       "66cf5e2370eef4be659212269272a5e79a82fa1c")
#                      "7b63e7bbe6c60d3ad3413a231835be6f5824240a") works with AST clasp
    fetch_git_revision("src/lisp/kernel/contrib/alexandria",
                       "https://github.com/clasp-developers/alexandria.git",
                       "e5c54bc30b0887c237bde2827036d17315f88737")
    fetch_git_revision("src/mps",
                       "https://github.com/Ravenbrook/mps.git",
                       #DLM says this will be faster.
#                       label = "master", revision = "b1cc9aa5f87f2619ff675c8756e83211865419de")
                       # Very recent branch - may have problems
#                       label = "master", revision = "b5be454728c2ac58b9cb2383360ed0366a7e4115")
                       #First branch that supported fork
#                       label = "master", revision = "46e0a8d77ac470282de7300f5eaf471ca2fbee05")
                       # David set up this branch/2018-08-18/exp-strategy-2 for clasp
                       "b8a05a3846430bc36c8200f24d248c8293801503")
    fetch_git_revision("src/lisp/modules/asdf",
                       "https://gitlab.common-lisp.net/asdf/asdf.git",
                       label = "master", revision = "3.3.1.2")
    os.system("(cd src/lisp/modules/asdf; ${MAKE-make} --quiet)")

# run this from a completely cold system with:
# ./waf distclean configure
# ./waf build_impsprep
# ./waf analyze_clasp
# This is the static analyzer - formerly called 'redeye'
def analyze_clasp(cfg):
    run_program_echo("build/boehm/cclasp-boehm",
                     "--feature", "ignore-extensions",
                     "--load",    "sys:modules;clasp-analyzer;run-serial-analyzer.lisp",
                     "--eval",    "(core:quit)")
    print("\n\n\n----------------- proceeding with static analysis --------------------")

def stage_value(ctx,s):
    if ( s == 'r' ):
        sval = -1
    elif ( s == 'i' ):
        sval = 0
    elif ( s == 'a' ):
        sval = 1
    elif ( s == 'b' ):
        sval = 2
    elif ( s == 'c' ):
        sval = 4
    elif ( s == 'd' ):
        sval = 5
    elif ( s == 'f' ):
        sval = 3
    elif ( s == 'rebuild' ):
        sval = 0
    elif ( s == 'dangerzone' ):
        sval = 0
    else:
        ctx.fatal("Illegal stage: %s" % s)
    return sval

# Called for each variant, at the end of the configure phase
def configure_common(cfg,variant):
#    include_path = "%s/%s/%s/src/include/clasp/main/" % (cfg.path.abspath(),out,variant.variant_dir()) #__class__.__name__)
#    cfg.env.append_value("CXXFLAGS", ['-I%s' % include_path])
#    cfg.env.append_value("CFLAGS", ['-I%s' % include_path])
    # These will end up in build/config.h
    cfg.define("EXECUTABLE_NAME",variant.executable_name())
    if (cfg.env.PREFIX):
        pass
    else:
        cfg.env.PREFIX = "/opt/clasp"
    cfg.define("PREFIX",cfg.env.PREFIX)
    assert os.path.isdir(cfg.env.LLVM_BIN_DIR)
    log.info("cfg.env.PREFIX is %s" % cfg.env.PREFIX)
    cfg.define("CLASP_CLANG_PATH", os.path.join(cfg.env.LLVM_BIN_DIR, "clang"))
    cfg.define("APP_NAME",APP_NAME)
    cfg.define("BITCODE_NAME",variant.bitcode_name())
    cfg.define("VARIANT_NAME",variant.variant_name())
    cfg.define("BUILD_STLIB", libraries_as_link_flags_as_string(cfg.env.STLIB_ST,cfg.env.STLIB))
    cfg.define("BUILD_LIB", libraries_as_link_flags_as_string(cfg.env.LIB_ST,cfg.env.LIB))
    log.debug("cfg.env.LINKFLAGS = %s", cfg.env.LINKFLAGS)
    log.debug("cfg.env.LDFLAGS = %s", cfg.env.LDFLAGS)
    cfg.define("BUILD_LINKFLAGS", ' '.join(cfg.env.LINKFLAGS) + ' ' + ' '.join(cfg.env.LDFLAGS))

def setup_clang_compiler(cfg,variant):
    cfg.env.COMPILER_CC="clang"
    cfg.env.COMPILER_CXX="clang++"
    cfg.env.CC = cfg.find_program(cfg.env.COMPILER_CC,quiet=True)
    cfg.env.CXX = cfg.find_program(cfg.env.COMPILER_CXX,quiet=True)
    cfg.env.LINK_CC = cfg.env.CC
    cfg.env.LINK_CXX = cfg.env.CXX

def setup_mpi_compiler(cfg,variant):
    cfg.env.COMPILER_CC="mpicc"
    cfg.env.COMPILER_CXX="mpic++"
    cfg.env.CC = cfg.find_program(cfg.env.COMPILER_CC,quiet=True)
    cfg.env.CXX = cfg.find_program(cfg.env.COMPILER_CXX,quiet=True)
    cfg.env.LINK_CC = cfg.env.CC
    cfg.env.LINK_CXX = cfg.env.CXX

class variant(object):
    build_with_debug_info = False

    def debug_extension(self):
        return "-d" if self.build_with_debug_info else ""
    def debug_dir_extension(self):
        return "_d" if self.build_with_debug_info else ""
    def mpi_extension(self):
        return "-mpi" if self.enable_mpi else ""
    def mpi_dir_extension(self):
        return "_mpi" if self.enable_mpi else ""
    def executable_name(self,stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        if (not (use_stage>='a' and use_stage <= 'z')):
            raise Exception("Bad stage: %s"% use_stage)
        return '%s%s-%s%s%s' % (use_stage,APP_NAME,self.gc_name,self.mpi_extension(),self.debug_extension())
    def fasl_name(self,build,stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        if (not (use_stage>='a' and use_stage <= 'z')):
            raise Exception("Bad stage: %s"% use_stage)
        if (build.env.CLASP_BUILD_MODE == 'fasl'):
            return build.path.find_or_declare('fasl/%s%s-%s%s%s-image.lfasl' % (use_stage,APP_NAME,self.gc_name,self.mpi_extension(),self.debug_extension()))
        else:
            return build.path.find_or_declare('fasl/%s%s-%s%s%s-image.fasl' % (use_stage,APP_NAME,self.gc_name,self.mpi_extension(),self.debug_extension()))
    def fasl_dir(self, stage=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        return 'fasl/%s%s-%s%s-bitcode' % (use_stage,APP_NAME,self.gc_name,self.mpi_extension())

    def common_lisp_output_name_list(self,build,input_files,stage=None,variant=None):
        if ( stage == None ):
            use_stage = self.stage_char
        else:
            use_stage = stage
        if (not (use_stage>='a' and use_stage <= 'z')):
            raise Exception("Bad stage: %s"% use_stage)
        name = 'fasl/%s%s-%s-common-lisp' % (use_stage,APP_NAME,self.gc_name)
        if (build.env.CLASP_BUILD_MODE == 'fasl'):
            return [build.path.find_or_declare(name+".lfasl")]
        elif (build.env.CLASP_BUILD_MODE == "object"):
            nodes = waf_nodes_for_object_files(build,input_files,self.fasl_dir(stage=use_stage))
            return nodes
        else:
            if (build.use_human_readable_bitcode):
                return [build.path.find_or_declare(name+".ll")]
            else:
                return [build.path.find_or_declare(name+".bc")]
    def variant_dir(self):
        return "%s%s%s"%(self.gc_name,self.mpi_dir_extension(),self.debug_dir_extension())
           
    def variant_name(self):
        if (self.enable_mpi):
            mpi_part = "-mpi"
        else:
            mpi_part = ""
        return "%s%s" % (self.gc_name,mpi_part)
    def bitcode_name(self):
        return "%s%s"%(self.variant_name(),self.debug_extension())
    def cxx_all_bitcode_name(self):
        return 'fasl/%s-all-cxx.a' % self.bitcode_name()
    def inline_bitcode_archive_name(self,name):
        return 'fasl/%s-%s-cxx.a' % (self.bitcode_name(),name)
    def inline_bitcode_name(self,name):
        return 'fasl/%s-%s-cxx.bc' % (self.bitcode_name(),name)
    def configure_for_release(self,cfg):
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3', '-g', '-fPIC' ])
        cfg.env.append_value('CFLAGS', [ '-O3', '-g', '-fPIC' ])
        cfg.define("ALWAYS_INLINE_MPS_ALLOCATIONS",1)
        if (os.getenv("CLASP_RELEASE_CXXFLAGS") != None):
            cfg.env.append_value('CXXFLAGS', os.getenv("CLASP_RELEASE_CXXFLAGS").split() )
        if (os.getenv("CLASP_RELEASE_LINKFLAGS") != None):
            cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_RELEASE_LINKFLAGS").split())
    def configure_for_debug(self,cfg):
        cfg.define("_DEBUG_BUILD",1)
        cfg.define("DEBUG_ASSERT",1)
        cfg.define("DEBUG_BOUNDS_ASSERT",1)
#        cfg.define("DEBUG_ASSERT_TYPE_CAST",1)  # checks runtime type casts
        cfg.define("CONFIG_VAR_COOL",1)
#        cfg.env.append_value('CXXFLAGS', [ '-O0', '-g' ])
        cfg.env.append_value('CXXFLAGS', [ '-O0', '-g' ])
        log.debug("cfg.env.LTO_FLAG = %s", cfg.env.LTO_FLAG)
        if (cfg.env.LTO_FLAG):
            cfg.env.append_value('LDFLAGS', [ '-Wl','-mllvm', '-O0', '-g' ])
        cfg.env.append_value('CFLAGS', [ '-O0', '-g', '-fPIC' ])
        if (os.getenv("CLASP_DEBUG_CXXFLAGS") != None):
            cfg.env.append_value('CXXFLAGS', os.getenv("CLASP_DEBUG_CXXFLAGS").split() )
        if (os.getenv("CLASP_DEBUG_LINKFLAGS") != None):
            cfg.env.append_value('LINKFLAGS', os.getenv("CLASP_DEBUG_LINKFLAGS").split())
    def common_setup(self,cfg):
        if self.build_with_debug_info:
            self.configure_for_debug(cfg)
        else:
            self.configure_for_release(cfg)
        configure_common(cfg, self)
        cfg.write_config_header("%s/config.h"%self.variant_dir(),remove=True)

class boehm_base(variant):
    gc_name = 'boehm'
    enable_mpi = False
    def configure_variant(self,cfg,env_copy):
        cfg.define("USE_BOEHM",1)
        setup_clang_compiler(cfg,self)
        if (cfg.env['DEST_OS'] == DARWIN_OS ):
            log.debug("boehm_base cfg.env.LTO_FLAG = %s", cfg.env.LTO_FLAG)
            if (cfg.env.LTO_FLAG):
                cfg.env.append_value('LDFLAGS', '-Wl,-object_path_lto,%s_lib.lto.o' % self.executable_name())
        log.info("Setting up boehm library cfg.env.STLIB_BOEHM = %s ", cfg.env.STLIB_BOEHM)
        log.info("Setting up boehm library cfg.env.LIB_BOEHM = %s", cfg.env.LIB_BOEHM)
        if (cfg.env.LIB_BOEHM == [] ):
            cfg.env.append_value('STLIB',cfg.env.STLIB_BOEHM)
        else:
            cfg.env.append_value('LIB',cfg.env.LIB_BOEHM)
        self.common_setup(cfg)

class boehm(boehm_base):
    def configure_variant(self,cfg,env_copy):
        cfg.setenv(self.variant_dir(), env=env_copy.derive())
        super(boehm,self).configure_variant(cfg,env_copy)

class boehm_d(boehm_base):
    build_with_debug_info = True

    def configure_variant(self,cfg,env_copy):
        cfg.setenv("boehm_d", env=env_copy.derive())
        super(boehm_d,self).configure_variant(cfg,env_copy)

class mps_base(variant):
    enable_mpi = False
    def configure_variant(self,cfg,env_copy):
        cfg.define("USE_MPS",1)
        setup_clang_compiler(cfg,self)
        if (cfg.env['DEST_OS'] == DARWIN_OS ):
            if (cfg.env.LTO_FLAG):
                cfg.env.append_value('LDFLAGS', '-Wl,-object_path_lto,%s_lib.lto.o' % self.executable_name())
        self.common_setup(cfg)

class mpsprep(mps_base):
    gc_name = 'mpsprep'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mpsprep", env=env_copy.derive())
        cfg.define("RUNNING_GC_BUILDER",1)
        super(mpsprep,self).configure_variant(cfg,env_copy)

class mpsprep_d(mps_base):
    gc_name = 'mpsprep'
    build_with_debug_info = True

    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mpsprep_d", env=env_copy.derive())
        cfg.define("RUNNING_GC_BUILDER",1)
        super(mpsprep_d,self).configure_variant(cfg,env_copy)

class mps(mps_base):
    gc_name = 'mps'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mps", env=env_copy.derive())
        super(mps,self).configure_variant(cfg,env_copy)

class mps_d(mps_base):
    gc_name = 'mps'
    build_with_debug_info = True

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

###### MPI versions
class boehm_mpi_base(variant):
    gc_name = 'boehm'
    enable_mpi = True
    def configure_variant(self,cfg,env_copy):
        cfg.define("USE_BOEHM",1)
        cfg.define("USE_MPI",1)
        setup_mpi_compiler(cfg,self)
        if (cfg.env['DEST_OS'] == DARWIN_OS ):
            log.debug("boehm_mpi_base cfg.env.LTO_FLAG = %s", cfg.env.LTO_FLAG)
            if (cfg.env.LTO_FLAG):
                cfg.env.append_value('LDFLAGS', '-Wl,-object_path_lto,%s_lib.lto.o' % self.executable_name())
        log.info("Setting up boehm library cfg.env.STLIB_BOEHM = %s ", cfg.env.STLIB_BOEHM)
        log.info("Setting up boehm library cfg.env.LIB_BOEHM = %s", cfg.env.LIB_BOEHM)
        if (cfg.env.LIB_BOEHM == [] ):
            cfg.env.append_value('STLIB',cfg.env.STLIB_BOEHM)
        else:
            cfg.env.append_value('LIB',cfg.env.LIB_BOEHM)
        self.common_setup(cfg)

class boehm_mpi(boehm_mpi_base):
    def configure_variant(self,cfg,env_copy):
        cfg.setenv(self.variant_dir(), env=env_copy.derive())
        super(boehm_mpi,self).configure_variant(cfg,env_copy)

class boehm_mpi_d(boehm_mpi_base):
    build_with_debug_info = True

    def configure_variant(self,cfg,env_copy):
        cfg.setenv("boehm_mpi_d", env=env_copy.derive())
        super(boehm_mpi_d,self).configure_variant(cfg,env_copy)

class mps_mpi_base(variant):
    enable_mpi = True
    def configure_variant(self,cfg,env_copy):
        cfg.define("USE_MPS",1)
        cfg.define("USE_MPI",1)
        setup_mpi_compiler(cfg,self)
        if (cfg.env['DEST_OS'] == DARWIN_OS ):
            if (cfg.env.LTO_FLAG):
                cfg.env.append_value('LDFLAGS', '-Wl,-object_path_lto,%s_lib.lto.o' % self.executable_name())
        self.common_setup(cfg)

class mpsprep_mpi(mps_mpi_base):
    gc_name = 'mpsprep'

    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mpsprep_mpi", env=env_copy.derive())
        cfg.define("RUNNING_GC_BUILDER",1)
        super(mpsprep_mpi,self).configure_variant(cfg,env_copy)

class mpsprep_mpi_d(mps_mpi_base):
    gc_name = 'mpsprep'
    build_with_debug_info = True

    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mpsprep_mpi_d", env=env_copy.derive())
        cfg.define("RUNNING_GC_BUILDER",1)
        super(mpsprep_mpi_d,self).configure_variant(cfg,env_copy)

class mps_mpi(mps_mpi_base):
    gc_name = 'mps'
    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mps_mpi", env=env_copy.derive())
        super(mps_mpi,self).configure_variant(cfg,env_copy)

class mps_mpi_d(mps_mpi_base):
    gc_name = 'mps'
    build_with_debug_info = True

    def configure_variant(self,cfg,env_copy):
        cfg.setenv("mps_mpi_d", env=env_copy.derive())
        super(mps_mpi_d,self).configure_variant(cfg,env_copy)

class iboehm_mpi(boehm_mpi):
    stage_char = 'i'
class aboehm_mpi(boehm_mpi):
    stage_char = 'a'
class bboehm_mpi(boehm_mpi):
    stage_char = 'b'
class cboehm_mpi(boehm_mpi):
    stage_char = 'c'

class iboehm_mpi_d(boehm_mpi_d):
    stage_char = 'i'
class aboehm_mpi_d(boehm_mpi_d):
    stage_char = 'a'
class bboehm_mpi_d(boehm_mpi_d):
    stage_char = 'b'
class cboehm_mpi_d(boehm_mpi_d):
    stage_char = 'c'

class imps_mpi(mps_mpi):
    stage_char = 'i'
class amps_mpi(mps_mpi):
    stage_char = 'a'
class bmps_mpi(mps_mpi):
    stage_char = 'b'
class cmps_mpi(mps_mpi):
    stage_char = 'c'

class imps_mpi_d(mps_mpi_d):
    stage_char = 'i'
class amps_mpi_d(mps_mpi_d):
    stage_char = 'a'
class bmps_mpi_d(mps_mpi_d):
    stage_char = 'b'
class cmps_mpi_d(mps_mpi_d):
    stage_char = 'c'

class impsprep_mpi(mpsprep_mpi):
    stage_char = 'i'
class ampsprep_mpi(mpsprep_mpi):
    stage_char = 'a'
class bmpsprep_mpi(mpsprep_mpi):
    stage_char = 'b'
class cmpsprep_mpi(mpsprep_mpi):
    stage_char = 'c'

class impsprep_mpi_d(mpsprep_mpi_d):
    stage_char = 'i'
class ampsprep_mpi_d(mpsprep_mpi_d):
    stage_char = 'a'
class bmpsprep_mpi_d(mpsprep_mpi_d):
    stage_char = 'b'
class cmpsprep_mpi_d(mpsprep_mpi_d):
    stage_char = 'c'




    
def configure(cfg):
    def update_exe_search_path(cfg):
        log.info("DEST_OS = %s" % cfg.env['DEST_OS'])
        llvm_config_binary = cfg.env.LLVM_CONFIG_BINARY
        if (len(llvm_config_binary) == 0):
            if (cfg.env['DEST_OS'] == DARWIN_OS ):
                llvm_config_binary = '/usr/local/Cellar/llvm/%s/bin/llvm-config'%CLANG_SPECIFIC_VERSION
                log.info("On darwin looking for %s" % llvm_config_binary)
            else:
                try:
                    llvm_config_binary = cfg.find_program('llvm-config-%s.0'%CLANG_VERSION)
                except cfg.errors.ConfigurationError:
                    cfg.to_log('llvm-config-%s.0 was not found (ignoring)'%CLANG_VERSION)
                    # Let's fail if no llvm-config binary has been found
                    llvm_config_binary = cfg.find_program('llvm-config')
                llvm_config_binary = llvm_config_binary[0]
                log.info("On %s looking for %s" % (cfg.env['DEST_OS'],llvm_config_binary))
            cfg.env["LLVM_CONFIG_BINARY"] = llvm_config_binary
        log.info("Using llvm-config binary: %s", cfg.env.LLVM_CONFIG_BINARY)
        # Let's prefix the OS's binary search PATH with the configured LLVM bin dir.
        # TODO Maybe it would be better to handle full binary paths explicitly -- if possible at all.
        path = os.getenv("PATH").split(os.pathsep)
        externals_bin_dir = run_llvm_config(cfg, "--bindir")
        path.insert(0, externals_bin_dir)
        cfg.environ["PATH"] = os.pathsep.join(path)
        log.info("PATH has been prefixed with '%s'", externals_bin_dir)
        assert os.path.isdir(externals_bin_dir), "Please provide a valid LLVM_CONFIG_BINARY. See the 'wscript.config.template' file."

    def check_externals_clasp_version(cfg):
        log.debug("check_externals_clasp_version begins, cfg.env.LLVM_CONFIG_BINARY = %s", cfg.env.LLVM_CONFIG_BINARY)
        if (not "externals-clasp" in cfg.env.LLVM_CONFIG_BINARY):
            return
        fin = open(run_llvm_config(cfg, "--prefix") + "/../../makefile", "r")
        externals_clasp_llvm_hash = "0bc70c306ccbf483a029a25a6fd851bc332accff"   # update this when externals-clasp is updated
        correct_version = False
        for x in fin:
            if (externals_clasp_llvm_hash in x):
                correct_version = True
        fin.close()
        if (not correct_version):
            raise Exception("You do not have the correct version of externals-clasp installed - you need the one with the LLVM_COMMIT=%s" % externals_clasp_llvm_hash)

    def load_local_config(cfg):
        local_environment = {}
        if os.path.isfile("./wscript.config"):
            log.debug("local_environment = %s", local_environment)
            exec(open("./wscript.config").read(), globals(), local_environment)
            for key in local_environment.keys():
                if (not key in VALID_OPTIONS):
                    raise Exception("%s is an INVALID wscript.config option - valid options are: %s" % (key, VALID_OPTIONS))
                else:
                    log.info("wscript.config option %s = %s", key, local_environment[key])
                cfg.env.update(local_environment)

    #
    # This is where configure(cfg) starts
    #
    log.pprint("BLUE", "cfg.options.enable_mpi = %s" % cfg.options.enable_mpi)
    log.pprint('BLUE', 'configure()')

    cfg.env["BUILD_ROOT"] = os.path.abspath(top) # KLUDGE there should be a better way than this
    load_local_config(cfg)
    cfg.load("why")
    cfg.check_waf_version(mini = '1.7.5')
    cfg.env["DEST_OS"] = cfg.env["DEST_OS"] or Utils.unversioned_sys_platform()
    update_exe_search_path(cfg)
    run_llvm_config(cfg, "--version") # make sure we fail early
    check_externals_clasp_version(cfg)

    if (cfg.env.LLVM_CONFIG_BINARY_FOR_LIBS):
        log.warn("Using a separate llvm-config binary for linking with the LLVM libs: %s", cfg.env.LLVM_CONFIG_BINARY_FOR_LIBS)
    else:
        log.debug("Using the same llvm-config binary for linking with the LLVM libs: %s", cfg.env.LLVM_CONFIG_BINARY)
        cfg.env["LLVM_CONFIG_BINARY_FOR_LIBS"] = cfg.env.LLVM_CONFIG_BINARY

    if (cfg.env.LLVM5_ORC_NOTIFIER_PATCH):
        cfg.define("LLVM5_ORC_NOTIFIER_PATCH",1)
    if (cfg.env.USE_PARALLEL_BUILD == False):
        pass
    else:
        cfg.env.USE_PARALLEL_BUILD = True
        cfg.define("USE_PARALLEL_BUILD",1)
    if (cfg.env.USE_BUILD_FORK_REDIRECT_OUTPUT == False):
        pass
    else:
        cfg.env.USE_BUILD_FORK_REDIRECT_OUTPUT = True
        cfg.define("USE_BUILD_FORK_REDIRECT_OUTPUT",1)
    cfg.env["LLVM_BIN_DIR"] = run_llvm_config(cfg, "--bindir")
    cfg.env["LLVM_AR_BINARY"] = "%s/llvm-ar" % cfg.env.LLVM_BIN_DIR
#    cfg.env["LLVM_AR_BINARY"] = cfg.find_program("llvm-ar", var = "LLVM_AR")[0]
    cfg.env["GIT_BINARY"] = cfg.find_program("git", var = "GIT")[0]
    log.debug("cfg.env['CLASP_BUILD_MODE'] = %s", cfg.env['CLASP_BUILD_MODE'])
    if ((cfg.env['CLASP_BUILD_MODE'] =='bitcode')):
        cfg.define("CLASP_BUILD_MODE",2) # thin-lto
        cfg.env.CLASP_BUILD_MODE = 'bitcode'
        cfg.env.LTO_FLAG = LTO_OPTION
    elif (cfg.env['CLASP_BUILD_MODE']==[] or cfg.env['CLASP_BUILD_MODE']=='object'):
        cfg.define("CLASP_BUILD_MODE",1) # object files
        cfg.env.CLASP_BUILD_MODE = 'object'
        cfg.env.LTO_FLAG = []
    elif (cfg.env['CLASP_BUILD_MODE']=='fasl'):
        cfg.define("CLASP_BUILD_MODE",0) # object files
        cfg.env.CLASP_BUILD_MODE = 'fasl'
        cfg.env.LTO_FLAG = []
    else:
        raise Exception("CLASP_BUILD_MODE can only be 'thinlto'(default), 'lto', or 'obj' - you provided %s" % cfg.env['CLASP_BUILD_MODE'])
    log.info("default cfg.env.CLASP_BUILD_MODE = %s, final cfg.env.LTO_FLAG = '%s'", cfg.env.CLASP_BUILD_MODE, cfg.env.LTO_FLAG)

    cur_clang_version = run_llvm_config(cfg, "--version")
    log.debug("cur_clang_version = %s", cur_clang_version)
    if (int(cur_clang_version[0]) != CLANG_VERSION):
        raise Exception("You must have clang/llvm version %d installed" % CLANG_VERSION )
    # find a lisp for the scraper
    if not cfg.env.SCRAPER_LISP:
        cfg.env["SBCL"] = cfg.find_program("sbcl", var = "SBCL")[0]
        cfg.env["SCRAPER_LISP"] = [cfg.env.SBCL,
                                   '--noinform', '--dynamic-space-size', '2048', '--lose-on-corruption', '--disable-ldb', '--end-runtime-options',
                                   '--disable-debugger', '--no-userinit', '--no-sysinit']
    global cxx_compiler, c_compiler
    cxx_compiler['linux'] = ["clang++"]
    c_compiler['linux'] = ["clang"]
    cfg.load('compiler_cxx')
    cfg.load('compiler_c')
### Without these checks the following error happens: AttributeError: 'BuildContext' object has no attribute 'variant_obj'
    cfg.check_cxx(lib='gmpxx gmp'.split(), cflags='-Wall', uselib_store='GMP')
    try:
        cfg.check_cxx(stlib='gc', cflags='-Wall', uselib_store='BOEHM')
    except ConfigurationError:
        cfg.check_cxx(lib='gc', cflags='-Wall', uselib_store='BOEHM')
    #libz
    cfg.check_cxx(lib='z', cflags='-Wall', uselib_store='Z')
    if (cfg.env['DEST_OS'] == LINUX_OS or cfg.env['DEST_OS'] == FREEBSD_OS):
        cfg.check_cxx(lib='dl', cflags='-Wall', uselib_store='DL')
        cfg.check_cxx(lib='elf', cflags='-Wall', uselib_store='ELF')
    cfg.check_cxx(lib='ncurses', cflags='-Wall', uselib_store='NCURSES')
#    cfg.check_cxx(stlib='lldb', cflags='-Wall', uselib_store='LLDB')
    cfg.check_cxx(lib='m', cflags='-Wall', uselib_store='M')
    if (cfg.env['REQUIRE_LIBFFI'] == []):
        cfg.env['REQUIRE_LIBFFI'] = True
    if (cfg.env['DEST_OS'] == DARWIN_OS and cfg.env['REQUIRE_LIBFFI'] == True):
        cfg.check_cxx(lib='ffi', cflags='-Wall', uselib_store="FFI")
    elif (cfg.env['DEST_OS'] == LINUX_OS ):
        cfg.check_cxx(lib='bsd', cflags='-Wall', uselib_store='BSD')
#        cfg.check_cxx(lib='gcc_s', cflags='-Wall', uselib_store="GCC_S")
#        cfg.check_cxx(lib='unwind-x86_64', cflags='-Wall', uselib_store='UNWIND_X86_64')
#        cfg.check_cxx(lib='unwind', cflags='-Wall', uselib_store='UNWIND')
#        cfg.check_cxx(lib='lzma', cflags='-Wall', uselib_store='LZMA')
    else:
        pass
    # Check the boost libraries one at a time and then all together to put them in uselib_store
    boost_libs = BOOST_LIBRARIES
    if (cfg.options.enable_mpi):
        boost_libs = boost_libs + [ "boost_mpi" ]
    for onelib in boost_libs:
        if (cfg.env['LINK_STATIC']):
            cfg.check_cxx(stlib=onelib, cflags='-Wall', uselib_store='BOOST-%s'%onelib)
        else:
            cfg.check_cxx(lib=onelib, cflags='-Wall', uselib_store='BOOST-%s'%onelib)
    log.info("Checking for all boost libraries together to put them all in one uselib_store")
    if (cfg.env['LINK_STATIC']):
        cfg.check_cxx(stlib=boost_libs, cflags='-Wall', uselib_store='BOOST')
    else:
        cfg.check_cxx(lib=boost_libs, cflags='-Wall', uselib_store='BOOST')
    cfg.extensions_include_dirs = []
    cfg.extensions_gcinterface_include_files = []
    cfg.extensions_stlib = []
    cfg.extensions_lib = []
    cfg.extensions_names = []
    cfg.recurse('extensions')
    log.debug("cfg.extensions_names before sort = %s", cfg.extensions_names)
    cfg.extensions_names = sorted(cfg.extensions_names)
    log.debug("cfg.extensions_names after sort = %s", cfg.extensions_names)
    clasp_gc_filename = "clasp_gc.cc"
    if (len(cfg.extensions_names) > 0):
        clasp_gc_filename = "clasp_gc_%s.cc" % ("_".join(cfg.extensions_names))
    log.debug("clasp_gc_filename = %s", clasp_gc_filename)
    cfg.define("CLASP_GC_FILENAME",clasp_gc_filename)
    llvm_liblto_dir = run_llvm_config(cfg, "--libdir")
    llvm_lib_dir = run_llvm_config_for_libs(cfg, "--libdir")
    log.debug("llvm_lib_dir = %s", llvm_lib_dir)
    cfg.env.append_value('LINKFLAGS', ["-L%s" % llvm_lib_dir])
    llvm_libraries = [ x[2:] for x in run_llvm_config_for_libs(cfg, "--libs").split()] # drop the '-l' prefixes
#dynamic llvm/clang
    cfg.check_cxx(lib=CLANG_LIBRARIES, cflags='-Wall', uselib_store='CLANG', libpath = llvm_lib_dir )
    cfg.check_cxx(lib=llvm_libraries, cflags = '-Wall', uselib_store = 'LLVM', libpath = llvm_lib_dir )
#static llvm/clang
#    cfg.check_cxx(stlib=llvm_libraries, cflags = '-Wall', uselib_store = 'LLVM', stlibpath = llvm_lib_dir )
#    cfg.check_cxx(stlib=CLANG_LIBRARIES, cflags='-Wall', uselib_store='CLANG', stlibpath = llvm_lib_dir )
    llvm_include_dir = run_llvm_config_for_libs(cfg, "--includedir")
    log.debug("llvm_include_dir = %s", llvm_include_dir)
    cfg.env.append_value('CXXFLAGS', ['-I./', '-I' + llvm_include_dir])
    cfg.env.append_value('CFLAGS', ['-I./'])
    if (cfg.env['DEST_OS'] == LINUX_OS):
        cfg.env.append_value('CXXFLAGS',['-fno-omit-frame-pointer', '-mno-omit-leaf-frame-pointer'])
        cfg.env.append_value('CFLAGS',['-fno-omit-frame-pointer', '-mno-omit-leaf-frame-pointer'])
    cfg.env.append_value('CXXFLAGS',['-fPIC'])
    cfg.env.append_value('CFLAGS',['-fPIC'])
#    if ('program_name' in cfg.__dict__):
#        pass
#    else:
#        cfg.env.append_value('CXXFLAGS', ['-I%s/include/clasp/main/'% cfg.path.abspath() ])
# Check if GC_enumerate_reachable_objects_inner is available
# If so define  BOEHM_GC_ENUMERATE_REACHABLE_OBJECTS_INNER_AVAILABLE
#
    if (cfg.env["BOEHM_GC_ENUMERATE_REACHABLE_OBJECTS_INNER_AVAILABLE"] == True):
        cfg.define("BOEHM_GC_ENUMERATE_REACHABLE_OBJECTS_INNER_AVAILABLE",1)
    cfg.define("USE_CLASP_DYNAMIC_CAST",1)
    cfg.define("BUILDING_CLASP",1)
    log.debug("cfg.env['DEST_OS'] == %s", cfg.env['DEST_OS'])
    if (cfg.env['DEST_OS'] == DARWIN_OS ):
        cfg.define("_TARGET_OS_DARWIN",1)
        cfg.define("USE_LIBUNWIND",1) # use LIBUNWIND
    elif (cfg.env['DEST_OS'] == LINUX_OS ):
        cfg.define("_TARGET_OS_LINUX",1);
    elif (cfg.env['DEST_OS'] == FREEBSD_OS ):
        cfg.define("_TARGET_OS_FREEBSD",1);
    else:
        raise Exception("Unknown OS %s"%cfg.env['DEST_OS'])
    cfg.define("PROGRAM_CLASP",1)
    cfg.define("CLASP_THREADS",1)
    cfg.define("CLASP_GIT_COMMIT",get_git_commit(cfg))
    cfg.define("CLASP_VERSION",get_clasp_version(cfg))
    cfg.define("CLBIND_DYNAMIC_LINK",1)
    cfg.define("DEFINE_CL_SYMBOLS",1)
    cfg.define("USE_SOURCE_DATABASE",1)
    cfg.define("USE_COMPILED_CLOSURE",1)  # disable this in the future and switch to ClosureWithSlots
    cfg.define("CLASP_UNICODE",1)
#    cfg.define("EXPAT",1)
    cfg.define("INCLUDED_FROM_CLASP",1)
    cfg.define("INHERITED_FROM_SRC",1)
    cfg.define("LLVM_VERSION_X100",400)
    cfg.define("LLVM_VERSION","4.0")
    cfg.define("NDEBUG",1)
#    cfg.define("READLINE",1)
#    cfg.define("USE_EXPENSIVE_BACKTRACE",1)
    cfg.define("X86_64",1)
#    cfg.define("DEBUG_FUNCTION_CALL_COUNTER",1)
    cfg.define("_ADDRESS_MODEL_64",1)
    cfg.define("__STDC_CONSTANT_MACROS",1)
    cfg.define("__STDC_FORMAT_MACROS",1)
    cfg.define("__STDC_LIMIT_MACROS",1)
    cfg.env.append_value('LINKFLAGS', ['-v'] )
#    includes = [ 'include/' ]
#    includes = includes + cfg.plugins_include_dirs
#    includes_from_build_dir = []
#    for x in includes:
#        includes_from_build_dir.append("-I%s/%s"%(cfg.path.abspath(),x))
#    cfg.env.append_value('CXXFLAGS', includes_from_build_dir )
#    cfg.env.append_value('CFLAGS', includes_from_build_dir )
#    log.debug("DEBUG includes_from_build_dir = %s", includes_from_build_dir)
    cfg.env.append_value('CXXFLAGS', [ '-std=c++11'])
#    cfg.env.append_value('CXXFLAGS', ["-D_GLIBCXX_USE_CXX11_ABI=1"])
    if (cfg.env.LTO_FLAG):
        cfg.env.append_value('CXXFLAGS', cfg.env.LTO_FLAG )
        cfg.env.append_value('CFLAGS', cfg.env.LTO_FLAG )
        cfg.env.append_value('LINKFLAGS', cfg.env.LTO_FLAG )
    if (cfg.env['DEST_OS'] == LINUX_OS ):
        if (cfg.env['USE_LLD'] and cfg.env.CLASP_BUILD_MODE == 'bitcode'):
            # Only use lld if USE_LLD is set and CLASP_BUILD_MODE is bitcode
            cfg.env.append_value('LINKFLAGS', '-fuse-ld=lld-%d.0' % CLANG_VERSION)
            log.info("Using the lld linker")
        else:
#            cfg.env.append_value('LINKFLAGS', '-fuse-ld=lld-%d.0' % CLANG_VERSION)
            cfg.env.append_value('LINKFLAGS', '-fuse-ld=gold')
            log.info("Using the gold linker")
        cfg.env.append_value('LINKFLAGS', ['-stdlib=libstdc++']) # libstdc++/GCC libc++/clang
        cfg.env.append_value('LINKFLAGS', ['-lstdc++']) # -lstdc++/GCC -lc++/clang
        cfg.env.append_value('LINKFLAGS', '-pthread')
    elif (cfg.env['DEST_OS'] == FREEBSD_OS ):
        #--lto-O0 is not effective for avoiding linker hangs
        # cfg.env.append_value('LINKFLAGS', ['-Wl,-export_dynamic,--lto-O0'])
        if (cfg.env['USE_LLD']):
            cfg.env.append_value('LINKFLAGS', '-fuse-ld=lld-%d.0' % CLANG_VERSION)
            log.info("Using the lld linker")
        else:
            #cfg.env.append_value('LINKFLAGS', '-fuse-ld=/opt/clasp/bin/ld.clasp')
            #log.info("Using linker frontend /opt/clasp/bin/ld.clasp")
            cfg.env.append_value('LINKFLAGS', '-fuse-ld=gold')
        cfg.env.append_value('LINKFLAGS', '-pthread')
        cfg.env.append_value('LINKFLAGS', '-lexecinfo')
    elif (cfg.env['DEST_OS'] == DARWIN_OS ):
        cfg.env.append_value('LINKFLAGS', ['-Wl,-export_dynamic'])
        lto_library_name = cfg.env.cxxshlib_PATTERN % "LTO"  # libLTO.<os-dep-extension>
        lto_library = "%s/%s" % ( llvm_liblto_dir, lto_library_name)
        cfg.env.append_value('LINKFLAGS',["-Wl,-lto_library,%s" % lto_library])
        cfg.env.append_value('LINKFLAGS', ['-lc++'])
        cfg.env.append_value('LINKFLAGS', ['-stdlib=libc++'])
        cfg.env.append_value('INCLUDES', '/usr/local/Cellar/libunwind-headers/35.3/include')  # brew install libunwind-headers
# Add macOS SDK paths
#        cfg.env.append_value('INCLUDES', [ ''.join( [ macosx_sdk_path(cfg), '/usr/include' ] ) ] ) 
#        cfg.env.append_value('LINKFLAGS', ''.join( [ '-L', macosx_sdk_path(cfg), '/usr/lib' ] ) ) 
    cfg.env.append_value('INCLUDES', [ run_llvm_config(cfg,"--includedir") ])
    cfg.env.append_value('INCLUDES', ['/usr/include'] )
    cfg.define("ENABLE_BACKTRACE_ARGS",1)
# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
#
# The following debugging flags slow down clasp
#  They should be disabled in production code
# 
    if (cfg.env.ADDRESS_SANITIZER):
        cfg.env.append_value('CXXFLAGS', ['-fsanitize=address'] )
        cfg.env.append_value('LINKFLAGS', ['-fsanitize=address'])
    if (cfg.env.DEBUG_GUARD):
        cfg.define("DEBUG_GUARD",1)
        cfg.define("DEBUG_GUARD_VALIDATE",1)
    if (cfg.env.DEBUG_GUARD_EXHAUSTIVE_VALIDATE):
        cfg.define("DEBUG_GUARD_EXHAUSTIVE_VALIDATE",1)
    cfg.define("DEBUG_MONITOR_SUPPORT",1) 
    if (cfg.env.DEBUG_OPTIONS):
        # on by default - figure out how to shut it off later and remove all of the code that depends on it
        for opt in cfg.env.DEBUG_OPTIONS:
            if (opt in DEBUG_OPTIONS):
                cfg.define(opt,1)
            else:
                raise Exception("Illegal DEBUG_OPTION %s - allowed options: %s" % (opt, DEBUG_OPTIONS))

# --------------------------------------------------
# --------------------------------------------------
# --------------------------------------------------
    cfg.env.append_value('CXXFLAGS', ['-Wno-macro-redefined'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-deprecated-register'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-expansion-to-defined'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-return-type-c-linkage'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-invalid-offsetof'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-#pragma-messages'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-inconsistent-missing-override'] )
    cfg.env.append_value('LIBPATH', ['/usr/lib', '/usr/local/lib'])
    cfg.env.append_value('STLIBPATH', ['/usr/lib', '/usr/local/lib'])
    cfg.env.append_value('LINKFLAGS', ['-fvisibility=default'])
    cfg.env.append_value('LINKFLAGS', ['-rdynamic'])
    sep = " "
    cfg.env.append_value('STLIB', cfg.extensions_stlib)
    cfg.env.append_value('LIB', cfg.extensions_lib)
    cfg.env.append_value('STLIB', cfg.env.STLIB_CLANG)
    cfg.env.append_value('STLIB', cfg.env.STLIB_LLVM)
    cfg.env.append_value('STLIB', cfg.env.STLIB_Z)
    if (cfg.env['LINK_STATIC']):
        cfg.env.append_value('STLIB', cfg.env.STLIB_BOOST)
    else:
        cfg.env.append_value('LIB', cfg.env.LIB_BOOST)
    log.info("About to check if appending LIB_FFI")
    if (cfg.env['DEST_OS'] == DARWIN_OS ):
        if (cfg.env['REQUIRE_LIBFFI'] == True):
            log.info("Appending LIB_FFI")
            cfg.env.append_value('LIB', cfg.env.LIB_FFI)
    if (cfg.env['DEST_OS'] == LINUX_OS or cfg.env['DEST_OS'] == FREEBSD_OS):
        cfg.env.append_value('LIB', cfg.env.LIB_DL)
        cfg.env.append_value('LIB', cfg.env.LIB_ELF)
        cfg.env.append_value('LIB', cfg.env.LIB_GCC_S)
        cfg.env.append_value('LIB', cfg.env.LIB_UNWIND_X86_64)
        cfg.env.append_value('LIB', cfg.env.LIB_UNWIND)
        cfg.env.append_value('LIB', cfg.env.LIB_LZMA)
    if (cfg.env['DEST_OS'] == LINUX_OS):
        cfg.env.append_value('LIB', cfg.env.LIB_BSD)
    cfg.env.append_value('LIB', cfg.env.LIB_CLANG)
    cfg.env.append_value('LIB', cfg.env.LIB_LLVM)
    cfg.env.append_value('LIB', cfg.env.LIB_NCURSES)
    cfg.env.append_value('LIB', cfg.env.LIB_M)
    cfg.env.append_value('LIB', cfg.env.LIB_GMP)
    cfg.env.append_value('LIB', cfg.env.LIB_Z)
    log.debug("cfg.env.STLIB = %s", cfg.env.STLIB)
    log.debug("cfg.env.LIB = %s", cfg.env.LIB)
####### Setup the variants
    env_copy = cfg.env.derive()
    log.info("About to setup variants - cfg.options.enabl_empi = %s" % cfg.options.enable_mpi)
    if (cfg.options.enable_mpi):
        log.info("Enabling MPI")
        mpi_variants = [False,True]
        cfg.env.ENABLE_MPI = True
    else:
        log.info("Disabling MPI")
        mpi_variants = [False]
        cfg.env.ENABLE_MPI = False
    for gc in GCS_NAMES:
        for enable_mpi in mpi_variants:
            for debug_build in [True, False]:
                if (enable_mpi):
                    mpi_part = "_mpi"
                else:
                    mpi_part = ""
                if (debug_build):
                    debug_part = "_d"
                else:
                    debug_part = ""
                variant_name = gc+mpi_part+debug_part
                variant_instance = eval("i" + variant_name + "()")
                log.pprint("Blue","Setting up variant: %s" % variant_instance.variant_dir())
                log.pprint("BLUE","variant_name = %s    variant_instance = %s" % (variant_name, variant_instance) )
                variant_instance.configure_variant(cfg, env_copy)
###### Final stuff
    if os.getenv("CLASP_SRC_DONTTOUCH") == None:
        update_dependencies(cfg)
    else:
        log.pprint('BLUE', 'not running update_dependencies(), leaving tree alone')

def pre_build_hook(bld):
    bld.build_start_time = time.time()
    log.info('Compilation started at %s', datetime.datetime.now())

def post_build_hook(bld):
    duration = round(time.time() - bld.build_start_time)
    log.info('Compilation finished in %s, at %s', str(datetime.timedelta(seconds = duration)), datetime.datetime.now())

def build(bld):
    def install(dest, files, cwd = bld.path, chmod = None):
        dest = '${PREFIX}/' + dest
        # NOTE: waf bug as of 2.0.6: if you call install_as(some-dir, file), then it chmod's away the x flag from some-dir
        if chmod:
            #print('install_as(%s, %s, ...)' % (dest, files))
            bld.install_as(dest, files, chmod = chmod)
        else:
            #print('install_files(%s, %s, ...)' % (dest, files))
            bld.install_files(dest, files, relative_trick = True, cwd = cwd)

    if not bld.variant:
        bld.fatal("Call waf with build_variant, e.g. './waf --jobs 2 --verbose build_cboehm'")

    variant = eval(bld.variant + "()")
    bld.variant_obj = variant

    # Reinitialize logging system with a handler that appends to ./build/variant/build.log
    log.reinitialize(console_level = logging.DEBUG if Logs.verbose >= 1 else logging.INFO,
                     log_file = os.path.join(bld.path.abspath(), out, variant.variant_dir(), "build.log"))

    log.debug('build() starts, CLASP_BUILD_MODE = %s, options: %s', bld.env.CLASP_BUILD_MODE, bld.options)
    bld.add_pre_fun(pre_build_hook);
    bld.add_post_fun(post_build_hook);

    stage = bld.stage
    stage_val = stage_value(bld,bld.stage)
    bld.stage_val = stage_val

    log.pprint('BLUE', 'build(), %s, %s, stage %s=%s%s' %
                (variant.variant_name(),
                 bld.env.CLASP_BUILD_MODE,
                 bld.stage,
                 bld.stage_val,
                 ", DEBUG_WHILE_BUILDING" if bld.options.DEBUG_WHILE_BUILDING else ''))

    # Waf groups are basically staging of tasks: all tasks in stage N must finish before any
    # task in stage N+1 can begin. (Don't get fooled by the API, groups are ordered internally.)
    # Tasks are recorded into the current stage which can be set by bld.set_group('stage-name').
    # NOTE: group based ordering overrides the set_input based task dependencies, so be careful
    # not to instantiate tasks into the wrong group.
    bld.add_group('preprocessing')
    bld.add_group('compiling/c++')
    bld.use_human_readable_bitcode = "USE_HUMAN_READABLE_BITCODE" in bld.env
    log.info("Using human readable bitcode: %s", bld.use_human_readable_bitcode)
    bld.clasp_source_files = collect_clasp_c_source_files(bld)

    bld.clasp_aclasp = collect_aclasp_lisp_files(wrappers = False)
    bld.clasp_bclasp = collect_bclasp_lisp_files()
    bld.clasp_cclasp = collect_cclasp_lisp_files()

    def find_lisp(bld,x):
        find = bld.path.find_node("%s.lsp"%x)
        if (find):
            return find.abspath()
        find = bld.path.find_node("%s.lisp"%x)
        if (find):
            return find.abspath()
        find = bld.path.find_or_declare("%s.lisp"%x)
        if (find):
            return find.abspath()
        return x
    
#    fout = open("/tmp/build.lisp", "w")
#    fout.write('(core:select-package :core)\n')
#    fout.write('(core:*make-special \'core::*number-of-jobs*)\n')
#    fout.write('(setq core::*number-of-jobs* 1)\n')
#    fout.write('(export \'core::*number-of-jobs*)\n')
#    fout.write('(load "%s" :verbose t)\n' % find_lisp(bld,"src/lisp/kernel/clasp-builder"))
#    fout.write('(core::remove-stage-features)\n')
#    fout.write('(setq *features* (cons :aclasp (cons :clasp-min *features*)))\n')
#    for x in bld.clasp_aclasp:
#        fout.write('(load "%s" :verbose t)\n' % find_lisp(bld,x))
#    for x in bld.clasp_aclasp:
#        fout.write('(load "%s" :verbose t)\n' % find_lisp(bld,x))
#    fout.write('(core::remove-stage-features)\n')
#    fout.write('(setq *features* (cons :bclasp (cons :clos *features*)))\n')
#    for x in bld.clasp_bclasp:
#        fout.write('(load "%s" :verbose t)\n' % find_lisp(bld,x))
#    fout.close()
    
    bld.clasp_cclasp_no_wrappers = collect_cclasp_lisp_files(wrappers = False)

    bld.extensions_include_dirs = []
    bld.extensions_include_files = []
    bld.extensions_source_files = []
    bld.extensions_gcinterface_include_files = []
    bld.extensions_builders = []

    bld.bclasp_executable = bld.path.find_or_declare(variant.executable_name(stage='b'))
    bld.cclasp_executable = bld.path.find_or_declare(variant.executable_name(stage='c'))
    bld.asdf_fasl_bclasp = bld.path.find_or_declare("%s/src/lisp/modules/asdf/asdf.fasl" % variant.fasl_dir(stage='b'))
    bld.asdf_fasl_cclasp = bld.path.find_or_declare("%s/src/lisp/modules/asdf/asdf.fasl" % variant.fasl_dir(stage='c'))
    bld.bclasp_fasl = variant.fasl_name(bld,stage='b')
    bld.cclasp_fasl = variant.fasl_name(bld,stage='c')
    bld.iclasp_executable = bld.path.find_or_declare(variant.executable_name(stage='i'))

    bld.set_group('compiling/c++')

    bld.recurse('extensions')

    log.info("There are %d extensions_builders", len(bld.extensions_builders))
    for x in bld.extensions_builders:
        x.run()

    bld.set_group('preprocessing')

    #
    # Installing the sources
    #
    clasp_c_source_files = bld.clasp_source_files + bld.extensions_source_files
    install('lib/clasp/', clasp_c_source_files)
    install('lib/clasp/', collect_waf_nodes(bld, 'include/clasp/', suffix = ".h"))
    # Gather lisp source files - but don't only use files with these extensions or we will miss lisp assets
    install('lib/clasp/', collect_waf_nodes(bld, 'src/lisp/')) # , suffix = [".lsp", ".lisp", ".asd"]))

    # If the bld.variant is not in bld.all_envs then we have a legal command but the variant wasn't configured
    #   this currently only happens if the user used: ./waf configure    without --enable-mp
    #   and then later did something like ./waf build_iboehm_mpi
    if (bld.variant in bld.all_envs):
        bld.env = bld.all_envs[bld.variant]
    else:
        log.pprint("RED","The variant %s is not configured - run ./waf configure --enable-mpi" % bld.variant)
        exit(1)
        
    include_dirs = ['.']
    include_dirs.append("%s/src/main/" % bld.path.abspath())
    include_dirs.append("%s/include/" % bld.path.abspath())
    include_dirs.append("%s/%s/%s/generated/" % (bld.path.abspath(), out, variant.variant_dir()))
    include_dirs = include_dirs + bld.extensions_include_dirs
    log.debug("include_dirs = %s", include_dirs)

    # Without this the parallel ASDF load-op's would step on each other's feet
    if (bld.options.RUN_THE_SCRAPER):
        task = precompile_scraper(env = bld.env)
        # TODO set the inputs to be all of the scraper dir? this would enable proper dependency tracking and recompilation.
        task.set_outputs([bld.path.find_or_declare("scraper-precompile-done")])
        bld.add_to_group(task)

    make_pump_tasks(bld, 'src/core/header-templates/', 'clasp/core/')
    make_pump_tasks(bld, 'src/clbind/header-templates/', 'clasp/clbind/')

    task = generate_extension_headers_h(env=bld.env)
    task.set_inputs(bld.extensions_gcinterface_include_files)
    task.set_outputs([bld.path.find_or_declare("generated/extension_headers.h")])
    bld.add_to_group(task)

    bld.set_group('compiling/c++')

    # Build the fork client

    bld(features='c cprogram', \
        source="src/fork-server/fork-client.c", \
        target="fork-client", \
        install_path="${PREFIX}/bin")

    # Always build the C++ code
    intrinsics_bitcode_node = bld.path.find_or_declare(variant.inline_bitcode_archive_name("intrinsics"))
    builtins_bitcode_node = bld.path.find_or_declare(variant.inline_bitcode_archive_name("builtins"))
    builtins_no_debug_info_bitcode_node = bld.path.find_or_declare(variant.inline_bitcode_archive_name("builtins-no-debug-info"))
    cclasp_common_lisp_output_name_list = variant.common_lisp_output_name_list(bld,bld.clasp_cclasp,stage='c')
    cxx_all_bitcode_node = bld.path.find_or_declare(variant.cxx_all_bitcode_name())
    out_dir_node = bld.path.find_dir(out)
    bclasp_symlink_node = out_dir_node.make_node("bclasp")
    bld_task = bld.program(source = clasp_c_source_files,
                           includes = include_dirs,
                           target = [bld.iclasp_executable],
                           install_path = '${PREFIX}/bin')

    #make_run_dsymutil_task(bld, 'i', iclasp_lto_o)

    if (bld.stage_val <= -1):
        log.info("Creating run_aclasp task")

        task = run_aclasp(env=bld.env)
        task.set_inputs([bld.iclasp_executable,
                         intrinsics_bitcode_node,
                         builtins_bitcode_node] +
                        waf_nodes_for_lisp_files(bld, bld.clasp_aclasp))
        aclasp_common_lisp_output_name_list = variant.common_lisp_output_name_list(bld, bld.clasp_aclasp, stage = 'a')
        task.set_outputs(aclasp_common_lisp_output_name_list)
        bld.add_to_group(task)
    if (bld.stage_val >= 1):
        log.info("Creating compile_aclasp task")

        task = compile_aclasp(env=bld.env)
        task.set_inputs([bld.iclasp_executable,
                         intrinsics_bitcode_node,
                         builtins_bitcode_node] +
                        waf_nodes_for_lisp_files(bld, bld.clasp_aclasp))
        aclasp_common_lisp_output_name_list = variant.common_lisp_output_name_list(bld, bld.clasp_aclasp, stage = 'a')
        log.debug("find_or_declare aclasp_common_lisp_output_name_list = %s", aclasp_common_lisp_output_name_list)
        task.set_outputs(aclasp_common_lisp_output_name_list)
        bld.add_to_group(task)

        aclasp_link_product = variant.fasl_name(bld,stage = 'a')
        task = link_fasl(env=bld.env)
        task.set_inputs([builtins_bitcode_node,
                         intrinsics_bitcode_node] +
                         aclasp_common_lisp_output_name_list)
        task.set_outputs([aclasp_link_product])
        bld.add_to_group(task)

        install('lib/clasp/', aclasp_link_product)
        install('lib/clasp/', aclasp_common_lisp_output_name_list)
    if (bld.stage_val >= 2):
        log.info("Creating compile_bclasp task")

        task = compile_bclasp(env=bld.env)
        task.set_inputs([bld.iclasp_executable,
                         aclasp_link_product,
                         intrinsics_bitcode_node,
                         builtins_bitcode_node] +
                        waf_nodes_for_lisp_files(bld, bld.clasp_bclasp))
        bclasp_common_lisp_output_name_list = variant.common_lisp_output_name_list(bld, bld.clasp_bclasp, stage = 'b')
        task.set_outputs(bclasp_common_lisp_output_name_list)
        bld.add_to_group(task)

        task = link_fasl(env=bld.env)
        task.set_inputs([builtins_bitcode_node,
                         intrinsics_bitcode_node] +
                         bclasp_common_lisp_output_name_list)
        task.set_outputs([bld.bclasp_fasl])
        bld.add_to_group(task)

        install('lib/clasp/', bld.bclasp_fasl)
        install('lib/clasp/', bclasp_common_lisp_output_name_list)

        if (False):   # build bclasp executable
            task = link_executable(env = bld.env)
            task.set_inputs(bclasp_common_lisp_output_name_list +
                            [cxx_all_bitcode_node])
            log.info("About to try and recurse into extensions again")
            bld.recurse('extensions')
            if ( bld.env['DEST_OS'] == DARWIN_OS ):
                if (bld.env.LTO_FLAG):
                    bclasp_lto_o = bld.path.find_or_declare('%s_exec.lto.o' % variant.executable_name(stage = 'b'))
                    task.set_outputs([bld.bclasp_executable,
                                      bclasp_lto_o])
                else:
                    bclasp_lto_o = None
                    task.set_outputs([bld.bclasp_executable])
            else:
                bclasp_lto_o = None
                task.set_outputs(bld.bclasp_executable)
            log.debug("link_executable for bclasp %s -> %s", task.inputs, task.outputs)
            bld.add_to_group(task)

            make_run_dsymutil_task(bld, 'b', bclasp_lto_o)

            install('bin/%s' % bld.bclasp_executable.name, bld.bclasp_executable, chmod = Utils.O755)
            bld.symlink_as('${PREFIX}/bin/clasp', bld.bclasp_executable.name)
            os.symlink(bld.bclasp_executable.abspath(), bclasp_symlink_node.abspath())
        # # Build ASDF for bclasp
        # cmp_asdf = compile_module(env = bld.env)
        # cmp_asdf.set_inputs([bld.iclasp_executable, bld.bclasp_fasl] + waf_nodes_for_lisp_files(bld, ["src/lisp/modules/asdf/build/asdf"]))
        # cmp_asdf.set_outputs(bld.asdf_fasl_bclasp)
        # bld.add_to_group(cmp_asdf)
        # install_files('lib/clasp/', bld.asdf_fasl_bclasp)
    if (bld.stage_val >= 3):
        log.info("Creating compile_cclasp task")
        # Build cclasp fasl
        task = compile_cclasp(env=bld.env)
        task.set_inputs([bld.iclasp_executable,
                         bld.bclasp_fasl,
                         cxx_all_bitcode_node] +
                        waf_nodes_for_lisp_files(bld, bld.clasp_cclasp))
        task.set_outputs(cclasp_common_lisp_output_name_list)
        bld.add_to_group(task)
    if (bld.stage == 'rebuild' or bld.stage == 'dangerzone'):
        log.pprint('RED', "!------------------------------------------------------------")
        log.pprint('RED', "!   You have entered the dangerzone!  ")
        log.pprint('RED', "!   While you wait...  https://www.youtube.com/watch?v=kyAn3fSs8_A")
        log.pprint('RED', "!------------------------------------------------------------")
        # Build cclasp
        task = recompile_cclasp(env = bld.env)
        task.set_inputs(waf_nodes_for_lisp_files(bld, bld.clasp_cclasp_no_wrappers))
        task.set_outputs(cclasp_common_lisp_output_name_list)
        bld.add_to_group(task)
    if (bld.stage == 'dangerzone' or bld.stage == 'rebuild' or bld.stage_val >= 3):
        task = link_fasl(env=bld.env)
        task.set_inputs([builtins_bitcode_node,
                         intrinsics_bitcode_node] +
                         cclasp_common_lisp_output_name_list)
        task.set_outputs([bld.cclasp_fasl])
        bld.add_to_group(task)

        install('lib/clasp/', bld.cclasp_fasl)
    if (bld.stage == 'rebuild' or bld.stage_val >= 3):
        install('lib/clasp/', cclasp_common_lisp_output_name_list)

        # Build serve-event
        serve_event_fasl = bld.path.find_or_declare("%s/src/lisp/modules/serve-event/serve-event.fasl" % variant.fasl_dir(stage = 'c'))
        task = compile_module(env=bld.env)
        task.set_inputs([bld.iclasp_executable,
                         bld.cclasp_fasl] +
                        waf_nodes_for_lisp_files(bld, ["src/lisp/modules/serve-event/serve-event"]))
        task.set_outputs(serve_event_fasl)
        bld.add_to_group(task)
        install('lib/clasp/', serve_event_fasl)

        # Build ASDF
        task = compile_module(env=bld.env)
        task.set_inputs([bld.iclasp_executable,
                         bld.cclasp_fasl] +
                        waf_nodes_for_lisp_files(bld, ["src/lisp/modules/asdf/build/asdf"]))
        task.set_outputs(bld.asdf_fasl_cclasp)
        bld.add_to_group(task)

        install('lib/clasp/', bld.asdf_fasl_cclasp)

        clasp_symlink_node = out_dir_node.make_node("clasp")
        log.debug("clasp_symlink_node =  %s", clasp_symlink_node)
        if (os.path.islink(clasp_symlink_node.abspath())):
            os.unlink(clasp_symlink_node.abspath())
    if (bld.stage == 'rebuild' or bld.stage_val >= 4):
        if (True):   # build cclasp executable
            task = link_executable(env = bld.env)
            task.set_inputs(cclasp_common_lisp_output_name_list +
                            [cxx_all_bitcode_node])
            log.info("About to try and recurse into extensions again")
            bld.recurse('extensions')
            if ( bld.env['DEST_OS'] == DARWIN_OS ):
                if (bld.env.LTO_FLAG):
                    cclasp_lto_o = bld.path.find_or_declare('%s_exec.lto.o' % variant.executable_name(stage = 'c'))
                    task.set_outputs([bld.cclasp_executable,
                                      cclasp_lto_o])
                else:
                    cclasp_lto_o = None
                    task.set_outputs([bld.cclasp_executable])
            else:
                cclasp_lto_o = None
                task.set_outputs(bld.cclasp_executable)
            log.debug("link_executable for cclasp %s -> %s", task.inputs, task.outputs)
            bld.add_to_group(task)

            make_run_dsymutil_task(bld, 'c', cclasp_lto_o)

            install('bin/%s' % bld.cclasp_executable.name, bld.cclasp_executable, chmod = Utils.O755)
            bld.symlink_as('${PREFIX}/bin/clasp', bld.cclasp_executable.name)
            os.symlink(bld.cclasp_executable.abspath(), clasp_symlink_node.abspath())
        else:
            os.symlink(bld.iclasp_executable.abspath(), clasp_symlink_node.abspath())
    log.pprint('BLUE', 'build() has finished')

def init(ctx):
    from waflib.Build import BuildContext, CleanContext, InstallContext, UninstallContext, ListContext, StepContext, EnvContext
    log.pprint('BLUE', "Running init() - this constructs the matrix of legal waf operation names")
    for gc in GCS_NAMES:
        for enable_mpi in ["", "_mpi"]:
            for debug_build in ["", "_d"]:
                variant_name = "%s%s%s" % (gc,enable_mpi,debug_build)
                for ctx in (BuildContext, CleanContext, InstallContext, UninstallContext, ListContext, StepContext, EnvContext):
                    name = ctx.__name__.replace('Context','').lower()
                    for stage_char in STAGE_CHARS:
                        # This instantiates classes, all with the same name 'tmp'.
                        class tmp(ctx):
                            variant = variant_name
                            cmd = name + '_' + stage_char + variant_name
                            stage = stage_char

            # NOTE: these are kinda bitrotten, but left here for now as a reference
            class tmp(BuildContext):
                variant = variant_name
                cmd = 'rebuild_c' + variant
                stage = 'rebuild'
            class tmp(BuildContext):
                variant = variant_name
                cmd = 'dangerzone_c' + variant
                stage = 'dangerzone'

#
#
# Tasks
#
#
def make_run_dsymutil_task(bld, stage_char, clasp_lto_o):
    if bld.env['DEST_OS'] == DARWIN_OS:
        variant = bld.variant_obj
        dsym_file = bld.path.find_or_declare("%s.dSYM" % variant.executable_name(stage = stage_char))
        dsym_nodes = dsym_waf_nodes(variant.executable_name(stage = stage_char), dsym_file)
        log.debug(stage_char + "clasp dsym_nodes = %s", dsym_nodes)
        task = run_dsymutil(env = bld.env)
        inputs = [bld.path.find_or_declare(variant.executable_name(stage = stage_char))]
        if clasp_lto_o:
            inputs += [clasp_lto_o]
        task.set_inputs(inputs)
        task.set_outputs(dsym_nodes)
        bld.add_to_group(task)
        bld.install_files('${PREFIX}/bin/%s' % dsym_file.name, dsym_nodes, relative_trick = True, cwd = dsym_file)

class run_dsymutil(clasp_task):
    color = 'BLUE';
    def run(self):
        cmd = 'dsymutil %s' % self.inputs[0]
        return self.exec_command(cmd)

class link_fasl(clasp_task):
    def run(self):
        if (self.env.CLASP_BUILD_MODE=='fasl'):
            log.debug("link_fasl self.inputs[3] = %s   self.outputs[0] = %s", self.inputs[3], self.outputs[0])
            cmd = [ "cp", self.inputs[3].abspath(),self.outputs[0].abspath()]
            return self.exec_command(cmd)
        if (self.env.LTO_FLAG):
            lto_option = self.env.LTO_FLAG
            if (self.env.USE_PARALLEL_BUILD and self.bld.stage_val < 3):
                lto_optimize_flag = "-O0"
                log.debug("With USE_PARALLEL_BUILD and stage_val = %d dropping down to -O0 for link_fasl", self.bld.stage_val)
            else:
                lto_optimize_flag = "-O2"
        else:
            lto_option = ""
            lto_optimize_flag = ""
        link_options = self.bld.env['LINKFLAGS']
        if (self.env['DEST_OS'] == DARWIN_OS):
            link_options = link_options + [ "-flat_namespace", "-undefined", "suppress", "-bundle" ]
        else:
            link_options = link_options + [ "-shared" ]
        cmd = [self.env.CXX[0]] + \
                  waf_nodes_to_paths(self.inputs) + \
                  [ lto_option, lto_optimize_flag ] + \
                  link_options + \
                  [ "-o", self.outputs[0].abspath() ]
        return self.exec_command(cmd)

class link_executable(clasp_task):
    def run(self):
        if (self.env.LTO_FLAG):
            lto_option_list = [self.env.LTO_FLAG,"-O2"]
            if (self.env['DEST_OS'] == DARWIN_OS ):
                lto_object_path_lto = ["-Wl,-object_path_lto,%s" % self.outputs[1].abspath()]
            else:
                lto_object_path_lto = []
        else:
            lto_option_list = []
            lto_object_path_lto = []
        link_options = []
        if (self.env['DEST_OS'] == DARWIN_OS ):
            link_options = link_options + [ LTO_OPTION, "-v", '-Wl,-stack_size,0x1000000']
        cmd = [ self.env.CXX[0] ] + \
              waf_nodes_to_paths(self.inputs) + \
              self.env['LINKFLAGS'] + \
              self.env['LDFLAGS']  + \
              prefix_list_elements_with(self.env['LIBPATH'], '-L') + \
              prefix_list_elements_with(self.env['STLIBPATH'], '-L') + \
              libraries_as_link_flags(self.env.STLIB_ST,self.env.STLIB) + \
              libraries_as_link_flags(self.env.LIB_ST,self.env.LIB) + \
              lto_option_list + \
              link_options + \
              lto_object_path_lto + \
              [ "-o", self.outputs[0].abspath()]
        return self.exec_command(cmd)

class run_aclasp(clasp_task):
    def run(self):
        executable = self.inputs[0].abspath()
        log.debug("In run_aclasp %s", executable)
        cmd = self.clasp_command_line(executable,
                                      image = False,
                                      features = ["no-implicit-compilation",
                                                  "jit-log-symbols",
                                                  "clasp-min"],
                                      forms = ['(load "sys:kernel;clasp-builder.lsp")',
                                               '(load-aclasp)'],
                                      *self.bld.clasp_aclasp)
        return self.exec_command(cmd)

class compile_aclasp(clasp_task):
    def run(self):
        executable = self.inputs[0].abspath()
        output_file = self.outputs[0].abspath()
        log.debug("In compile_aclasp %s -> %s", executable, output_file)
        cmd = self.clasp_command_line(executable,
                                      image = False,
                                      features = ["clasp-min"],
                                      forms = ['(setq *features* (cons :aclasp *features*))',
                                               '(load "sys:kernel;clasp-builder.lsp")',
                                               #'(load-aclasp)',
                                               '(setq core::*number-of-jobs* %d)' % self.bld.jobs,
                                               '(core:compile-aclasp :output-file #P"%s")' % output_file,
                                               '(core:quit)'],
                                      *self.bld.clasp_aclasp)
        return self.exec_command(cmd)

class compile_bclasp(clasp_task):
    def run(self):
        executable = self.inputs[0].abspath()
        image_file = self.inputs[1].abspath()
        output_file = self.outputs[0].abspath()
        log.debug("In compile_bclasp %s %s -> %s", executable, image_file, output_file)
        cmd = self.clasp_command_line(executable,
                                      image = image_file,
                                      features = [],
                                      forms = ['(setq *features* (cons :bclasp *features*))',
                                               '(load "sys:kernel;clasp-builder.lsp")',
                                               '(setq core::*number-of-jobs* %d)' % self.bld.jobs,
                                               '(core:compile-bclasp :output-file #P"%s")' % output_file,
                                               '(core:quit)'],
                                      *self.bld.clasp_bclasp)

        return self.exec_command(cmd)

class compile_cclasp(clasp_task):
    def run(self):
        executable = self.inputs[0].abspath()
        image_file = self.inputs[1].abspath()
        output_file = self.outputs[0].abspath()
        log.debug("In compile_cclasp %s --image %s -> %s", executable, image_file, output_file)
        forms = ['(setq *features* (cons :cclasp *features*))',
                 '(load "sys:kernel;clasp-builder.lsp")',
                 '(setq core::*number-of-jobs* %d)' % self.bld.jobs]
        if (self.bld.options.LOAD_CCLASP):
            forms += ['(load-cclasp)']
        else:
            forms += ['(core:compile-cclasp :output-file #P"%s")' % output_file,
                      '(core:quit)']
        cmd = self.clasp_command_line(executable,
                                      image = image_file,
                                      features = [],
                                      forms = forms,
                                      *self.bld.clasp_cclasp)
        return self.exec_command(cmd)

class recompile_cclasp(clasp_task):
    def run(self):
        env = self.env
        other_clasp = env.CLASP or "clasp"
        output_file = self.outputs[0]
        log.debug("In recompile_cclasp %s -> %s", other_clasp, output_file)
        if not os.path.isfile(other_clasp):
            raise Exception("To use the recompile targets you need to provide a working clasp executable. See wscript.config and/or set the CLASP env variable.")
        cmd = self.clasp_command_line(other_clasp,
                                      features = ['ignore-extensions'],
                                      resource_dir = os.path.join(self.bld.path.abspath(), out, self.bld.variant_obj.variant_dir()),
                                      forms = ['(setq *features* (cons :cclasp *features*))',
                                               '(load "sys:kernel;clasp-builder.lsp")',
                                               '(setq core::*number-of-jobs* %d)' % self.bld.jobs,
                                               '(core:recompile-cclasp :output-file #P"%s")' % output_file,
                                               '(core:quit)'],
                                      *self.bld.clasp_cclasp_no_wrappers)
        return self.exec_command(cmd)

class compile_module(clasp_task):
    def run(self):
        executable = self.inputs[0].abspath()
        image_file = self.inputs[1].abspath()
        source_file = self.inputs[2].abspath()
        fasl_file = self.outputs[0].abspath()
        log.debug("In compile_module %s --image %s, %s -> %s", executable, image_file, source_file, fasl_file)
        cmd = self.clasp_command_line(executable,
                                      image = image_file,
                                      features = ['ignore-extensions'],
                                      forms = ['(compile-file #P"%s" :output-file #P"%s" :output-type :fasl)' % (source_file, fasl_file),
                                               '(core:quit)'])
        return self.exec_command(cmd)

class generate_extension_headers_h(clasp_task):
    def run(self):
        log.debug("generate_extension_headers_h running, inputs: %s", self.inputs)
        output_file = self.outputs[0].abspath()
        new_contents = "// Generated by the wscript generate_extension_headers_h task - Editing it is unwise!\n"
        old_contents = ""
        for x in self.inputs[1:]:
            new_contents += ("#include \"%s\"\n" % x.abspath())
        if os.path.isfile(output_file):
            fin = open(output_file, "r")
            old_contents = fin.read()
            fin.close()
        if old_contents != new_contents:
            log.debug("Writing to %s", output_file)
            fout = open(output_file, "w")
            fout.write(new_contents)
            fout.close()
        else:
            log.debug("NOT writing to %s - it is unchanged", output_file)

class link_bitcode(clasp_task):
    ext_out = ['.a']    # this affects the task execution order

    def run(self):
        all_inputs = StringIO()
        for f in self.inputs:
            all_inputs.write(' %s' % f.abspath())
        cmd = "" + self.env.LLVM_AR_BINARY + " ru %s %s" % (self.outputs[0], all_inputs.getvalue())
        print("link_bitcode command: %s" % cmd);
        return self.exec_command(cmd)

class build_bitcode(clasp_task):
    def run(self):
        env = self.env
        cmd = [] + \
              env.CXX + \
              self.colon("ARCH_ST", "ARCH") + \
              env.CXXFLAGS + env.CPPFLAGS + \
              [ '-emit-llvm' ] + \
              self.colon("FRAMEWORKPATH_ST", "FRAMEWORKPATH") + \
              self.colon("CPPPATH_ST", "INCPATHS") + \
              self.colon("DEFINES_ST", "DEFINES") + \
              [ self.inputs[0].abspath() ] + \
              [ '-c' ] + \
              [ '-g' ] + \
              [ '-o', self.outputs[0].abspath() ]
#        cmd.remove("-g")
        return self.exec_command(cmd)


class build_bitcode_no_debug_info(clasp_task):
    def run(self):
        env = self.env
        cmd = [] + \
              env.CXX + \
              self.colon("ARCH_ST", "ARCH") + \
              env.CXXFLAGS + env.CPPFLAGS + \
              [ '-emit-llvm' ] + \
              self.colon("FRAMEWORKPATH_ST", "FRAMEWORKPATH") + \
              self.colon("CPPPATH_ST", "INCPATHS") + \
              self.colon("DEFINES_ST", "DEFINES") + \
              [ self.inputs[0].abspath() ] + \
              [ '-c' ] + \
              [ '-o', self.outputs[0].abspath() ]
        cmd.remove("-g")
        return self.exec_command(cmd)

class scraper_task(clasp_task):
    def scraper_command_line(self, extraCommands = [], scraperArgs = []):
        env = self.env
        bld = self.generator.bld
        scraper_home = os.path.join(env.BUILD_ROOT, "src/scraper/")
        fasl_dir = os.path.join(bld.path.abspath(), out, "host-fasl/")
        cmd = [] + env.SCRAPER_LISP + [
            "--eval", "(require :asdf)",
            "--eval", "(asdf:initialize-source-registry '(:source-registry (:directory \"%s\") :ignore-inherited-configuration))" % scraper_home,
            "--eval", "(asdf:initialize-output-translations '(:output-translations (t (\"%s\" :implementation)) :inherit-configuration))" % fasl_dir,
            "--load", os.path.join(env.BUILD_ROOT, "src/scraper/dependencies/bundle.lisp"),
            "--eval", "(let ((uiop:*uninteresting-conditions* (list* 'style-warning uiop:*usual-uninteresting-conditions*)) (*compile-print* nil) (*compile-verbose* nil)) (asdf:load-system :clasp-scraper))",
            ] + extraCommands + [
            "--eval", "(quit)", "--end-toplevel-options"] + scraperArgs
        return cmd

class precompile_scraper(scraper_task):
    weight = 5    # Tell waf to run this early among the equal tasks because it will take long
    before = ['expand_pump_template']

    def run(self):
        cmd = self.scraper_command_line(["--eval", '(with-open-file (stream "%s" :direction :output :if-exists :supersede) (terpri stream))' % self.outputs[0].abspath()])
        return self.exec_command(cmd)

class generate_sif_files(scraper_task):
    ext_out = ['.sif']    # this affects the task execution order
    after = ['expand_pump_template']
    waf_print_keyword = "Scraping"

    def run(self):
        env = self.env
        preproc_args = [] + env.CXX + ["-E", "-DSCRAPING"] + self.colon("ARCH_ST", "ARCH") + env.CXXFLAGS + env.CPPFLAGS + \
                       self.colon("FRAMEWORKPATH_ST", "FRAMEWORKPATH") + \
                       self.colon("CPPPATH_ST", "INCPATHS") + \
                       self.colon("DEFINES_ST", "DEFINES")
        # NOTE if we ever want to turn the scraper into an executable sbcl image, then we will need to pass
        # every argument on the command line. Passing the preprocessor args is a headache due to escaping and
        # such, so for now we just pass it as an arg in the --eval form.
        preproc_args_as_string = ' '.join('"' + item + '"' for item in preproc_args)
        cmd = self.scraper_command_line(["--eval", "(cscrape:generate-sif-files \"%s\" '(%s))" % (env.BUILD_ROOT + "/", preproc_args_as_string)])
        for cxx_node, sif_node in zip(self.inputs, self.outputs):
            cmd.append(cxx_node.abspath())
            cmd.append(sif_node.abspath())
        return self.exec_command(cmd)

class generate_headers_from_all_sifs(scraper_task):
    waf_print_keyword = "Scraping, generate-headers-from-all-sifs"

    def run(self):
        env = self.env
        bld = self.generator.bld
        cmd = self.scraper_command_line(["--eval", "(cscrape:generate-headers-from-all-sifs)"],
                                        [os.path.join(bld.path.abspath(), out, bld.variant_obj.variant_dir() + "/"),
                                         env.BUILD_ROOT + "/"])
        for f in self.inputs:
            cmd.append(f.abspath())
        return self.exec_command(cmd)

def make_pump_tasks(bld, template_dir, output_dir):
    log.debug("Building pump tasks: %s -> %s", template_dir, output_dir)
    templates = collect_waf_nodes(bld, template_dir, suffix = '.pmp')
    assert len(templates) > 0
    for template_node in templates:
        template_name = template_node.name
        output_path = os.path.join("generated/", output_dir, template_name.replace(".pmp", ".h"))
        output_node = bld.path.find_or_declare(output_path)
        log.debug("Creating expand_pump_template: %s -> %s", template_node.abspath(), output_node.abspath())
        assert output_node
        task = expand_pump_template(env = bld.env)
        task.set_inputs([template_node])
        task.set_outputs([output_node])
        bld.add_to_group(task)
        bld.install_files('${PREFIX}/lib/clasp/', [output_node], relative_trick = True, cwd = bld.path)
    log.info("Created %s pump template tasks found in dir: %s", len(templates), template_dir)

class expand_pump_template(clasp_task):
    ext_out  = ['.h']      # this affects the task execution order

    def run(self):
        assert len(self.inputs) == len(self.outputs) == 1
        cmd = ['python',
               os.path.join(self.bld.path.abspath(), "tools-for-build/pump.py"),
               self.inputs[0].abspath(),
               self.outputs[0].abspath()]
        return self.exec_command(cmd)

#
#
# TaskGen's
#
#
@TaskGen.feature('cxx')
@TaskGen.after('process_source')
def postprocess_all_c_tasks(self):

    def install(dest, files, cwd = self.bld.path):
        dest = '${PREFIX}/' + dest
        self.bld.install_files(dest, files, relative_trick = True, cwd = cwd)

    if (not 'variant_obj' in self.bld.__dict__):
        log.debug("It's not the main build, bailing out from postprocess_all_c_tasks")
        return

    if (not self.bld.options.RUN_THE_SCRAPER):
        log.warn("Skipping scrape and some C tasks as requested")
        return

    variant = self.bld.variant_obj
    all_o_nodes = []
    all_cxx_nodes = []
    intrinsics_o = None
    builtins_o = None
    builtins_no_debug_info_o = None
    for task in self.compiled_tasks:
        log.debug("Scrape taskgen inspecting C task %s", task)
        if ( task.__class__.__name__ == 'cxx' ):
            for node in task.inputs:
                all_cxx_nodes.append(node)
                if ends_with(node.name, 'intrinsics.cc'):
                    intrinsics_cc = node
                if ends_with(node.name, 'builtins.cc'):
                    builtins_cc = node
##switch back to old way to create scraper tasks                    
#                sif_node = node.change_ext('.sif')
#                self.create_task('generate_one_sif', node, [sif_node])
#                all_sif_files.append(sif_node)
            for node in task.outputs:
                all_o_nodes.append(node)
                if ends_with(node.name, 'intrinsics.cc'):
                    intrinsics_o = node
                if ends_with(node.name, 'builtins.cc'):
                    builtins_o = node
                    builtins_no_debug_info_o = node
        if ( task.__class__.__name__ == 'c' ):
            for node in task.outputs:
                all_o_nodes.append(node)

    all_sif_nodes = []

    # Start 'job_count' number of parallel scraper processes, each processing several files at once
    job_count = self.bld.jobs
    job_count = 8 # KLUDGE fixed to 8 jobs because waf seems to redo the entire scraping when the task layout changes, e.g. due to a different ./waf --jobs xx
    if True:
        # Split into task chunks of at most 20 files each
        chunk_size = min(20, max(1, len(all_cxx_nodes) // job_count))
        chunks = list(list_chunks_of_size(all_cxx_nodes, chunk_size))
    else:
        # Split into the optimal sized chunks
        chunks = split_list(all_cxx_nodes, min(job_count, len(all_cxx_nodes)))
        chunk_size = len(chunks[0])

    log.info('Creating %s parallel scraper tasks, each processing %s files, for the total %s cxx files', len(chunks), chunk_size, len(all_cxx_nodes))
    assert len([x for sublist in chunks for x in sublist]) == len(all_cxx_nodes)
    for cxx_nodes in chunks:
        sif_nodes = [x.change_ext('.sif') for x in cxx_nodes]
        all_sif_nodes += sif_nodes
        self.create_task('generate_sif_files', cxx_nodes, sif_nodes)

    scraper_output_nodes = [self.path.find_or_declare('generated/' + i) for i in
                            ['c-wrappers.h',
                             'cl-wrappers.lisp',
                             'enum_inc.h',
                             'initClassesAndMethods_inc.h',
                             'initFunctions_inc.h',
                             'initializers_inc.h',
                             'sourceInfo_inc.h',
                             'symbols_scraped_inc.h']]

    self.create_task('generate_headers_from_all_sifs', all_sif_nodes, scraper_output_nodes)
    # TODO FIXME this includes cl-wrappers.lisp
    install('lib/clasp/', scraper_output_nodes)  # includes

# intrinsics
    intrinsics_bitcode_archive_node = self.path.find_or_declare(variant.inline_bitcode_archive_name("intrinsics"))
    intrinsics_bitcode_alone_node = self.path.find_or_declare(variant.inline_bitcode_name("intrinsics"))
    log.debug("intrinsics_cc = %s, intrinsics_o.name = %s, intrinsics_bitcode_alone_node = %s", intrinsics_cc, intrinsics_o.name, intrinsics_bitcode_alone_node)
    self.create_task('build_bitcode',
                     [intrinsics_cc] + scraper_output_nodes,
                     intrinsics_bitcode_alone_node)
    self.create_task('link_bitcode',
                     [intrinsics_o],
                     intrinsics_bitcode_archive_node)
    install('lib/clasp/', intrinsics_bitcode_archive_node)   # install bitcode
    install('lib/clasp/', intrinsics_bitcode_alone_node)     # install bitcode

# builtins
    builtins_bitcode_archive_node = self.path.find_or_declare(variant.inline_bitcode_archive_name("builtins"))
    builtins_bitcode_alone_node = self.path.find_or_declare(variant.inline_bitcode_name("builtins"))
    log.debug("builtins_cc = %s, builtins_o.name = %s, builtins_bitcode_alone_node = %s", builtins_cc, builtins_o.name, builtins_bitcode_alone_node)
    self.create_task('build_bitcode',
                     [builtins_cc] + scraper_output_nodes,
                     builtins_bitcode_alone_node)
    self.create_task('link_bitcode',
                     [builtins_o],
                     builtins_bitcode_archive_node)
    install('lib/clasp/', builtins_bitcode_archive_node)
    install('lib/clasp/', builtins_bitcode_alone_node)

# builtins with no debug info
    builtins_no_debug_info_bitcode_archive_node = self.path.find_or_declare(variant.inline_bitcode_archive_name("builtins-no-debug-info"))
    builtins_no_debug_info_bitcode_alone_node = self.path.find_or_declare(variant.inline_bitcode_name("builtins-no-debug-info"))
    log.debug("No debug-info builtins_cc = %s, builtins_no_debug_info_o.name = %s, builtins_no_debug_info_bitcode_alone_node = %s", builtins_cc, builtins_no_debug_info_o.name, builtins_no_debug_info_bitcode_alone_node)
    self.create_task('build_bitcode_no_debug_info',
                     [builtins_cc] + scraper_output_nodes,
                     builtins_no_debug_info_bitcode_alone_node)
    self.create_task('link_bitcode',
                     [builtins_no_debug_info_o],
                     builtins_no_debug_info_bitcode_archive_node)
    install('lib/clasp/', builtins_no_debug_info_bitcode_archive_node)
    install('lib/clasp/', builtins_no_debug_info_bitcode_alone_node)

#all of C
    cxx_all_bitcode_node = self.path.find_or_declare(variant.cxx_all_bitcode_name())
    self.create_task('link_bitcode',
                     all_o_nodes,
                     cxx_all_bitcode_node)
    install('lib/clasp/', cxx_all_bitcode_node)
