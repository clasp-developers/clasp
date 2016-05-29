top = '.'
out = 'wbuild'
APPNAME = 'clasp'
VERSION = '0.0'

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
    cfg.env.append_value('CXXFLAGS', ['-I../src/main'] )
    cfg.env.append_value('CXXFLAGS', ['-I/usr/local/Cellar/gmp/6.0.0a/include'] )
    cfg.env.append_value('CXXFLAGS', ['-I/Users/meister/Development/externals-clasp/build/release/include'])
    cfg.env.append_value('CXXFLAGS', ['-I/usr/local/Cellar/gmp/6.0.0a/include'] )
    cfg.env.append_value('CXXFLAGS', ['-I/opt/local/include'] )
    cfg.env.append_value('CXXFLAGS', ['-I/Users/meister/Development/clasp/build/clasp/Contents/Resources/lib/common/include'] )
    cfg.env.append_value('CXXFLAGS', ['-I/Users/meister/Development/clasp/include'] )
    cfg.env.append_value('CXXFLAGS', ['-I/Users/meister/Development/clasp/src/main/include'] )
    cfg.env.append_value('CXXFLAGS', ['-I/usr/include'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-deprecated-register'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-expansion-to-defined'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-#pragma-messages'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-inconsistent-missing-override'] )
#    cfg.check(features='cxx cxxprogram', lib=['pthread'], uselib_store='PTHREAD')
#    cfg.env.append_value('LINKFLAGS_MYSDK', mysdk + '/mysdk.so')
#    cfg.env.LINKFLAGS_PYEXT_MYSDK = cfg.env.LINKFLAGS_PYEXT
#    cfg.env.append_value('LINKFLAGS_PYEXT_MYSDK', mysdk + '/mysdk.so')

def build(bld):
#    bld(name='myInclude', export_includes=[bld.env.MY_MYSDK, 'include'])
    bld.recurse('src') #  core cffi llvmo asttooling')

# Have all 'cxx' targets have 'include' in their include paths.
from waflib import TaskGen
@TaskGen.taskgen_method
@TaskGen.feature('cxx')
def add_include(self):
    self.use = self.to_list(getattr(self, 'use', [])) + ['include']
