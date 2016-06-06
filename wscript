import os
import StringIO

top = '.'
out = 'wbuild'
APPNAME = 'clasp'
VERSION = '0.0'


VARIANTS = [ 'clasp_boehm_o',
             'clasp_boehm_d',
             'clasp_boehmdc_o',
             'clasp_boehmdc_d',
             'clasp_mpsprep_o',
             'clasp_mpsprep_d',
             'clasp_mps_o',
             'clasp_mps_d' ]

class variant():
    pass

class clasp_boehm_o(variant):
    install_name = 'clasp-boehm-o'
    def configure(self,cfg,env_copy):
        cfg.setenv("clasp_boehm_o", env=env_copy.derive())
        cfg.define("USE_BOEHM",1)
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3' ])
        cfg.env.append_value('CFLAGS', [ '-O3' ])
        cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.install_name)
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        cfg.write_config_header('clasp_boehm_o/config.h', remove=False)
    def install(self):
        return self.install_name

class clasp_boehm_d(variant):
    install_name = 'clasp-boehm-d'
    def configure(self,cfg,env_copy):
        cfg.setenv("clasp_boehm_d", env=env_copy.derive())
        cfg.define("USE_BOEHM",1)
        cfg.define("_DEBUG_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-g' ])
        cfg.env.append_value('CFLAGS', [ '-g' ])
        cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.install_name)
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        cfg.write_config_header('clasp_boehm_d/config.h', remove=False)
    def install(self):
        return self.install_name

class clasp_boehmdc_o(variant):
    install_name = 'clasp-boehmdc-o'
    def configure(self,cfg,env_copy):
        cfg.setenv("clasp_boehmdc_o", env=env_copy.derive())
        cfg.define("USE_BOEHM",1)
        cfg.define("USE_CXX_DYNAMIC_CAST",1)
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3' ])
        cfg.env.append_value('CFLAGS', [ '-O3' ])
        cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.install_name)
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        cfg.write_config_header('clasp_boehmdc_o/config.h', remove=False)
    def install(self):
        return self.install_name

class clasp_boehmdc_d(variant):
    install_name = 'clasp-boehmdc-d'
    def configure(self,cfg,env_copy):
        cfg.setenv("clasp_boehmdc_d", env=env_copy.derive())
        cfg.define("USE_BOEHM",1)
        cfg.define("USE_CXX_DYNAMIC_CAST",1)
        cfg.define("_DEBUG_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-g' ])
        cfg.env.append_value('CFLAGS', [ '-g' ])
        cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.install_name)
        cfg.env.append_value('LINKFLAGS', ['-lgc'])
        cfg.write_config_header('clasp_boehmdc_d/config.h', remove=False)
    def install(self):
        return self.install_name

class clasp_mpsprep_o(variant):
    install_name = 'clasp-mpsprep-o'
    def configure(self,cfg,env_copy):
        cfg.setenv("clasp_mpsprep_o", env=env_copy.derive())
        cfg.define("USE_MPS",1)
        cfg.define("RUNNING_GC_BUILDER",1)
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3' ])
        cfg.env.append_value('CFLAGS', [ '-O3' ])
        cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.install_name)
        cfg.write_config_header('clasp_mpsprep_o/config.h', remove=False)
    def install(self):
        return self.install_name
        
class clasp_mpsprep_d(variant):
    install_name = 'clasp-mpsprep-d'
    def configure(self,cfg,env_copy):
        cfg.setenv("clasp_mpsprep_d", env=env_copy.derive())
        cfg.define("USE_MPS",1)
        cfg.define("RUNNING_GC_BUILDER",1)
        cfg.define("_DEBUG_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-g' ])
        cfg.env.append_value('CFLAGS', [ '-g' ])
        cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.install_name)
        cfg.write_config_header('clasp_mpsprep_d/config.h', remove=False)
    def install(self):
        return self.install_name

class clasp_mps_o(variant):
    install_name = 'clasp-mps-o'
    def configure(self,cfg,env_copy):
        cfg.setenv("clasp_mps_o", env=env_copy.derive())
        cfg.define("USE_MPS",1)
        cfg.define("_RELEASE_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-O3' ])
        cfg.env.append_value('CFLAGS', [ '-O3' ])
        cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.install_name)
        cfg.write_config_header('clasp_mps_o/config.h', remove=False)
    def install(self):
        return self.install_name

class clasp_mps_d(variant):
    install_name = 'clasp-mps-d'
    def configure(self,cfg,env_copy):
        cfg.setenv("clasp_mps_d", env=env_copy.derive())
        cfg.define("USE_MPS",1)
        cfg.define("_DEBUG_BUILD",1)
        cfg.env.append_value('CXXFLAGS', [ '-g' ])
        cfg.env.append_value('CFLAGS', [ '-g' ])
        cfg.env.append_value('LINKFLAGS', '-Wl,-object_path_lto,%s.lto.o' % self.install_name)
        cfg.write_config_header('clasp_mps_d/config.h', remove=False)
    def install(self):
        return self.install_name

def options(opt):
    opt.load('compiler_cxx')
    opt.load('compiler_c')
#    opt.add_option('--gc', default='boehm', help='Garbage collector');


def configure(cfg):
    cfg.check_waf_version(mini='1.7.5')
    cfg.load('compiler_cxx')
    cfg.load('compiler_c')
    if ('program_name' in cfg.__dict__):
        pass
    else:
        cfg.env.append_value('CXXFLAGS', ['-I../../include/clasp/main/'] )
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
    includes = [ '-I../../include/' ]
    cfg.env.append_value('CXXFLAGS', [ '-std=c++11'])
    cfg.env.append_value('CXXFLAGS', includes )
    cfg.env.append_value('CFLAGS', includes )
    cfg.env.append_value('CXXFLAGS', '-flto')
    cfg.env.append_value('CFLAGS', '-flto')
    cfg.env.append_value('CXXFLAGS', os.getenv("CLASP_RELEASE_CXXFLAGS").split() )
    cfg.env.append_value('CXXFLAGS', ['-I/usr/include'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-deprecated-register'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-expansion-to-defined'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-return-type-c-linkage'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-invalid-offsetof'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-#pragma-messages'] )
    cfg.env.append_value('CXXFLAGS', ['-Wno-inconsistent-missing-override'] )
    cfg.env.append_value('LINKFLAGS',"-Wl,-lto_library,%s/libLTO.%s" % (os.getenv("CLASP_RELEASE_LLVM_LIB_DIR"), os.getenv("CLASP_LIB_EXTENSION")))
    cfg.env.append_value('LINKFLAGS', ['-flto'])
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
    cfg.env.append_value('LINKFLAGS', ['-L../../build/clasp/Contents/Resources/lib/common/lib'])
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
    env_copy = cfg.env.derive()
    for v in VARIANTS:
        vi = eval(v+"()")
        vi.configure(cfg,env_copy)


def build(bld):
#    bld(name='myInclude', export_includes=[bld.env.MY_MYSDK, 'include'])
    if not bld.variant:
        bld.fatal('Call waf with build_variant')
    print("In build(bld)")
    if ( '_source_files' in bld.__dict__ ):
        # Don't build anything, just recurse into 'src'
        bld.recurse('src')
    else:
        bld._source_files = []
        bld.recurse('src')
        bld.recurse('src/main')
#        print("recursed source_files = %s" % bld._source_files)
#    bld.program(source=all_files(),target="clasp")
        bld.env = bld.all_envs[bld.variant]
        variant = eval(bld.variant+"()")
        bld.program(source=bld._source_files,target="clasp")
        bld.install_as('${PREFIX}/%s/%s' % (os.getenv("EXECUTABLE_DIR"),variant.install()))

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
#    run_str = 'generate-headers-from-all-sifs src/main/ ${SRC}'
    ext_out = ['.h']
    def run(self):
        cmd = StringIO.StringIO()
        cmd.write('generate-headers-from-all-sifs src/main')
        for f in self.inputs:
            cmd.write(' %s' % f.abspath())
        return self.exec_command(cmd.getvalue())

class link_bitcode(Task.Task):
    ext_out = ['.lbc']
    def run(self):
        cmd = StringIO.StringIO()
        cmd.write('link-llvm ')
        for f in self.inputs:
            cmd.write(' %s' % f.abspath())
        cmd.write(' -o %s' % self.outputs[0])
        return self.exec_command(cmd.getvalue())


# Have all 'cxx' targets have 'include' in their include paths.
@TaskGen.feature('cxx')
@TaskGen.after('process_source')
def preprocess_task_generator(self):
    print("Incoming  self: %s" % self)
    compiled_tasks = self.compiled_tasks
    all_sif_files = []
    all_o_files = []
    for task in self.compiled_tasks:
        if ( task.__class__.__name__ == 'cxx' ):
            for node in task.inputs:
                i_node = node.change_ext('.i')
                sif_node = node.change_ext('.sif')
                o_node = node.change_ext('.o')
                self.create_task('preprocess',node,[i_node])
                self.source.append(i_node)
                self.create_task('sif',i_node,[sif_node])
                all_sif_files.append(sif_node)
                all_o_files.append(o_node)
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
    self.create_task('link_bitcode',all_o_files,'clasp.lbc')


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

