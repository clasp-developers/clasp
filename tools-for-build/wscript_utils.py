from waflib.Tools import cxx
import tempfile
import sys, logging, os, subprocess
from waflib import Logs, Task, TaskGen
import waflib.Options
from waflib import Utils

try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

DARWIN_OS = 'darwin'
LINUX_OS = 'linux'
FREEBSD_OS = 'freebsd'

#
# run_program
#
def run_program(binary, *args):
    log.debug("run_program for %s %s", binary, args)
    proc = subprocess.Popen([binary] + list(args), stdout = subprocess.PIPE, shell = False, universal_newlines = True)
    (stdout, err) = proc.communicate()
    return stdout

def run_program_echo(binary, *args):
    log.debug("run_program_echo for %s %s", binary, args)
    cmd = "%s %s" % (binary, " ".join('"{0}"'.format(w.replace('"',r'\"')) for w in args))
    print(" cmd = %s" % cmd )
    subprocess.call(["/bin/sh","-c",cmd])
    # proc = subprocess.Popen([binary] + list(args), shell = False, universal_newlines = True)

def get_git_commit(cfg):
    return run_program(cfg.env.GIT_BINARY, "rev-parse", "--short", "HEAD").strip()

def get_clasp_version(cfg):
    if (cfg.env.CLASP_VERSION):
        return cfg.env.CLASP_VERSION
    return run_program(cfg.env.GIT_BINARY, "describe", "--always").strip()

def run_llvm_config(cfg, *args):
    log.debug("run_llvm_config LLVM_CONFIG_BINARY = %s", cfg.env.LLVM_CONFIG_BINARY)
    result = run_program(cfg.env.LLVM_CONFIG_BINARY, *args)
    assert len(result) > 0
    return result.strip()

def run_llvm_config_for_libs(cfg, *args):
    log.debug("run_llvm_config_for_libs LLVM_CONFIG_BINARY_FOR_LIBS = %s", cfg.env.LLVM_CONFIG_BINARY_FOR_LIBS)
    result = run_program(cfg.env.LLVM_CONFIG_BINARY_FOR_LIBS, *args)
    assert len(result) > 0
    return result.strip()

def maybe_dump_command(cmd, kind = ''):
    # print a copy-paste'able command line
    if isinstance(cmd, list):
        cmdstr = StringIO()
        first = True
        for x in cmd:
            if first:
                first = False
            else:
                cmdstr.write(" \\\n")
            cmdstr.write(x)
        cmdstr = cmdstr.getvalue()
    else:
        cmdstr = cmd
    if waflib.Options.options.PRINT_EXTERNAL_COMMANDS:
        log.pprint('RED', 'command line for %s:' % kind)
        log.pprint('YELLOW', cmdstr)
    else:
        log.debug("command line for %s:\n%s", kind, cmdstr)

#
# macOS specfic tools
#
def macosx_sdk_path(cfg):
    result = ""
    dest_os = cfg.env["DEST_OS"]
    if ( dest_os == DARWIN_OS ):
        result = run_program('xcrun', '--show-sdk-path')
        assert len(result) > 0
        result = result.strip()
    log.debug("macosx_sdk_path: %s", result)
    return result

def get_macosx_version(cfg):
    result = [0]
    dest_os = cfg.env["DEST_OS"]
    if ( dest_os == DARWIN_OS ):
        result = run_program("sw_vers","-productVersion");
        assert len(result) > 0
        result = result.strip().split(".")
        result = [int(result[0]),int(result[1]),int(result[2])]
    log.debug("macosx productVersion: %s", result)
    return result

#
# logging
#
global log

# We need a wrapper because of the way python globals and modules work.
class clasp_logger():
    logger = None

    def __init__(self):
        self.reinitialize()

    def reinitialize(self, console_level = logging.INFO, log_file = None):
        def make_console_log_handler():
            handler = logging.StreamHandler()
            handler.setLevel(console_level)
            handler.setFormatter(logging.Formatter('%(message)s'))
            return handler

        def make_permanent_log_handler():
            encoding = sys.stdout.encoding if sys.hexversion > 0x3000000 else None
            handler = logging.FileHandler(log_file, 'a', encoding = encoding)
            handler.setFormatter(logging.Formatter('%(asctime)-15s %(levelname)-5s %(message)s'))
            handler.setLevel(logging.DEBUG)
            return handler

        logger = logging.getLogger("clasp-build-logger")
        logger.setLevel(logging.DEBUG)
        logger.handlers = []
        logger.addHandler(make_console_log_handler())
        if log_file:
            logger.addHandler(make_permanent_log_handler())

        self.logger = logger
        self.logger.info('reinitialized logger')

    def debug(self, *k, **kw):
        self.logger.debug(*k, **kw)

    def info(self, *k, **kw):
        self.logger.info(*k, **kw)

    def warn(self, *k, **kw):
        self.logger.warn(*k, **kw)

    def pprint(self, color, *k, **kw):
        Logs.pprint(color, *k, **kw)
        self.logger.debug(*k, **kw)

log = clasp_logger()

#
# tasks
#
class clasp_task(Task.Task):
    waf_print_keyword = None

    def keyword(self):
        if self.waf_print_keyword:
            return self.waf_print_keyword
        else:
            return type(self).__name__

    def exec_command(self, cmd, **kw):
        maybe_dump_command(cmd, type(self).__name__)
        kw['stdout'] = sys.stdout        # to redirect the stdout of the spawned executable to ours
        return super(clasp_task, self).exec_command(cmd, **kw)

    def clasp_command_line(self, clasp_exe_path, *args, **kwargs):
        assert os.path.isfile(clasp_exe_path)
        # NOTE: these wouldn't work as defaulting **kwargs, because it's mixed with *args
        image = kwargs.get("image")
        features = kwargs.get("features")
        forms = kwargs.get("forms")
        resource_dir = kwargs.get("resource_dir")

        cmd = [ clasp_exe_path,
                "--norc",
                "--type", "image",
                "--disable-mpi",
        ]

        if (self.env.THREAD_SANITIZER):
            cmd = cmd + [ "-f", "sanitize=thread" ]

        if (self.env.ADDRESS_SANITIZER):
            cmd = cmd + [ "-f", "sanitize=address" ]

        if (self.env.MEMORY_SANITIZER):
            cmd = cmd + [ "-f", "sanitize=memory" ]

        if resource_dir:
            cmd = cmd + [ "--resource-dir", resource_dir ]

        if image == False:
            cmd = cmd + [ "--ignore-image" ]
        elif image:
            cmd = cmd + [ "--image", image ]

        if (self.bld.options.DEBUG_WHILE_BUILDING):
            features = features + [ "exit-backtrace",
                                    "pause-pid",
                                    "jit-log-symbols",
                                    "debug-run-clang",
            ]

        for feature in features:
            cmd = cmd + [ "--feature", feature ]

        for form in forms:
            cmd = cmd + [ "--eval", form ]

        cmd = cmd + ["--"] + list(args)

        return cmd

#
# lists and collecting files
#
def ensure_list(x):
    if isinstance(x, list):
        return x
    else:
        return [x]

def list_chunks_of_size(lst, chunk_size):
    '''Yield successive n-sized chunks from l.'''
    for i in range(0, len(lst), chunk_size):
        yield lst[i:i + chunk_size]

def split_list(lst, n):
    '''Split the list into n chunks'''
    L = len(lst)
    assert 0 < n <= L
    s = L // n
    return [lst[p : p + s] for p in range(0, L, s)]

def ends_with(x, postfix):
    return x[:len(postfix)] == postfix

def waf_nodes_to_paths(lst):
    return [i.abspath() for i in lst]

def prefix_list_elements_with(lst, prefix):
    return [ '' + prefix + i for i in lst]

def collect_files(root, suffix = []):
    result = []
    suffixes = ensure_list(suffix)
    for root, dirs, files in os.walk(root):
        for name in files:
            collect = len(suffix) == 0
            for suffix in suffixes:
                if name[ - len(suffix) :] == suffix:
                    collect = True
                    break
            if collect:
                result.append(os.path.join(root, name))
    return result

def collect_waf_nodes(bld, root, suffix = []):
    result = []
    for path in collect_files(root, suffix = suffix):
        node = bld.path.find_node(path)
        if (node):
            result.append(node)
        else:
            log.warn("waf's find_node() returned None for path: %s", path)
    return result

def dsym_waf_nodes(name, path):
    info_plist = path.find_or_declare("Contents/Info.plist")
    dwarf_file = path.find_or_declare("Contents/Resources/DWARF/%s" % name)
    log.debug("dsym_output_files, info_plist = %s, dwarf_file = %s", info_plist, dwarf_file)
    return [info_plist, dwarf_file]

def waf_nodes_for_lisp_files(bld, paths):
    nodes = []
    for path in paths:
        if path[:4] == "src/":
            waf_node = bld.path.find_resource("%s.lsp" % path)
            if (waf_node == None):
                waf_node = bld.path.find_resource("%s.lisp" % path)
            #log.debug("Looking for lisp file with .lsp or .lisp: %s --> %s", path, waf_node)
        else: # generated files
            waf_node = bld.path.find_or_declare("%s.lisp" % path)
            #log.debug("Looking for generated lisp file with .lisp: %s --> %s", path, waf_node)
        assert waf_node != None, "Could not find waf node for lisp file %s - did you run './waf update_dependencies'?" % path
        nodes.append(waf_node)
    return nodes


def waf_nodes_for_object_files(bld, paths, fasl_dir):
    nodes = []
    extension = ""
    if (bld.env.CLASP_BUILD_MODE=="object"):
        extension = "o"
    elif (bld.env.CLASP_BUILD_MODE=="faso"):
        extension = "faso"
    elif (bld.env.CLASP_BUILD_MODE=="fasobc"):
        extension = "fasobc"
    elif (bld.env.CLASP_BUILD_MODE=="fasoll"):
        extension = "fasoll"
    elif (bld.env.CLASP_BUILD_MODE=="bitcode"):
        if (bld.use_human_readable_bitcode):
            extension = "ll"
        else:
            extension = "bc"
    for path in paths:
        waf_node = bld.path.find_or_declare("%s/%s.%s" % (fasl_dir,path,extension))
        nodes.append(waf_node)
    return nodes

def waf_nodes_for_faso_files(bld, paths, fasl_dir):
    nodes = []
    for path in paths:
        waf_node = bld.path.find_or_declare("%s/%s.faso" % (fasl_dir,path))
        nodes.append(waf_node)
    return nodes

def libraries_as_link_flags(fmt, libs):
    result = []
    for x in libs:
        result.append(fmt % "")
        result.append(x)
    return result

def libraries_as_link_flags_as_string(fmt, libs):
    result = StringIO()
    for x in libs:
        result.write(" ")
        result.write(fmt % x)
    return result.getvalue()


#@TaskGen.extension(".snapshot")
#def process(self, node):
#    tsk = self.create_task('link_snapshot')
#    print(tsk.__class__)
    
class link_snapshot(clasp_task):
    def run(self):
        executable = self.inputs[0].abspath()
        output_file = self.outputs[0].abspath()
        image_file = self.inputs[1].abspath()
        cmd = self.clasp_command_line(executable,
                                      image = image_file,
                                      features = [],
                                      forms = [ '(gctools:save-lisp-and-die "%s")' % output_file,
                                                '(core:exit)'])
        log.info("link_snapshot = %s\n", cmd)
        return self.exec_command(cmd)
    

class symlink_executable(clasp_task):
    def run(self):
        cmd = [ 'ln', '-s', '-f',
                self.inputs[0].abspath(),
                self.outputs[0].abspath()
        ];
        return self.exec_command(cmd)


class embed_command_line_cxxprogram(cxx.cxxprogram):
    def exec_command(self,cmd,**kw):
        link_filename = "%s/link_command_%s" % (tempfile.gettempdir(), os.getpid())
        object_filename = "%s.o" % link_filename
        print("Writing link command to: %s" % link_filename)
        text_file = open(link_filename, "w")
        for line in cmd:
            text_file.write(line)
            text_file.write('\n')
        text_file.close()
        if (self.bld.env["DEST_OS"] == LINUX_OS ):
            cmd2 = [ 'objcopy', '--input', 'binary', '--output', 'elf64-x86-64', '--binary-architecture', 'i386', link_filename, object_filename ]
            super(embed_command_line_cxxprogram,self).exec_command(cmd2)
            cmd = cmd + [ object_filename ]
        log.info("Caught exec_command cmd = %s" % cmd)
        return super(embed_command_line_cxxprogram,self).exec_command(cmd,**kw)


def embed_snapshot(bld,snapshot_file,input_executable,output_executable,install_name):
    log.info("dtarget -> %s" % output_executable )
    log.info("bld.env[DEST_OS] = %s" % bld.env["DEST_OS"] )
    if (bld.env["DEST_OS"] == DARWIN_OS):
        log.info("dtarget -> %s" % output_executable )
        env2 = bld.env.derive()
        env2.append_value("LINKFLAGS",["-sectcreate", "__CLASP", "__clasp", snapshot_file.abspath()])
        link2 = embed_command_line_cxxprogram(env=env2)
        link2.name = "final_build"
        link2.set_inputs( bld.iclasp_link_task.inputs) # snapshot_file
        link2.set_outputs( [ output_executable ] )
        log.info("final_build -> %s" % link2)
        bld.add_to_group(link2)
    else:
        snapshot_object_file = bld.path.find_or_declare("generated/%s_snapshot.o" % install_name)
        log.info("snapshot_object_file = %s" % snapshot_object_file.abspath() )
        task = linux_snapshot_to_object(env=bld.env)
        task.set_inputs( [snapshot_file])
        task.set_outputs( [snapshot_object_file] )
        bld.add_to_group(task)
        link2 = embed_command_line_cxxprogram(env=bld.env)
        link2.name = "final_build"
        link2.set_inputs( bld.iclasp_link_task.inputs + [snapshot_object_file] ) # snapshot_file
        link2.set_outputs( [ output_executable ] )
        bld.add_to_group(link2)
    bld.install_as('${PREFIX}/bin/%s'%install_name,output_executable,chmod=Utils.O755)
    

class linux_snapshot_to_object(waflib.Task.Task):
    def run(self):
        cmd = [ 'objcopy', '--input', 'binary', '--output', 'elf64-x86-64', '--binary-architecture', 'i386', self.inputs[0].bldpath(), self.outputs[0].bldpath() ]
        log.info("linux_snapshot_to_object cmd = %s" % cmd )
        return self.exec_command(cmd)
    
def fetch_git_revision(path, url, revision = "", label = "master"):
    log.info("Git repository %s  url: %s\n     revision: %s  label: %s\n" % (path, url, revision, label))
    ret = os.system("./tools-for-build/fetch-git-revision.sh '%s' '%s' '%s' '%s'" % (path, url, revision, label))
    if ( ret != 0 ):
        raise Exception("Failed to fetch git url %s" % url)

