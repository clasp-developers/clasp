import sys, logging, os, subprocess
from waflib import Logs, Task
import waflib.Options

try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

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
    proc = subprocess.Popen([binary] + list(args), shell = False, universal_newlines = True)

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
        ]

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
        assert waf_node != None, "Could not find waf node for lisp file %s - did you run './waf update_dependencies'?" % file_name
        nodes.append(waf_node)
    return nodes


def waf_nodes_for_object_files(bld, paths, fasl_dir):
    nodes = []
    for path in paths:
        waf_node = bld.path.find_or_declare("%s/%s.o" % (fasl_dir,path))
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
