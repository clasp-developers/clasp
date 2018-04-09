import sys, logging, os
from waflib import Logs, Task
import waflib.Options

try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

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

def ensure_list(x):
    if isinstance(x, list):
        return x
    else:
        return [x]

def collect_files(root, suffix = []):
    result = []
    suffixes = ensure_list(suffix)
    for root, dirs, files in os.walk(root):
        for name in files:
            for suffix in suffixes:
                if name[ - len(suffix) :] == suffix:
                    result.append(os.path.join(root, name))
                    break
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
