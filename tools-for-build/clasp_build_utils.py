import sys, logging
from waflib import Logs

# A simple wrapper that can redirect all log levels into a file.
class clasp_logger():
    logger_name = 'clasp-build-log'

    def __init__(self, log_file):
        if log_file:
            logger = logging.getLogger(self.logger_name)
            if sys.hexversion > 0x3000000:
                encoding = sys.stdout.encoding
            else:
                encoding = None
            hdlr = logging.FileHandler(log_file, 'a', encoding = encoding)
            formatter = logging.Formatter('%(asctime)-15s %(message)s')
            hdlr.setFormatter(formatter)
            logger.addHandler(hdlr)
            logger.setLevel(logging.DEBUG)
            self.permanent_logger = logger
        else:
            # An empty logger that does nothing. Will be reinitialized.
            self.permanent_logger = logging.getLogger(self.logger_name)

    def debug(self, *k, **kw):
        Logs.debug(*k, **kw)
        self.permanent_logger.debug(*k, **kw)

    def info(self, *k, **kw):
        Logs.info(*k, **kw)
        self.permanent_logger.info(*k, **kw)

    def warn(self, *k, **kw):
        Logs.warn(*k, **kw)
        self.permanent_logger.warn(*k, **kw)

    def pprint(self, color, *k, **kw):
        Logs.pprint(color, *k, **kw)
        self.permanent_logger.info(*k, **kw)

# log will be re-initialized in build() to append the build debug log into build/variant/build.log.
# Until then it will only log to the console.
global log
log = clasp_logger(None)
