(k:includes #~"bdwgc/"
            #~"bdwgc/include/"
            #~"libatomic_ops/src/")

(k:recurse #P"analysis/"
           #P"gctools/"
           #P"clbind/"
           #P"serveEvent/"
           #P"sockets/"
           #P"llvmo/"
           #P"mpip/"
           #P"asttooling/"
           #P"main/"
           #P"core/"
           #P"lisp/")

(k:sources :scraper
           #@"clasp_gc.cc"
           ;#@"cl-wrappers.lisp"
           #@"c-wrappers.h"
           #@"enum_inc.h"
           #@"expose_inc.h"
           #@"header_includes_inc.h"
           #@"init_classes_inc.h"
           #@"init_functions_inc.h"
           #@"initializers_inc.h"
           #@"pre_gc_startup_inc.h"
           #@"rename-methods.sexp"
           #@"source_info_inc.h"
           #@"symbols_scraped_inc.h"
           #@"terminators_inc.h")

(k:sources :install-code
           #~"bdwgc/include/"
           #~"schubfach/")
