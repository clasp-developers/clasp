(k:includes #~"bdwgc/"
            #~"bdwgc/include/"
            #~"libatomic_ops/src/")

(k:recurse #P"gctools/"
           #P"clbind/"
           #P"serveEvent/"
           #P"sockets/"
           #P"llvmo/"
           #P"mpip/"
           #P"asttooling/"
           #P"main/"
           #P"core/"
           #P"lisp/")

(k:sources :iclasp
           #~"clasp_gc.sif")
