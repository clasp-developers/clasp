(if (member :cando k:*extensions*)
    (k:sources :libclasp #~"clasp_gc_cando.sif")
    (k:sources :libclasp #~"clasp_gc.sif"))
