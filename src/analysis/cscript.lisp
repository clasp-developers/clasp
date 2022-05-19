(print *features*)

(if (member :cando k:*extensions*)
    (k:sources :iclasp #~"clasp_gc_cando.sif")
    (k:sources :iclasp #~"clasp_gc.sif"))