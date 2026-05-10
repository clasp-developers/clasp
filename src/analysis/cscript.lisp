;; Feed the base .sif to libclasp. The libclasp emitter (src/koga/ninja.lisp)
;; folds each enabled extension's clasp_gc.dif onto the base before
;; generate-headers consumes the combined .sif. If a .dif is missing, ninja
;; will error at build time naming the missing file.
(k:sources :libclasp #~"clasp_gc.sif")
