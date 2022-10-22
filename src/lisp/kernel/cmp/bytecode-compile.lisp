
(in-package :cmp)

(defun make-lexical-environment (parent &key (vars (cmp:lexenv/vars parent))
                                          (tags (cmp:lexenv/tags parent))
                                          (blocks (cmp:lexenv/blocks parent))
                                          (frame-end (cmp:lexenv/frame-end parent))
                                          (funs (cmp:lexenv/funs parent))
                                          (notinlines (cmp:lexenv/notinlines parent)))
  (cmp:lexenv/make vars tags blocks funs notinlines frame-end))
