(cl:in-package #:clasp-cleavir)

(eval-when (:compile-toplevel)
  (ensure-unicode-table-loaded))

(setq *unicode-file-read* #.*unicode-file-read*)
(setq *additional-clasp-character-names* #.*additional-clasp-character-names*)
(setq *mapping-char-code-to-char-names* #.*mapping-char-code-to-char-names*)

#|
(time
 (load (compile-file "sys:kernel;cleavir;define-unicode-tables.lisp")))
|#
