(defconstant *hir-commands*
  '(("HIR commands"
     ((:c :continue) hir-continue nil
      ":c(ontinue) Continue processing forms"
      "Stuff"))))

(define-condition continue-hir (condition) ())

(defun hir-continue ()
  (format t "About to signal continue-hir~%")
  (signal 'continue-hir))


(defun ttpl ()
  (format t "Starting tpl~%")
  (handler-case (core:tpl :commands *hir-commands*)
    (continue-hir (x) nil))
  (format t "Done tpl~%"))
