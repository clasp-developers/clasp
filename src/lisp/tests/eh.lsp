
(defun foo ()
  (block x
    (funcall (lambda () (return-from x nil)))))

(defun baz ()
  (BLOCK top
    (LET ((var NIL))
      (DECLARE (IGNORABLE var))
      (TAGBODY
         (LET ((CORE::*HANDLER-CLUSTERS* 
                 (CONS
                  (LIST
                   (CONS 'ERROR
                         #'(LAMBDA (CORE::TEMP)
                             (DECLARE (IGNORABLE CORE::TEMP) (core:lambda-name baz-inner))
                             (SETQ var CORE::TEMP)
                             (GO bottom))))
                  CORE::*HANDLER-CLUSTERS*)))
           (RETURN-FROM top (ERROR "foo")))
         bottom
         (RETURN-FROM top
           (LET ((FORMAT var))))))))


