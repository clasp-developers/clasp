
#+(or)(define-condition missing () () )

(print (macroexpand '(handler-case (error "foo")
                      (error (format t "Hit error~%")))))

(print (macroexpand '(HANDLER-BIND
       ((ERROR
         #'(LAMBDA (CORE::TEMP)
             (DECLARE (IGNORABLE CORE::TEMP))
             (SETQ #:G1254 CORE::TEMP)
             (GO #:G1255))))
                      (RETURN-FROM #:G1253 (ERROR "foo")))))

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


(defun bar ()
  (handler-case (error "foo")
    (error (format t "Hit error~%"))))

(defun foo ()
;;  (declare (optimize (debug 3)))
  (block x
    (funcall (lambda ()
               (block y
                 (funcall (lambda () (return-from y 1234))))
               (return-from x 5678)))))


#+(or)(defun eh-bab (a)
  (declare (optimize (debug 3)))
  (block x
    (funcall (lambda ()
               (declare (core:lambda-name x))
               (block y
                 (when (eq a 'x)
                   (return-from x 'aa))
                 (funcall (lambda ()
                            (declare (core:lambda-name y))
                            (block z
                              (when (eq a 'y)
                                (return-from x 'bb))
                              (funcall (lambda ()
                                         (declare (core:lambda-name z))
                                         (when (eq a 'z)
                                           (return-from x 'cc))
                                         (funcall (lambda ()
                                                    (declare (core:lambda-name final))
                                                    (when (eq a 'a) (return-from x 'dd))
                                                    (when (eq a 'b) (return-from y) 'ee)
                                                    (when (eq a 'c) (return-from z))))))))))))))

