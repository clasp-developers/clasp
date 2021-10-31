(in-package #:core)

;;; I don't see any problem with the following code to "inline" write and
;;; write-to-string, but it's broken. During build it causes weird integers
;;; to be printed apparently randomly, and there's an error while building
;;; ASDF.
#+(or)
(progn

(defvar *printer-variables*
  ;; :stream excepted as it's not dynamic.
  '((:escape *print-escape*) (:radix *print-radix*) (:base *print-base*)
    (:circle *print-circle*) (:pretty *print-pretty*) (:level *print-level*)
    (:length *print-length*) (:case *print-case*) (:array *print-array*)
    (:gensym *print-gensym*) (:readably *print-readably*)
    (:right-margin *print-right-margin*) (:miser-width *print-miser-width*)
    (:lines *print-lines*) (:pprint-dispatch *print-pprint-dispatch*)))

;; Given a keyword argument list, return three values:
;; A list of bindings of variables to argument forms
;;  some are printer variables, but any duplicates will get a gensym
;; A variable that can be evaluated to get the stream to write to
;; T iff the arguments were valid.
;;  (invalidity would be due to e.g. non-constant keywords.)
(defun parse-printer-keys (kwargs env streamp)
  (let ((bindings nil)
        (stream '*standard-output*)
        (seen-stream-p nil)
        (info (loop for (key var) in *printer-variables*
                    collect (list key var nil))))
    (loop for (keywordf arg) on kwargs by #'cddr
          for keyword = (if (constantp keywordf env)
                            (ext:constant-form-value keywordf env)
                            ;; variable keyword
                            (return (values nil nil nil)))
          do (case keyword
               ((:allow-other-keys)
                ;; If we have seen :allow-other-keys, we just give up on
                ;; expanding - that's a weird case to deal with.
                (return (values nil nil nil)))
               ((:stream)
                (if streamp
                    (let ((temp (gensym "STREAM")))
                      (when seen-stream-p
                        (setf seen-stream-p t stream temp))
                      (push `(,temp ,arg) bindings))
                    ;; invalid keyword
                    (return (values nil nil nil))))
               (t (let ((i (assoc keyword info)))
                    (cond ((null i) ; invalid keyword
                           (return (values nil nil nil)))
                          ((third i) ; seen already
                           (push `(,(gensym) ,arg) bindings))
                          (t
                           (setf (third i) t) ; mark seen
                           (push `(,(second i) ,arg) bindings))))))
          finally
             (return
               (values (nreverse bindings) stream t)))))

(define-compiler-macro write (&whole form object &rest keys
                                     &key &allow-other-keys
                                     &environment env)
  (multiple-value-bind (bindings stream validp)
      (parse-printer-keys keys env t)
    (if validp
        (let ((osym (gensym "OBJECT")))
          `(let ((,osym ,object) ,@bindings)
             (declare (ignorable ,@(mapcar #'first bindings)))
             (write-object ,osym ,stream)))
        form)))

(define-compiler-macro write-to-string
    (&whole form object &rest keys &key &allow-other-keys &environment env)
  (multiple-value-bind (bindings stream validp)
      (parse-printer-keys keys env nil)
    (declare (ignore stream))
    (if validp
        (let ((osym (gensym "OBJECT")))
          `(let ((,osym ,object) ,@bindings)
             (declare (ignorable ,@(mapcar #'first bindings)))
             (stringify ,osym)))
        form)))

) ; #+(or)

;;; These are more or less inlines. KLUDGE.
(define-compiler-macro prin1 (object &optional (stream '*standard-output*))
  `(let ((*print-escape* t)) (write-object ,object ,stream)))
(define-compiler-macro princ (object &optional (stream '*standard-output*))
  `(let ((*print-escape* nil) (*print-readably* nil))
     (write-object ,object ,stream)))

(define-compiler-macro prin1-to-string (object)
  `(let ((*print-escape* t)) (stringify ,object)))
(define-compiler-macro princ-to-string (object)
  `(let ((*print-escape* nil) (*print-readably* nil)) (stringify ,object)))
