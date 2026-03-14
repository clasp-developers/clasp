(in-package #:clos)

(defgeneric print-object (object stream))

(defmethod print-object (object stream)
  (core::write-ugly-object object stream))

(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  ;; this function is defined later in iolib.lisp.
  `(core::%print-unreadable-object ,object ,stream ,type ,identity
                                   ,(when body
                                      `(lambda () ,@body))))

(defmethod print-object ((class class) stream)
  (print-unreadable-object (class stream :type t)
    (write (class-name class) :stream stream))
  class)

(defmethod print-object ((gf standard-generic-function) stream)
  (print-unreadable-object (gf stream :type t)
    (write (generic-function-name gf) :stream stream))
  gf)

(defun %print-method (m stream)
  (print-unreadable-object (m stream :type t)
    ;; FIXME: Just use FORMAT
    (princ (let ((gf (method-generic-function m)))
             (if gf
                 (generic-function-name gf)
                 'UNNAMED))
           stream)
    (loop for q in (method-qualifiers m)
          do (write-char #\Space stream)
             (prin1 q stream))
    (write-char #\Space stream)
    (prin1 (loop for spec in (method-specializers m)
                 collect (typecase spec
                           (class (class-name spec))
                           (eql-specializer
                            `(eql ,(eql-specializer-object spec)))
                           (t spec)))
           stream)))

(defmethod print-object ((m standard-method) stream)
  (print-unreadable-object (m stream :type t)
    ;; FIXME: Just use FORMAT
    (princ (let ((gf (method-generic-function m)))
             (if gf
                 (generic-function-name gf)
                 'UNNAMED))
           stream)
    (loop for q in (method-qualifiers m)
          do (write-char #\Space stream)
             (prin1 q stream))
    (write-char #\Space stream)
    (prin1 (loop for spec in (method-specializers m)
                 collect (typecase spec
                           (class (class-name spec))
                           (eql-specializer
                            `(eql ,(eql-specializer-object spec)))
                           (t spec)))
           stream))
  m)

(defmethod print-object ((s slot-definition) stream)
  (print-unreadable-object (s stream :type t)
    (write (slot-definition-name s) :stream stream))
  s)

(defmethod print-object ((mc method-combination) stream)
  (print-unreadable-object (mc stream :type t)
    (write (method-combination-name mc) :stream stream))
  mc)

(defmethod print-object ((es eql-specializer) stream)
  (print-unreadable-object (es stream :type t)
    (write (eql-specializer-object es) :stream stream))
  es)

(defmethod print-object ((object standard-object) stream)
  (print-unreadable-object (object stream :type t :identity t))
  object)

(defmethod print-object ((obj structure-object) stream)
  (let* ((class (class-of obj))
	 (slotds (class-slots class)))
    (when (and ;; to fix ansi-tests PRINT-LEVEL.8 & PRINT-LEVEL.9
           ;; printing a struct w/o slots
           ;; don't test for slotds
           ;; *p-readably* effectively disables *p-level*
           (not *print-readably*)
           *print-level*
           (zerop *print-level*))
      (write-string "#" stream)
      (return-from print-object obj))
    (write-string "#S(" stream)
    (prin1 (class-name class) stream)
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i))
	 (limit (or *print-length* most-positive-fixnum))
	 (sv))
	((null scan))
      (declare (fixnum i))
      (when (>= i limit)
	(write-string " ..." stream)
	(return))
      (setq sv (standard-instance-access obj i))
      (unless (eq sv (core:unbound))
        ;; fix bug where symbols like :FOO::BAR are printed
        (write-string " " stream)
        (let ((kw (intern (symbol-name (slot-definition-name (car scan)))
                          (load-time-value (find-package "KEYWORD")))))
          (prin1 kw stream))
        (write-string " " stream)
        (if *print-level*
            (let ((*print-level* (1- *print-level*)))
              (prin1 sv stream))
            (prin1 sv stream))))
    (write-string ")" stream)
    obj))
