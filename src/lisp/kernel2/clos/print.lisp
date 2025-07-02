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
