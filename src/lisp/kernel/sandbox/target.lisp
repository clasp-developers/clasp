;;; CL functions that call "explicit" functions, intended to be relinked
;;; The directly environment-dependent parts are the coerce-s and previous group

;;; Should compile in any environment with the explicit: known, and CL standard functions, though.

(defun funcall (fdesignator &rest arguments)
  (explicit:apply (coerce:function-designator fdesignator) arguments))

(define-compiler-macro funcall (fdesignator &rest arguments)
  `(explicit:funcall (coerce:function-designator ,fdesignator) ,@arguments))

(defun apply (fdesignator &rest spreadable-arguments)
  ;; Weird but true, since it's not just a list of arguments, it's a list where the
  ;; last element is a list (a spreadable argument list)
  (explicit:apply #'explicit:apply fdesignator spreadable-arguments))

(define-compiler-macro apply (fdesignator &rest spreadable-arguments)
  `(explicit:apply (coerce:function-designator ,fdesignator) ,@spreadable-arguments))

#+(or)
(defun export (symbols &optional package)
  (let ((symbols (mapcar #'coerce-symbol-designator (coerce-list-designator symbols)))
        (package (coerce-package-designator package)))
    (explicit:export symbols package)
    t))

(defun member (item list &key (test nil testp) (test-not nil test-not-p) (key #'identity))
  (let ((key (coerce-fdesignator key)))
    (if testp
        (if test-not-p
            (error "Both :test and :test-not specified")
            (explicit:member item list (coerce-fdesignator test) key))
        (if test-not-p
            (explicit:member-not item list (coerce-fdesignator test-not) key)
            (explicit:member item list #'eql key)))))

(defun member-if (test list &key (key #'identity))
  (explicit:member-if (coerce-fdesignator test) list (coerce-fdesignator key)))
(defun member-if-not (test list &key (key #'identity))
  (explicit:member-if-not (coerce-fdesignator test) list (coerce-fdesignator key))))

(defun assoc (item alist &key (test nil testp) (test-not nil test-not-p) (key #'identity))
  (let ((key (coerce-fdesignator key)))
    (if testp
        (if test-not-p
            (error "Both :test and :test-not specified")
            (explicit:assoc item list (coerce-fdesignator test) key))
        (if test-not-p
            (explicit:assoc-not item list (coerce-fdesignator test-not) key)
            (explicit:assoc item list #'eql key)))))

(defun assoc-if (test list &key (key #'identity))
  (explicit:assoc-if (coerce-fdesignator test) list (coerce-fdesignator key)))
(defun assoc-if-not (test list &key (key #'identity))
  (explicit:assoc-if-not (coerce-fdesignator test) list (coerce-fdesignator key)))

(macrolet ((defmapfoo (name)
             `(progn
                (defun ,name (fdesignator &rest lists)
                  (apply #',name (coerce-fdesignator fdesignator) lists))
                (define-compiler-macro ,name (fdesignator &rest lists)
                  (list* ,name (list 'coerce-fdesignator fdesignator) lists)))))
  (defmapfoo mapc) (defmapfoo mapcar)  (defmapfoo mapcan)
  (defmapfoo mapl) (defmapfoo maplist) (defmapfoo mapcon))

#+(or)
(progn
;;; whoa! not a generic function!
(defun make-instance (class &rest initargs &key &allow-other-keys)
  (apply #'explicit:make-instance
         (coerce:class-designator class)
         initargs))

(define-compiler-macro make-instance (class &rest initargs &key &allow-other-keys)
  `(explicit:make-instance (coerce:class-designator ,class) ,@initargs))
)

;; FIXME: break on signals, warning output stream, etc
#+(or)
(macrolet ((defsignaler (name default)
             `(progn
                (defun ,name (datum &rest arguments)
                  (,name (apply #'coerce-to-condition ',default datum arguments)))
                (define-compiler-macro ,name (datum &rest arguments)
                  (list ,name (list* 'coerce-to-condition ',default datum arguments))))))
  (defsignaler signal simple-condition)
  (defsignaler warn simple-warning)
  (defsignaler error simple-error)
  (defsignaler cerror simple-error))
