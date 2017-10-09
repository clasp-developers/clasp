;;; CL functions that call "explicit" functions, intended to be relinked
;;; The directly environment-dependent parts are the coerce-s and previous group

;;; Should compile in any environment with the explicit: known, and CL standard functions, though.

(defun funcall (fdesignator &rest arguments)
  (explicit:apply (coerce-fdesignator fdesignator) arguments))

(define-compiler-macro funcall (fdesignator &rest arguments)
  `(explicit:funcall (coerce-fdesignator ,fdesignator) ,@arguments))

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
  (defmapfoo mapc) (defmapfoo mapcar) (defmapfoo mapcan)
  (defmapfoo mapl) (defmapfoo maplist) (defmapfoo mapcon))

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

;;; Designator resolution

(defun coerce-fdesignator (fdesignator)
  (etypecase fdesignator
    (symbol (symbol-function fdesignator))
    (function fdesignator)))

(define-compiler-macro coerce-fdesignator (&whole whole fdesignator)
  (if (and (consp fdesignator)
           (consp (cdr fdesignator))
           (null (cddr fdesignator)))
      (cond ((eq (car fdesignator) 'quote)
             `(symbol-function ,fdesignator))
            ((eq (car fdesignator) 'function)
             fdesignator)
            (t whole))
      whole))

(defun coerce-string-designator (sdesignator)
  (etypecase sdesignator
    (string sdesignator)
    (symbol (symbol-name sdesignator))
    (character (make-string 1 :initial-element sdesignator))))

(defun coerce-package-designator (package-designator)
  (if (packagep package-designator)
      package-designator
      (find-package (coerce-string-designator package-designator))))

(defun coerce-list-designator (list-designator)
  (typecase list-designator
    (null list-designator)
    (cons list-designator)
    (t (list list-designator))))

;;; CLHS 9.1.2.1
(defun coerce-to-condition (simple-type datum &rest arguments)
  (etypecase datum
    (symbol
     (apply #'make-condition datum arguments))
    ((or string function) ; format control
     (make-condition simple-type
                     :format-control datum
                     :format-arguments arguments))
    (condition
     (assert (null arguments))
     datum)))
