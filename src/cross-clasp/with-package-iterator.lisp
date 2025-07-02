(in-package #:cross-clasp)

(defun inherited-symbols (package)
  (loop for s being the symbols of package
        for name = (symbol-name s)
        when (eql (nth-value 1 (find-symbol name package)) :inherited)
          collect s))

(defun internal-symbols (package)
  (loop for s being the present-symbols of package
        for name = (symbol-name s)
        when (eql (nth-value 1 (find-symbol name package)) :internal)
          collect s))

(defun external-symbols (package)
  ;; subtlety: to be external the symbol has to be present, because
  ;; export will import inherited symbols before marking them exported
  (loop for s being the external-symbols of package collect s))

(defun present-symbols (package)
  (loop for s being the present-symbols of package collect s))

(defun internal-and-inherited-symbols (package)
  (loop for s being the symbols of package
        for name = (symbol-name s)
        unless (eql (nth-value 1 (find-symbol name package)) :external)
          collect s))

(defun external-and-inherited-symbols (package)
  (loop for s being the symbols of package
        for name = (symbol-name s)
        unless (eql (nth-value 1 (find-symbol name package)) :internal)
          collect s))

(defun all-symbols (package)
  (loop for s being the symbols of package collect s))

(defun coerce-package-designator (designator)
  (etypecase designator
    (package designator)
    ((or character string symbol)
     (or (clostrum:find-package m:*client* *build-rte* (string designator))
       (error 'package-error :package designator)))))

(defun core::packages-iterator (packages symbol-types maybe-list)
  (declare (ignore maybe-list)) ; clasp has this but we don't need it
  (let ((bad-types (set-difference symbol-types '(:internal :external :inherited))))
    (unless (null bad-types)
      (error "Bad symbol-types for ~s: ~s"
             'with-package-iterator bad-types)))
  (let ((packages (etypecase packages
                    (list (mapcar #'coerce-package-designator packages))
                    ((or package character string symbol)
                     (list (coerce-package-designator packages))))))
    (when (or (null packages) (null symbol-types))
      (flet ((iterate () (values nil nil nil nil))) #'iterate))
    (let* ((getter (cond ((equal symbol-types '(:inherited)) #'inherited-symbols)
                         ((equal symbol-types '(:internal)) #'internal-symbols)
                         ((equal symbol-types '(:external)) #'external-symbols)
                         ((or (equal symbol-types '(:internal :external))
                            (equal symbol-types '(:external :internal)))
                          #'present-symbols)
                         ((or (equal symbol-types '(:internal :inherited))
                            (equal symbol-types '(:inherited :internal)))
                          #'internal-and-inherited-symbols)
                         ((or (equal symbol-types '(:external :inherited))
                            (equal symbol-types '(:inherited :external)))
                          #'external-and-inherited-symbols)
                         (t ; already ruled out bad specs, so it must be all 3
                          #'all-symbols)))
           (package (pop packages))
           (symbols (funcall getter package)))
      (flet ((iterate ()
               (tagbody
                again
                  (when (null symbols)
                    (cond ((null packages)
                           (return-from iterate (values nil nil nil nil)))
                          (t (setf package (pop packages)
                                   symbols (funcall getter package))
                             (go again))))
                  (let ((sym (pop symbols)))
                    (return-from iterate
                      (values t sym
                              (nth-value 1 (find-symbol (symbol-name sym) package))
                              package))))))
        #'iterate))))

(defmacro %with-package-iterator ((iterator package-list &rest symbol-types)
                                  &body body)
  (let ((ithunk (gensym "ITERATOR-THUNK")))
    `(let ((,ithunk (core::packages-iterator ,package-list ',symbol-types t)))
       (macrolet ((,iterator () (list 'funcall ',ithunk))) ,@body))))
