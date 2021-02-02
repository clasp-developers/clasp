(in-package "MP")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun atomic-expander (symbol)
    (core:get-sysprop symbol 'atomic-expander))
  (defun (setf atomic-expander) (expander symbol)
    (core:put-sysprop symbol 'atomic-expander expander)))

(defmacro atomic (place &rest keys &key order &allow-other-keys
                  &environment env)
  (declare (ignore order))
  (multiple-value-bind (temps values old new read write cas)
      (apply #'get-atomic-expansion place :environment env keys)
    (declare (ignore old new write cas))
    `(let* (,@(mapcar #'list temps values)) ,read)))

(define-setf-expander atomic (place &rest keys &key order &allow-other-keys
                              &environment env)
  (declare (ignore order))
  (multiple-value-bind (temps vals old new read write cas)
      (apply #'get-atomic-expansion place :environment env keys)
    (declare (ignore old cas))
    (values temps vals `(,new) write read)))

(defun get-atomic-expansion (place &rest keys
                             &key environment (order nil orderp)
                             &allow-other-keys)
  (declare (ignore order))
  ;; Default the order parameter. KLUDGEy.
  (unless orderp (setf keys (list* :order :sequentially-consistent keys)))
  (etypecase place
    (symbol
     ;; KLUDGE: This will not work in bclasp at all, and the cleavir interface
     ;; may not be great for this.
     #-cclasp
     (multiple-value-bind (expansion expanded)
         (macroexpand-1 place environment)
       (if expanded
           (apply #'get-atomic-expansion expansion keys)
           (error "CAS on variables not supported yet")))
     #+cclasp
     (let ((info (cleavir-env:variable-info environment place)))
       (etypecase info
         (cleavir-env:symbol-macro-info
          (apply #'get-atomic-expansion (macroexpand-1 place environment) keys))
         (cleavir-env:special-variable-info
          (apply #'get-atomic-expansion `(symbol-value ',place) keys))
         (cleavir-env:lexical-variable-info
          ;; TODO
          (error 'operation-not-atomic :place place))
         (null
          (error "Unknown variable ~a" place)))))
    (cons
     (let* ((name (car place))
            (expander (atomic-expander name)))
       (if expander
           (apply expander place keys)
           (multiple-value-bind (expansion expanded)
               (macroexpand-1 place environment)
             (if expanded
                 (apply #'get-atomic-expansion expansion keys)
                 (error 'operation-not-atomic :place place))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-atomic-expander (name place-ll expander-ll body)
    (let ((place (gensym "PLACE")))
      (multiple-value-bind (decls body doc)
          (core:process-declarations body t)
        ;; FIXME: probably have to sort the decls by lambda list (ugh)
        `(lambda (,place ,@expander-ll)
           (declare ,@decls)
           ,@(when doc (list doc))
           (destructuring-bind ,place-ll (rest ,place)
             (block ,name ,@body)))))))

(defmacro define-atomic-expander (accessor
                                  place-lambda-list expander-lambda-list
                                  &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (atomic-expander ',accessor)
           ,(expand-atomic-expander
             accessor place-lambda-list expander-lambda-list body))))

(defmacro define-simple-atomic-expander (name (&rest params)
                                         reader writer casser
                                         &optional documentation)
  (let ((stemps (loop repeat (length params) collect (gensym "TEMP"))))
    `(define-atomic-expander ,name (,@params) (&key order environment)
       (declare (ignore environment))
       ,@(when documentation (list documentation))
       (let ((scmp (gensym "CMP")) (snew (gensym "NEW"))
             ,@(loop for stemp in stemps
                     collect `(,stemp (gensym "TEMP"))))
         (values (list ,@stemps) (list ,@params) scmp snew
                 (list ',reader order ,@stemps)
                 (list ',writer order snew ,@stemps)
                 (list ',casser order scmp snew ,@stemps))))))

;;;

(define-atomic-expander the (type place) (&rest keys)
  (multiple-value-bind (vars vals old new read write cas)
      (apply #'get-atomic-expansion place keys)
    (values vars vals old new
            `(the ,type ,read)
            `(let ((,new (the ,type ,new))) ,write)
            `(let ((,old (the ,type ,old)) (,new (the ,type ,new))) ,cas))))

(define-atomic-expander first (list) (&rest keys)
  (apply #'get-atomic-expansion `(car ,list) keys))
(define-atomic-expander rest (list) (&rest keys)
  (apply #'get-atomic-expansion `(cdr ,list) keys))

(define-simple-atomic-expander car (list)
  core::car-atomic core::rplaca-atomic core:cas-car)
(define-simple-atomic-expander cdr (list)
  core::cdr-atomic core::rplacd-atomic core:cas-cdr)

(define-simple-atomic-expander core:rack-ref (rack index)
  core::atomic-rack-read core::atomic-rack-write core::cas-rack)

#+(or)
(define-simple-atomic-expander symbol-value (symbol)
  core:atomic-symbol-value core:atomic-setf-symbol-value core:cas-symbol-value)

#+(or)
(define-simple-atomic-expander svref (vector index)
  core:atomic-svref core:atomic-svset core:cas-svref)
