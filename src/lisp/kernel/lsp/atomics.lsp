(in-package "MP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFINE-ATOMIC-EXPANSION, GET-ATOMIC-EXPANSION
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun atomic-expander (symbol)
    (core:get-sysprop symbol 'atomic-expander))
  (defun (setf atomic-expander) (expander symbol)
    (core:put-sysprop symbol 'atomic-expander expander)))

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
           (error "Atomic operations on lexical variables not supported yet")))
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
                 (list 'progn (list ',writer order snew ,@stemps) snew)
                 (list ',casser order scmp snew ,@stemps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ATOMIC itself
;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived operators
;;;

(defmacro atomic-update (place update-fn &rest arguments &environment env)
  (multiple-value-bind (vars vals old new cas read)
      (get-cas-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
            (,old ,read))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
             until (eq ,old (setf ,old ,cas))
             finally (return ,new)))))

(defmacro atomic-incf (place &optional (delta 1))
  `(atomic-update ,place #'+ ,delta))

(defmacro atomic-decf (place &optional (delta 1))
  `(atomic-update ,place #'(lambda (y x) (- x y)) ,delta))

(defmacro atomic-push (item place &environment env)
  (multiple-value-bind (vars vals old new cas read)
      (get-cas-expansion place env)
    (let ((gitem (gensym "ITEM")))
      `(let* ((,gitem ,item) ; evaluate left-to-right (CLHS 5.1.1.1)
              ,@(mapcar #'list vars vals)
              (,old ,read)
              (,new (cons ,item ,old)))
         (loop until (eq ,old (setf ,old ,cas))
               do (setf (cdr ,new) ,old)
               finally (return ,new))))))

(defmacro atomic-pop (place &environment env)
  (multiple-value-bind (vars vals old new cas read)
      (get-cas-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
            (,old ,read))
       (loop (let ((,new (cdr ,old)))
               (when (eq ,old (setf ,old ,cas))
                 (return (car ,old))))))))

(defmacro atomic-pushnew (item place &rest keys &key key test test-not
                          &environment env)
  (declare (ignore key test test-not))
  (multiple-value-bind (vars vals old new cas read)
      (get-cas-expansion place env)
    (let ((gitem (gensym "ITEM")) (bname (gensym "ATOMIC-PUSHNEW"))
          gkeybinds gkeys)
      ;; Ensuring CLHS 5.1.1.1 evaluation order is weird here. We'd like to
      ;; only evaluate the keys one time, but we want the adjoin to get
      ;; constant keywords the compiler transformations can work with.
      (loop for thing in keys
            if (constantp thing env)
              do (push (ext:constant-form-value thing env) gkeys)
            else
              do (let ((gkey (gensym "K")))
                   (push gkey gkeys)
                   (push `(,gkey ,thing) gkeybinds))
            finally (setf gkeys (nreverse gkeys)
                          gkeybinds (nreverse gkeybinds)))
      ;; Actual expansion
      `(let* ((,gitem ,item)
              ,@(mapcar #'list vars vals)
              ,@gkeybinds
              (,old ,read))
         (loop named ,bname
               for ,new = (adjoin ,gitem ,old ,@gkeys)
               until (eq ,old (setf ,old ,cas))
               finally (return-from ,bname ,new))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Particular atomic expanders
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
  core::car-atomic core::rplaca-atomic core::cas-car)
(define-simple-atomic-expander cdr (list)
  core::cdr-atomic core::rplacd-atomic core::cas-cdr)

(define-simple-atomic-expander core:rack-ref (rack index)
  core::atomic-rack-read core::atomic-rack-write core::cas-rack)

;; Ignores order specification for the moment.
(define-atomic-expander symbol-value (symbol) (&key order environment)
  (declare (ignore order environment))
  (let ((gs (gensym "SYMBOL")) (cmp (gensym "CMP")) (new (gensym "NEW")))
    (values (list gs) (list symbol) cmp new
            `(core:atomic-symbol-value ,gs)
            `(progn (core:atomic-set-symbol-value ,new ,gs) ,new)
            `(core:cas-symbol-value ,cmp ,new ,gs))))

#+(or)
(define-simple-atomic-expander svref (vector index)
  core:atomic-svref core:atomic-svset core:cas-svref)
