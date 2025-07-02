(in-package "MP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFINE-ATOMIC-EXPANSION, GET-ATOMIC-EXPANSION
;;;

(defun atomic-expander (symbol)
  (core:get-sysprop symbol 'atomic-expander))
(defun (setf atomic-expander) (expander symbol)
  (setf (core:get-sysprop symbol 'atomic-expander) expander))

(defun get-atomic-expansion (place &rest keys
                             &key environment (order nil orderp)
                             &allow-other-keys)
  "Analogous to GET-SETF-EXPANSION. Returns the following seven values:
* a list of temporary variables, which will be bound as if by LET*
* a list of forms, whose results will be bound to the variables
* a variable for the old value of PLACE, for use in CAS
* a variable for the new value of PLACE, for use in CAS and SETF
* a form to atomically read the value of PLACE
* a form to atomically write the value of PLACE
* a form to perform an atomic EQ-based compare-and-swap of PLACE
The keyword arguments are passed unmodified to the expander, except that
defaulting of ORDER is applied."
  (declare (ignore order))
  ;; Default the order parameter. KLUDGEy.
  (unless orderp (setf keys (list* :order :sequentially-consistent keys)))
  (etypecase place
    (symbol
     ;; KLUDGE: This will not work in bclasp at all, and the cleavir interface
     ;; may not be great for this.
     #-(or cclasp eclasp)
     (multiple-value-bind (expansion expanded)
         (macroexpand-1 place environment)
       (if expanded
           (apply #'get-atomic-expansion expansion keys)
           (error "Atomic operations on lexical variables not supported yet")))
     #+(or cclasp eclasp)
     (let ((info (cleavir-env:variable-info
                  clasp-cleavir:*clasp-system* environment place)))
       (etypecase info
         (cleavir-env:symbol-macro-info
          (apply #'get-atomic-expansion (macroexpand-1 place environment) keys))
         (cleavir-env:special-variable-info
          (apply #'get-atomic-expansion `(symbol-value ',place) keys))
         (cleavir-env:lexical-variable-info
          ;; TODO
          (error 'not-atomic :place place))
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
                 (error 'not-atomic :place place))))))))

(defun expand-atomic-expander (name place-ll expander-ll body)
  (let ((place (gensym "PLACE")))
    (multiple-value-bind (decls body doc)
        (core:process-declarations body t)
      ;; FIXME: probably have to sort the decls by lambda list (ugh)
      `(lambda (,place ,@expander-ll)
         (declare ,@decls)
         ,@(when doc (list doc))
         (destructuring-bind ,place-ll (rest ,place)
           (block ,name ,@body))))))

(defmacro define-atomic-expander (accessor
                                  place-lambda-list expander-lambda-list
                                  &body body)
  "Analogous to DEFINE-SETF-EXPANDER; defines how to access (accessor ...)
places atomically.
The body must return the seven values of GET-ATOMIC-EXPANSION.
It is up to you the definer to ensure the swap is performed atomically.
This means you will almost certainly need Clasp's synchronization operators
(e.g., CAS on some other place).
Unlike setf expanders, atomic expanders can take arbitrary keyword arguments.
These correspond to any keyword arguments used in an ATOMIC place, plus the
keyword :environment which holds the environment, and the defaulting of :order
to :sequentially-consistent. The EXPANDER-LAMBDA-LIST is this lambda list.
All expanders should be prepared to accept :order and :environment. Anything
beyond that is your extension."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (atomic-expander ',accessor)
           ,(expand-atomic-expander
             accessor place-lambda-list expander-lambda-list body))))

;;; RELEASE and ACQUIRE-RELEASE don't really make sense for writes
(defun reduce-read-order (order)
  (ecase order
    ((:sequentially-consistent :acquire :relaxed) order)
    ((:acquire-release) :acquire)
    ((:release) :relaxed)))

;;; same idea
(defun reduce-write-order (order)
  (ecase order
    ((:sequentially-consistent :release :relaxed) order)
    ((:acquire-release) :release)
    ((:acquire) :relaxed)))

(defmacro define-simple-atomic-expander (name (&rest params)
                                         reader writer casser
                                         &optional documentation)
  (let ((stemps (loop repeat (length params) collect (gensym "TEMP"))))
    `(define-atomic-expander ,name (,@params) (&key order environment)
       (declare (ignore environment))
       ,@(when documentation (list documentation))
       (let ((scmp (gensym "CMP")) (snew (gensym "NEW"))
             ,@(loop for stemp in stemps
                     collect `(,stemp (gensym "TEMP")))
             (read-order (reduce-read-order order))
             (write-order (reduce-write-order order)))
         (values (list ,@stemps) (list ,@params) scmp snew
                 (list ',reader read-order ,@stemps)
                 (list 'progn (list ',writer write-order snew ,@stemps) snew)
                 (list ',casser order scmp snew ,@stemps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ATOMIC itself
;;;

(defmacro atomic (place &rest keys &key order &allow-other-keys
                  &environment env)
  "(ATOMIC place &key order &allow-other-keys)
Atomically read from PLACE. ORDER is an atomic ordering specifier, i.e. one of
the keywords:
:RELAXED :ACQUIRE :RELEASE :ACQUIRE-RELEASE :SEQUENTIALLY-CONSISTENT
The default is the last. The meanings of these match the C++ standard (more
detailed explanation forthcoming elsewhere).
Other keywords are passed to the atomic expander function.
Experimental."
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
;;; CAS
;;;

(defmacro cas (place old new &rest keys &key order &allow-other-keys
               &environment env)
  "(CAS place old new)
Atomically store NEW in PLACE if OLD matches the current value of PLACE.
Matching is as if by EQ.
Returns the previous value of PLACE; if it's EQ to OLD the swap happened.
Only the swap is atomic. Evaluation of PLACE's subforms, OLD, and NEW is
not guaranteed to be in any sense atomic with the swap, and likely won't be.
PLACE must be a CAS-able place. CAS-able places are either symbol macros,
special variables,
or accessor forms with a CAR of
SYMBOL-VALUE, SYMBOL-PLIST, CLOS:STANDARD-INSTANCE-ACCESS, THE,
SLOT-VALUE, CLOS:SLOT-VALUE-USING-CLASS, CAR, CDR, FIRST, REST, SVREF,
or macro forms that expand into CAS-able places,
or an accessor defined with DEFINE-ATOMIC-EXPANDER.
Some CAS accessors have additional semantic constraints.
You can see their documentation with e.g. (documentation 'slot-value 'mp:atomic)
This is planned to be expanded to include variables,
possibly other simple vectors, and slot accessors.
Keys are passed to GET-ATOMIC-EXPANSION.
Experimental."
  (declare (ignore order))
  (multiple-value-bind (temps values oldvar newvar read write cas)
      (apply #'get-atomic-expansion place :environment env keys)
    (declare (ignore read write))
    `(let* (,@(mapcar #'list temps values)
            (,oldvar ,old) (,newvar ,new))
       ,cas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived operators
;;;

(defmacro atomic-update-explicit ((place
                                   &rest keys &key order &allow-other-keys)
                                  update-fn &rest arguments &environment env)
  (declare (ignore order))
  (multiple-value-bind (vars vals old new read write cas)
      (apply #'get-atomic-expansion place :environment env keys)
    (declare (ignore write))
    (let ((gfn (gensym "UPDATE-FN"))
          (asyms (loop repeat (length arguments) collect (gensym "ARG"))))
      `(let* (,@(mapcar #'list vars vals)
              (,gfn ,update-fn)
              ,@(mapcar #'list asyms arguments)
              (,old ,read))
         (loop for ,new = (funcall ,gfn ,old ,@asyms)
               until (eq ,old (setf ,old ,cas))
               finally (return ,new))))))

(defmacro atomic-update (place update-fn &rest arguments)
  "Perform an atomic update of PLACE. In more detail, the value of PLACE is
set to (funcall UPDATE-FN VALUE ARGUMENTS...), where VALUE is the old value of
PLACE. This is analogous to what DEFINE-MODIFY-MACRO expansions do.
As with DEFINE-MODIFY-MACRO, the new value is returned.
Evaluation order is left to right as specified in CLHS 5.1.1.1. Note that this
is different from the SBCL macro of the same name, which may perform multiple
evaluations of the update-fn and arguments, and passes arguments to the update
function in a different order."
  `(atomic-update-explicit (,place) ,update-fn ,@arguments))

(defmacro atomic-incf-explicit ((place &rest keys &key order &allow-other-keys)
                                &optional (delta 1))
  (declare (ignore order))
  `(atomic-update-explicit (,place ,@keys) #'+ ,delta))

(defmacro atomic-decf-explicit ((place &rest keys &key order &allow-other-keys)
                                &optional (delta 1))
  (declare (ignore order))
  `(atomic-update-explicit (,place ,@keys) #'- ,delta))

(defmacro atomic-incf (place &optional (delta 1))
  `(atomic-update ,place #'+ ,delta))

(defmacro atomic-decf (place &optional (delta 1))
  `(atomic-update ,place #'- ,delta))

(defmacro atomic-push-explicit
    (item (place &rest keys &key order &allow-other-keys) &environment env)
  (declare (ignore order))
  (multiple-value-bind (vars vals old new read write cas)
      (apply #'get-atomic-expansion place :environment env keys)
    (declare (ignore write))
    (let ((gitem (gensym "ITEM")))
      `(let* ((,gitem ,item) ; evaluate left-to-right (CLHS 5.1.1.1)
              ,@(mapcar #'list vars vals)
              (,old ,read)
              (,new (cons ,gitem ,old)))
         (loop until (eq ,old (setf ,old ,cas))
               do (setf (cdr ,new) ,old)
               finally (return ,new))))))

(defmacro atomic-push (item place)
  "As CL:PUSH, but as an atomic RMW operation."
  `(atomic-push-explicit ,item (,place)))

(defmacro atomic-pop-explicit ((place &rest keys &key order &allow-other-keys)
                               &environment env)
  (declare (ignore order))
  (multiple-value-bind (vars vals old new read write cas)
      (apply #'get-atomic-expansion place :environment env keys)
    (declare (ignore write))
    `(let* (,@(mapcar #'list vars vals)
            (,old ,read))
       (loop (let ((,new (cdr ,old)))
               (when (eq ,old (setf ,old ,cas))
                 (return (car ,old))))))))

(defmacro atomic-pop (place)
  "As CL:POP, but as an atomic RMW operation."
  `(atomic-pop-explicit (,place)))

(defmacro atomic-pushnew-explicit (item (place
                                         &rest place-keys
                                         &key order &allow-other-keys)
                                   &rest keys &key key test test-not
                                   &environment env)
  (declare (ignore key test test-not) (ignore order))
  (multiple-value-bind (vars vals old new read write cas)
      (apply #'get-atomic-expansion place :environment env place-keys)
    (declare (ignore write))
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

(defmacro atomic-pushnew (item place &rest keys &key test test-not key)
  (declare (ignore test test-not key))
  "As CL:PUSHNEW, but as an atomic RMW operation.
ITEM, the subforms of PLACE, and the keywords are evaluated exactly once in the
same order as they are for CL:PUSHNEW, specified in CLHS 5.1.1.1."
  `(atomic-pushnew-explicit ,item (,place) ,@keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Particular atomic expanders
;;;

(define-atomic-expander the (type place) (&rest keys)
  "(cas (the y x) o n) = (cas x (the y o) (the y n))"
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
  core:car-atomic core:rplaca-atomic core:cas-car)
(define-simple-atomic-expander cdr (list)
  core:cdr-atomic core:rplacd-atomic core:cas-cdr)

(define-simple-atomic-expander core:rack-ref (rack index)
  core::atomic-rack-read core::atomic-rack-write core::cas-rack)

;; Ignores order specification for the moment.
(define-atomic-expander symbol-value (symbol) (&key order environment)
  (declare (ignore order environment))
  "Because special variable bindings are always thread-local, the symbol-value
of a symbol can only be used for synchronization through this accessor if there
are no bindings (in which case the global, thread-shared value is used."
  (let ((gs (gensym "SYMBOL")) (cmp (gensym "CMP")) (new (gensym "NEW")))
    (values (list gs) (list symbol) cmp new
            `(core:atomic-symbol-value ,gs)
            `(progn (core:atomic-set-symbol-value ,new ,gs) ,new)
            `(core:cas-symbol-value ,cmp ,new ,gs))))

(define-atomic-expander symbol-plist (symbol) (&key order environment)
  (declare (ignore order environment))
  (let ((gs (gensym "SYMBOL")) (cmp (gensym "CMP")) (new (gensym "NEW")))
    (values (list gs) (list symbol) cmp new
            `(core:atomic-symbol-plist ,gs)
            `(progn (core:atomic-set-symbol-plist ,new ,gs) ,new)
            `(core:cas-symbol-plist ,cmp ,new ,gs))))

(define-atomic-expander svref (simple-vector index) (&key order environment)
  (declare (ignore environment))
  (let ((gv (gensym "VECTOR")) (gi (gensym "INDEX"))
        (cmp (gensym "CMP")) (new (gensym "NEW"))
        (read-order (reduce-read-order order))
        (write-order (reduce-write-order order)))
    (values (list gv gi)
            (list `(the simple-vector ,simple-vector)
                  `(let ((,gi (the fixnum ,index)))
                     (unless (array-in-bounds-p ,gv ,gi)
                       (error 'core:sequence-out-of-bounds
                              :datum ,gi
                              :expected-type (list 'integer 0 (length ,gv))
                              :object ,gv))
                     ,gi))
            cmp new
            `(core::atomic-aref ,read-order ,gv ,gi)
            `(setf (core::atomic-aref ,write-order ,gv ,gi) ,new)
            `(core::acas ,order ,cmp ,new ,gv ,gi))))

#+(or)
(define-simple-atomic-expander svref (vector index)
  core:atomic-svref core:atomic-svset core:cas-svref)
