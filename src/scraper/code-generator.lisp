(in-package :cscrape)

(define-constant *batch-classes* 3)
(defparameter *function-partitions* 3)

(define-constant +root-dummy-class+ "::_RootDummyClass" :test 'equal)

(define-condition bad-c++-name (error)
  ((name :initarg :name :accessor name))
  (:report (lambda (condition stream)
             (format stream "Bad C++ function name: ~a" (name condition)))))


(defun file-line (object)
  ;;; Displaying the line number causes trivial changes to the code that move lines
  ;;; to recompile a lot of code
  (format nil "~a" (file% object))
  #+(or) (format nil "~a:~a" (file% object) (line% object)))


(defun validate-va-rest (lambda-list function-name)
  (unless (stringp lambda-list)
    (error "Expected lambda-list ~s to be a string" lambda-list))
  (when (search "&va-rest" lambda-list)
    (unless (search "VaList_" function-name)
      (error "Possible core:&va-rest consistency problem!!!!!~% A lambda list ~s contains &va-rest but the function ~s doesn't take VaList_sp as an argument"
             lambda-list function-name)))
  (when (search "VaList_" function-name)
    (when (search "&rest" lambda-list)
      (error "Possible core:&va-rest consistency problem!!!!!~% A function-name ~s contains VaList_sp but the function takes a &rest argument"
             function-name))))

(defun partition-list (list parts &key (last-part-longer nil))
  ;; Partition LIST into PARTS parts.  They will all be the same
  ;; length except the last one which will be shorter or, if
  ;; LAST-PART-LONGER is true, longer.  Doesn't deal with the case
  ;; where there are less than PARTS elements in LIST at all (it does
  ;; something, but it may not be sensible).
  (loop with size = (if last-part-longer
                        (floor (length list) parts)
                      (ceiling (length list) parts))
        and tail = list
        for part upfrom 1
        while tail
        collect (loop for pt on tail
                      for i upfrom 0
                      while (or (and last-part-longer (= part parts))
                                (< i size))
                      collect (first pt)
                      finally (setf tail pt))))

(defun group-expose-functions-by-namespace (functions)
  (declare (optimize (speed 3)))
  (let ((ns-hashes (make-hash-table :test #'equal)))
    (dolist (func functions)
      (let* ((namespace (namespace% func))
             (ns-ht (gethash namespace ns-hashes (make-hash-table :test #'equal))))
        (setf (gethash (lisp-name% func) ns-ht) func)
        (setf (gethash namespace ns-hashes) ns-ht)))
    ns-hashes))

(defun generate-expose-function-signatures (sout ns-grouped-expose-functions)
  (format sout "#ifdef EXPOSE_FUNCTION_SIGNATURES~%")
  (maphash (lambda (ns func-ht)
             (let* ((decl-count 0)
                    (output (with-output-to-string (sdec)
                              (maphash (lambda (name f)
                                         (declare (ignore name))
                                         (when (and (typep f '(or expose-defun expose-defun-setf))
                                                    (provide-declaration% f))
                                           (format sdec "    ~a;~%" (signature% f))
                                           (incf decl-count)))
                                       func-ht))))
               (when (> decl-count 0)
                 (format sout "namespace ~a {~%" ns)
                 (format sout "~a" output)
                 (format sout "};~%"))))
           ns-grouped-expose-functions)
  (format sout "#endif // EXPOSE_FUNCTION_SIGNATURES~%"))

#+(or)(defun split-c++-name (name)
        (declare (optimize (speed 3)))
        (let ((under (search "__" name :test #'string=)))
          (unless under
            (error 'bad-c++-name :name name))
          (let* ((name-pos (+ 2 under)))
            (values (subseq name 0 under)
                    (subseq name name-pos)))))

(defun maybe-wrap-lambda-list (ll)
  (if (> (length ll) 0)
      (format nil "(~a)" ll)
      ll))

(defun generate-expose-function-bindings (sout ns-grouped-expose-functions start-index)
  (declare (optimize (speed 3)))
  (flet ((expose-one (f ns index)
           (let ((name (format nil "expose_function_~d_helper" index)))
             (format sout "NOINLINE void ~a() {~%" name)
             (etypecase f
               (expose-defun
                (format sout "  /* expose-defun */ expose_function(~a,&~a::~a,~s);~%"
                        (lisp-name% f)
                        ns
                        (function-name% f)
                        (maybe-wrap-lambda-list (lambda-list% f))))
               (expose-defun-setf
                (format sout "  /* expose-defun-setf */ expose_function_setf(~a,&~a::~a,~s);~%"
                        (lisp-name% f)
                        ns
                        (function-name% f)
                        (maybe-wrap-lambda-list (lambda-list% f))))
               (expose-extern-defun
                (format sout "  // ~a~%" (file-line f))
                (format sout "  /* expose-extern-defun */ expose_function(~a,~a,~s);~%"
                        (lisp-name% f)
                        (pointer% f)
                        (if (lambda-list% f)
                            (maybe-wrap-lambda-list (lambda-list% f))
                            ""))))
             (format sout "}~%")
             (cons name f))))
    (let (helpers
          (index start-index))
      (format sout "#ifdef EXPOSE_FUNCTION_BINDINGS_HELPERS~%")
      (maphash (lambda (ns funcs-ht)
                 (maphash (lambda (name f)
                            (declare (ignore name))
                            (handler-case
                                (push (expose-one f ns (incf index)) helpers)
                              (serious-condition (condition)
                                (error "There was an error while exposing a function in ~a at line ~d~%~a~%" (file% f) (line% f) condition))))
                          funcs-ht))
               ns-grouped-expose-functions)
      (format sout "#endif // EXPOSE_FUNCTION_BINDINGS_HELPERS~%")
      (format sout "#ifdef EXPOSE_FUNCTION_BINDINGS~%")
      (let* ((unsorted-helpers (nreverse helpers))
             (sorted-helpers (stable-sort unsorted-helpers #'< :key (lambda (x) (priority% (cdr x))))))
        (dolist (helper sorted-helpers)
          (format sout "  ~a(); // ~a[~a]~%" (car helper) (lisp-name% (cdr helper)) (priority% (cdr helper)))))
      (format sout "#endif // EXPOSE_FUNCTION_BINDINGS~%")
      index)))

(defun generate-expose-one-source-info-helper (sout obj idx)
  (let* ((lisp-name (lisp-name% obj))
         (absolute-file (truename (pathname (file% obj))))
         (file (enough-namestring absolute-file (pathname (format nil "~a/" (uiop:getenv "CLASP_HOME")))))
         (line (line% obj))
         (char-offset (character-offset% obj))
         (docstring (docstring% obj))
         (kind (cond
                 ((typep obj 'function-mixin) "code_kind")
                 ((typep obj 'class-method-mixin) "code_kind")
                 ((typep obj 'method-mixin) "method_kind")
                 ((typep obj 'exposed-class) "class_kind")
                 (t "unknown_kind")))
         (helper-name (format nil "source_info_~d_helper" idx)))
    (format sout "NOINLINE void source_info_~d_helper() {~%" idx)
    (format sout " define_source_info( ~a, ~a, ~s, ~d, ~d, ~a );~%"
            kind lisp-name file char-offset line docstring )
    (format sout "}~%")
    helper-name))

(defun generate-expose-source-info (sout functions classes)
  (declare (optimize debug))
  (let (helpers
        (index 0))
    (format sout "#ifdef SOURCE_INFO_HELPERS~%")
    (dolist (f functions)
      (push (generate-expose-one-source-info-helper sout f (incf index)) helpers))
    (maphash (lambda (k class)
               (declare (ignore k))
               (push (generate-expose-one-source-info-helper sout class (incf index)) helpers)
               (dolist (method (methods% class))
                 (push (generate-expose-one-source-info-helper sout method (incf index)) helpers))
               (dolist (class-method (class-methods% class))
                 (push (generate-expose-one-source-info-helper sout class-method (incf index)) helpers)))
           classes)
    (format sout "#endif // SOURCE_INFO_HELPERS~%")
    (format sout "#ifdef SOURCE_INFO~%")
    (dolist (helper (nreverse helpers))
      (format sout "  ~a();~%" helper))
    (format sout "#endif // SOURCE_INFO~%")))

(defun generate-code-for-source-info (functions classes)
  (with-output-to-string (sout)
    (generate-expose-source-info sout functions classes)))

#+(or)(defun generate-tags-file (tags-file-name tags)
        (declare (optimize (speed 3)))
        (let* ((source-info-tags (extract-unique-source-info-tags tags))
               (file-ht (make-hash-table :test #'equal)))
          (dolist (tag source-info-tags)
            (push tag (gethash (tags:file tag) file-ht)))
          (let ((tags-data-ht (make-hash-table :test #'equal)))
            (maphash (lambda (file-name file-tags-list)
                       (let ((buffer (make-string-output-stream #+(or):element-type #+(or)'(unsigned-byte 8))))
                         (dolist (tag file-tags-list)
                           (format buffer "~a~a~a,~a~%"
                                   (tags:function-name tag)
                                   (code-char #x7f)
                                   (tags:line tag)
                                   (tags:character-offset tag)))
                         (setf (gethash file-name tags-data-ht) (get-output-stream-string buffer))))
                     file-ht)
            (with-open-file (sout tags-file-name :direction :output #+(or):element-type #+(or)'(unsigned-byte 8)
                                  :if-exists :supersede)
              (maphash (lambda (file buffer)
                         (format sout "~a,~a~%"
                                 file
                                 (length buffer))
                         (princ buffer sout))
                       tags-data-ht)))))

(defun generate-code-for-init-functions (all-functions)
  (declare (optimize (speed 3)))
  (let ((partitions (partition-list all-functions *function-partitions*))
        (index 0))
    (with-output-to-string (sout)
      (loop for functions in partitions
            for batch-num from 1
            do (progn
                 (format sout "#ifdef EXPOSE_FUNCTION_BATCH~a~%" batch-num)
                 (let ((ns-grouped (group-expose-functions-by-namespace functions)))
                   (generate-expose-function-signatures sout ns-grouped)
                   (format sout "// starting with index ~a~%" index)
                   (setf index (generate-expose-function-bindings sout ns-grouped index))
                   )
                 (format sout "#endif // EXPOSE_FUNCTION_BATCH~a~%" batch-num))))))

(defun mangle-and-wrap-name (name arg-types)
  "* Arguments
- name :: A string
* Description
Convert colons to underscores"
  (let ((type-part (with-output-to-string (sout)
                     (loop for type in arg-types
                           do (loop for c across type
                                    do (if (alphanumericp c)
                                           (princ c sout)
                                           (princ #\_ sout)))))))
    (format nil "wrapped_~a_~a" (substitute #\_ #\: name) type-part)))

(defgeneric direct-call-function (c-code cl-code func c-code-info cl-code-info))

(defmethod direct-call-function (c-code cl-code (func t) c-code-info cl-code-info)
  (format c-code "// Do nothing yet for function ~a of type ~a~%" (function-name% func) (type-of func))
  (format cl-code ";;; Do nothing yet for function ~a of type ~a~%" (function-name% func) (type-of func)))

(defmethod direct-call-function (c-code cl-code (func expose-extern-defun) c-code-info cl-code-info)
  (let ((function-ptr (function-ptr% func)))
    (if (function-ptr-type function-ptr)
        (multiple-value-bind (return-type arg-types)
            (convert-function-ptr-to-c++-types function-ptr)
          (let* ((wrapped-name (mangle-and-wrap-name (function-name% func) arg-types))
                 (one-func-code
                   (generate-wrapped-function wrapped-name
                                              (function-ptr-namespace function-ptr)
                                              (function-name% func)
                                              return-type
                                              arg-types)))
            (format c-code "// Generating code for ~a::~a~%" (function-ptr-namespace function-ptr) (function-name% func))
            (format c-code "// return-type -> ~s~%" return-type)
            (format c-code "// arg-types -> ~s~%" arg-types)
            (format c-code "//  Found at ~a-----------~%" (file-line func))
            (format c-code-info "// Generating code for ~a::~a~%" (function-ptr-namespace function-ptr) (function-name% func))
            (format c-code-info "//            Found at ~a~%-----------~%" (file-line func))
            (format c-code "~a~%" one-func-code)
            (format cl-code ";;; Generating code for ~a::~a~%" (function-ptr-namespace function-ptr) (function-name% func))
            (format cl-code-info ";;; Generating code for ~a::~a~%" (function-ptr-namespace function-ptr) (function-name% func))
            (format cl-code-info ";;;            Found at ~a:~a~%----------~%" (file% func) (line% func))
            (let* ((raw-lisp-name (lisp-name% func))
                   (maybe-fixed-magic-name (maybe-fix-magic-name raw-lisp-name)))
              (validate-va-rest (lambda-list% func) wrapped-name)
              (format cl-code "(wrap-c++-function ~a (~a) (~a) ~s )~%" maybe-fixed-magic-name (declare% func) (lambda-list% func) wrapped-name ))))
        (call-next-method))))

(defmethod direct-call-function (c-code cl-code (func expose-defun) c-code-info cl-code-info)
  (multiple-value-bind (return-type arg-types)
      (parse-types-from-signature (signature% func))
    (let* ((wrapped-name (mangle-and-wrap-name (function-name% func) arg-types))
           (one-func-code
            (generate-wrapped-function wrapped-name
                                       (namespace% func)
                                       (function-name% func)
                                       return-type arg-types)))
      (format c-code "// Generating code for ~a::~a~%" (namespace% func) (function-name% func))
      (format c-code-info "// Generating code for ~a::~a~%" (namespace% func) (function-name% func))
      (format c-code-info "//            Found at ~a~%-----------~%" (file-line func))
      (format c-code "~a~%" one-func-code)
      (format cl-code ";;; Generating code for ~a::~a~%" (namespace% func) (function-name% func))
      (format cl-code-info ";;; Generating code for ~a::~a~%" (namespace% func) (function-name% func))
      (format cl-code-info ";;;            Found at ~a:~a~%----------~%" (file% func) (line% func))
      (let* ((raw-lisp-name (lisp-name% func))
             (maybe-fixed-magic-name (maybe-fix-magic-name raw-lisp-name)))
        (validate-va-rest (lambda-list% func) wrapped-name)
        (format cl-code "(wrap-c++-function ~a (~a) (~a) ~s )~%" maybe-fixed-magic-name (declare% func) (lambda-list% func) wrapped-name )))))

(defmethod direct-call-function (c-code cl-code (func expose-defun-setf) c-code-info cl-code-info)
  (multiple-value-bind (return-type arg-types)
      (parse-types-from-signature (signature% func))
    (let* ((wrapped-name (mangle-and-wrap-name (function-name% func) arg-types))
           (one-func-code
            (generate-wrapped-function wrapped-name
                                       (namespace% func)
                                       (function-name% func)
                                       return-type arg-types)))
      (format c-code "// Generating code for ~a::~a~%" (namespace% func) (function-name% func))
      (format c-code-info "// Generating code for ~a::~a~%" (namespace% func) (function-name% func))
      (format c-code-info "//            Found at ~a~%-----------~%" (file-line func))
      (format c-code "~a~%" one-func-code)
      (format cl-code ";;; Generating code for ~a::~a~%" (namespace% func) (function-name% func))
      (format cl-code-info ";;; Generating code for ~a::~a~%" (namespace% func) (function-name% func))
      (format cl-code-info ";;;            Found at ~a:~a~%----------~%" (file% func) (line% func))
      (let* ((raw-lisp-name (lisp-name% func))
             (maybe-fixed-magic-name (maybe-fix-magic-name raw-lisp-name)))
        (validate-va-rest (lambda-list% func) wrapped-name)
        (format cl-code "(wrap-c++-function-setf ~a (~a) (~a) ~s )~%" maybe-fixed-magic-name (declare% func) (lambda-list% func) wrapped-name )))))

(defun generate-code-for-direct-call-functions (functions)
  (let ((c-code (make-string-output-stream))
        (c-code-info (make-string-output-stream))
        (cl-code (make-string-output-stream))
        (cl-code-info (make-string-output-stream))
        (*print-pretty* nil))
    (format cl-code "(in-package :core)~%")
    (mapc (lambda (func)
            (direct-call-function c-code cl-code func c-code-info cl-code-info))
          functions)
    (values (get-output-stream-string c-code)
            (get-output-stream-string cl-code)
            (get-output-stream-string c-code-info)
            (get-output-stream-string cl-code-info))))

(define-condition broken-inheritance (error)
    ((class-with-missing-parent :initarg :class-with-missing-parent :accessor class-with-missing-parent)
     (starting-possible-child-class :initarg :starting-possible-child-class :accessor starting-possible-child-class)
     (possible-ancestor-class :initarg :possible-ancestor-class :accessor possible-ancestor-class))
  (:report (lambda (condition stream)
             (format stream "While testing if the ancestor of ~a is ~a ~%   the chain of inheritance was broken when no parent could be found for ~a~%"
                     (starting-possible-child-class condition)
                     (possible-ancestor-class condition)
                     (class-with-missing-parent condition)))))

(define-condition possible-recursive-inheritance (error)
    ((class-with-missing-parent :initarg :class-with-missing-parent :accessor class-with-missing-parent)
     (starting-possible-child-class :initarg :starting-possible-child-class :accessor starting-possible-child-class)
     (possible-ancestor-class :initarg :possible-ancestor-class :accessor possible-ancestor-class))
  (:report (lambda (condition stream)
             (format stream "While testing if the ancestor of ~a is ~a ~%   the chain of inheritance was broken when no parent could be found for ~a~%"
                     (starting-possible-child-class condition)
                     (possible-ancestor-class condition)
                     (class-with-missing-parent condition)))))

(defun inherits-from* (x-name y-name inheritance)
  (let ((depth 0)
        ancestor
        (entry-x-name x-name))
    (loop
       (setf ancestor (gethash x-name inheritance))
       (when (string= ancestor +root-dummy-class+)
         (return-from inherits-from* nil))
       (unless ancestor
         (error 'broken-inheritance
                :class-with-missing-parent x-name
                :starting-possible-child-class entry-x-name
                :possible-ancestor-class y-name))
       (if (string= ancestor y-name)
           (return-from inherits-from* t))
       (incf depth)
       (when (> depth 20)
         (error "While testing if ~a inherits from ~a the maximum depth test was hit - current class is ~a - check for recursive class definition of ~a" entry-x-name y-name x-name entry-x-name))
       (setf x-name ancestor))))

(defun inherits-from (x y inheritance)
  (declare (optimize debug))
  (let ((x-name (class-key% x))
        (y-name (class-key% y)))
    (inherits-from* x-name y-name inheritance)))

(defun sort-classes-by-inheritance (exposed-classes)
  (declare (optimize debug))
  (let ((inheritance (make-hash-table :test #'equal))
        (classes nil))
    (maphash (lambda (k v)
               (let ((base (base% v)))
                 (when base (setf (gethash k inheritance) base))
                 (push v classes)))
             exposed-classes)
    (handler-case
        (setf classes (sort classes (lambda (x y)
                                      (not (inherits-from x y inheritance)))))
      #+(or)
      (broken-inheritance (e)
        (let ((x (starting-possible-child-class e)))
          (error "~a~%    The info for ~a is ~a"
                 (with-output-to-string (sout) (print-object e sout))
                 x
                 (gethash x exposed-classes)))))
    (values classes inheritance)))

(defun generate-code-for-init-class-kinds (exposed-classes sout)
  (declare (optimize (speed 3)))
  (let ((sorted-classes (sort-classes-by-inheritance exposed-classes)))
    (format sout "#ifdef SET_CLASS_KINDS~%")
    (dolist (exposed-class sorted-classes)
      (format sout "set_one_static_class_Header<~a::~a>();~%"
              (tags:namespace% (class-tag% exposed-class))
              (tags:name% (class-tag% exposed-class))))
    (format sout "#endif // SET_CLASS_KINDS~%")))

(defun generate-code-for-init-classes-class-symbols (exposed-classes sout)
  (declare (optimize (speed 3)))
  (let ((sorted-classes (sort-classes-by-inheritance exposed-classes)))
    (format sout "#ifdef SET_CLASS_SYMBOLS~%")
    (dolist (exposed-class sorted-classes)
      (format sout "set_one_static_class_symbol<~a::~a>(bootStrapSymbolMap,~a);~%"
              (tags:namespace% (class-tag% exposed-class))
              (tags:name% (class-tag% exposed-class))
              (lisp-name% exposed-class)))
    (format sout "#endif // SET_CLASS_SYMBOLS~%")))

(defun as-var-name (ns name)
  (format nil "~a_~a_var" ns name))

(defun build-enum-name (key)
  (with-output-to-string (sout)
    (loop for c across key
          if (alphanumericp c) do (princ c sout)
            else do (princ #\_ sout))))

;;; This is used to determine stamp ranges. Since subclasses always have a
;;; higher stamp value than their superclasses, the low end of the range for
;;; any class is just that class itself.
(defun highest-stamp-class (c)
  (if (direct-subclasses% c)
      (let ((high-stamp 0)
            (high-class nil))
        (dolist (cs (direct-subclasses% c))
          (multiple-value-bind (temp-stamp temp-class)
              (highest-stamp-class cs)
            (if (> temp-stamp high-stamp)
                (setf high-stamp temp-stamp
                      high-class temp-class))))
        (values high-stamp high-class))
      (values (stamp% c) c)))

(defun split-class-key (key)
  (let ((pos (search "::" key)))
    (values (subseq key 0 pos) (subseq key (+ 2 pos)))))

(defun extract-forwards (classes)
  (let ((namespaces (make-hash-table :test #'equal)))
    (maphash (lambda (key class)
               (declare (ignore class))
               (multiple-value-bind (namespace class-name)
                   (split-class-key key)
                 (push class-name (gethash namespace namespaces nil))))
             classes)
    namespaces))

(defun generate-mps-poison (sout)
  "Sections that are only applicable to Boehm builds include this to prevent them from compiling in MPS builds"
  (format sout " #if defined(USE_ANALYSIS)~%")
  (format sout "  #error \"Do not include this section when USE_ANALYSIS is defined - use the section from clasp_gc_xxx.cc\"~%")
  (format sout " #endif // USE_ANALYSIS~%"))

(defun gather-all-subclasses-for (class-key inheritance)
  (loop for x in (gethash class-key inheritance)
        append (cons x
                     (gather-all-subclasses-for x inheritance))))

(defun gather-all-subclasses (inheritance)
  (let ((reverse-inheritance (make-hash-table :test #'equal))
        (deep-inheritance (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (push key (gethash value reverse-inheritance)))
             inheritance)
    (maphash (lambda (key value)
               (loop for subclasses in value
                     do (loop for sub in (gather-all-subclasses-for key reverse-inheritance)
                              do (pushnew sub (gethash key deep-inheritance) :test #'string=))))
             reverse-inheritance)
    deep-inheritance))

(defgeneric stamp-value (class))


(defconstant +stamp-shift+    2)
(defconstant +derivable-wtag+ #B00)
(defconstant +rack-wtag+      #B01)
(defconstant +wrapped-wtag+   #B10)
(defconstant +header-wtag+    #B11)
(defconstant +max-wtag+       #B11)

(defun adjust-stamp (stamp &optional (wtag +max-wtag+))
  (logior (ash stamp +stamp-shift+) wtag))

(defmethod stamp-value ((class gc-managed-type))
  (adjust-stamp (stamp% class) +header-wtag+))

(defun class-wtag (class)
  (let ((ckey (class-key% class)))
    (cond ((member ckey '("core::Instance_O" "core::FuncallableInstance_O"
                          "clbind::ClassRep_O")
                   :test #'string=)
           +rack-wtag+)
          ((member ckey '("core::WrappedPointer_O") :test #'string=)
           +wrapped-wtag+)
          ((member ckey '("core::DerivableCxxObject_O") :test #'string=)
           +derivable-wtag+)
          (t +header-wtag+))))

(defmethod stamp-value ((class kind))
  (adjust-stamp (stamp% class) (tags:stamp-wtag class)))

(defmethod stamp-value ((class t))
  "This could change the value of stamps for specific classes - but that would break quick typechecks like (typeq x Number)"
  (adjust-stamp (stamp% class) (class-wtag class)))

(defun generate-declare-forwards (stream exposed-classes)
  (format stream "#ifdef DECLARE_FORWARDS~%")
  (generate-mps-poison stream)
  (let ((forwards (extract-forwards exposed-classes)))
    (maphash (lambda (namespace classes)
               (format stream "namespace ~a {~%~{  class ~a;~%~}~%};~%"
                       namespace classes))
             forwards))
  (format stream "#endif // DECLARE_FORWARDS~%"))

(defun generate-declare-inheritance (stream sorted-classes inheritance)
  (format stream "#ifdef DECLARE_INHERITANCE~%")
  (let ((reverse-classes (reverse sorted-classes))
        (all-subclasses (gather-all-subclasses inheritance)))
    (format t "all-subclasses = ~a~%" all-subclasses)
    (loop for class in reverse-classes
          for subclasses = (gethash (class-key% class) all-subclasses)
          until (string= (class-key% class) "core::T_O")
          do (format stream "// ~a~%" (class-key% class))
             (loop for subclass in subclasses
                   do (format stream " template <> struct Inherits<::~a,::~a> : public std::true_type  {};~%"
                              (class-key% class) subclass))))
  (format stream "#endif // DECLARE_INHERITANCE~%"))

(defun generate-gc-enum (stream sorted-classes gc-managed-types)
  (format stream "#ifdef GC_ENUM~%")
  (let ((stamp-max 0))
    (dolist (c sorted-classes)
      (format stream "STAMPWTAG_~a = ADJUST_STAMP(~a), // stamp ~d unshifted 0x~x  shifted 0x~x~%"
              (build-enum-name (class-key% c)) (stamp-value c)
              (ash (stamp-value c) (- +stamp-shift+)) (stamp-value c)
              (ash (stamp-value c) +stamp-shift+))
      (setf stamp-max (max stamp-max (stamp-value c))))
    (maphash (lambda (key type)
               (declare (ignore key))
               (format stream "STAMPWTAG_~a = ADJUST_STAMP(~a),~%"
                       (build-enum-name (c++type% type)) (stamp-value type))
               (setf stamp-max (max stamp-max (stamp-value type))))
             gc-managed-types)
    (format stream "STAMPWTAG_max = ADJUST_STAMP(~a),~%" stamp-max))
  (format stream "#endif // GC_ENUM~%"))

(defun generate-gc-enum-names (stream sorted-classes gc-managed-types)
  (format stream "#ifdef GC_ENUM_NAMES~%")
  (dolist (c sorted-classes)
    (format stream "register_stamp_name(\"STAMPWTAG_~a\",ADJUST_STAMP(~a));~%" (build-enum-name (class-key% c)) (stamp-value c)))
  (maphash (lambda (key type)
             (declare (ignore key))
             (format stream "register_stamp_name(\"STAMPWTAG_~a\",ADJUST_STAMP(~a));~%"
                     (build-enum-name (c++type% type))
                     (stamp-value type)))
           gc-managed-types)
  (format stream "#endif // GC_ENUM_NAMES~%"))

(defun generate-gc-stamp-selectors (stream sorted-classes gc-managed-types)
  (format stream "#ifdef GC_STAMP_SELECTORS~%")
  (flet ((write-one-gcstamp (type mangled-key)
           (format stream "template <> class gctools::GCStamp<~a> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::STAMPWTAG_~a;
};~%"
                   type mangled-key)))
    (generate-mps-poison stream)
    (dolist (c sorted-classes)
      (write-one-gcstamp (class-key% c) (build-enum-name (class-key% c))))
    (maphash (lambda (key type)
               (declare (ignore key))
               (write-one-gcstamp (c++type% type)
                                  (build-enum-name (c++type% type))))
             gc-managed-types))
  (format stream "#endif // GC_STAMP_SELECTORS~%"))

(defun generate-gc-dynamic-cast (stream sorted-classes)
  (format stream "#ifdef GC_DYNAMIC_CAST~%")
  (generate-mps-poison stream)
  (dolist (c sorted-classes)
    (format stream "template <typename FP> struct Cast<~a*,FP> {
  inline static bool isA(FP client) {
    gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(GeneralPtrToHeaderPtr(client));
    size_t kindVal = header->shifted_stamp();~%"
            (class-key% c))
    (let ((high-stamp (highest-stamp-class c)))
      (if (eq (stamp% c) high-stamp)
          (format stream "    // IsA-stamp-range ~a val -> ADJUST_STAMP(~a)
    return (kindVal == ISA_ADJUST_STAMP(~a));~%"
                  (class-key% c) (stamp-value c) (stamp-value c))
          (format stream "    // IsA-stamp-range ~a low high -> ISA_ADJUST_STAMP(~a) ISA_ADJUST_STAMP(~a)
    return ((ISA_ADJUST_STAMP(~a) <= kindVal) && (kindVal <= ISA_ADJUST_STAMP(~a)));~%"
                  (class-key% c)
                  (stamp-value c) (adjust-stamp high-stamp)
                  (stamp-value c) (adjust-stamp high-stamp))))
    (format stream "  };~%};~%"))
  (format stream "#endif // GC_DYNAMIC_CAST~%"))

(defun generate-gc-typeq (stream sorted-classes)
  (format stream "#ifdef GC_TYPEQ~%")
  (dolist (c sorted-classes)
    (multiple-value-bind (high-stamp high-class)
        (highest-stamp-class c)
      (if (eq (stamp% c) high-stamp)
          (format stream "    ADD_SINGLE_TYPEQ_TEST(~a,TYPEQ_ADJUST_STAMP(~a)); ~%"
                  (class-key% c) (stamp-value c))
          (format stream "    ADD_RANGE_TYPEQ_TEST(~a,~a,TYPEQ_ADJUST_STAMP(~a),TYPEQ_ADJUST_STAMP(~a));~%"
                  (class-key% c) (class-key% high-class)
                  (stamp-value c) (adjust-stamp high-stamp)))))
  (format stream "#endif // GC_TYPEQ~%"))

(defun generate-allocate-all-classes (stream sorted-classes)
  (format stream "#ifdef ALLOCATE_ALL_CLASSES~%")
  (dolist (exposed-class sorted-classes)
    (format stream "gctools::smart_ptr<core::Instance_O> ~a = allocate_one_class<~a::~a>(~a);~%"
            (as-var-name (tags:namespace% (class-tag% exposed-class))
                         (tags:name% (class-tag% exposed-class)))
            (tags:namespace% (class-tag% exposed-class))
            (tags:name% (class-tag% exposed-class))
            (meta-class% exposed-class)))
  (format stream "#endif // ALLOCATE_ALL_CLASSES~%"))

(defun generate-set-bases-all-classes (stream sorted-classes)
  (format stream "#ifdef SET_BASES_ALL_CLASSES~%")
  (dolist (exposed-class sorted-classes)
    (unless (string= (base% exposed-class) +root-dummy-class+)
      (format stream "~a->addInstanceBaseClassDoNotCalculateClassPrecedenceList(~a::static_classSymbol());~%"
              (as-var-name (tags:namespace% (class-tag% exposed-class))
                           (tags:name% (class-tag% exposed-class)))
              (base% exposed-class))
      (format stream "~a->addInstanceAsSubClass(~a::static_classSymbol());~%"
              (as-var-name (tags:namespace% (class-tag% exposed-class))
                           (tags:name% (class-tag% exposed-class)))
              (base% exposed-class))))
  (format stream "#endif // SET_BASES_ALL_CLASSES~%"))

(defun generate-calculate-class-precedence-all-classes (stream sorted-classes)
  (format stream "#ifdef CALCULATE_CLASS_PRECEDENCE_ALL_CLASSES~%")
  (dolist (exposed-class sorted-classes)
    (unless (string= (base% exposed-class) +root-dummy-class+)
      (format stream "~a->__setupStage3NameAndCalculateClassPrecedenceList(~a::~a::static_classSymbol());~%"
              (as-var-name (tags:namespace% (class-tag% exposed-class))
                           (tags:name% (class-tag% exposed-class)))
              (tags:namespace% (class-tag% exposed-class))
              (tags:name% (class-tag% exposed-class)))))
  (format stream "#endif //#ifdef CALCULATE_CLASS_PRECEDENCE_ALL_CLASSES~%"))

(defun generate-expose-static-class-variables (stream sorted-classes)
  (format stream "#ifdef EXPOSE_STATIC_CLASS_VARIABLES~%")
  (dolist (exposed-class sorted-classes)
    (let ((class-tag (class-tag% exposed-class)))
      (format stream "namespace ~a {
  gctools::Header_s::StampWtagMtag::Value ~a::static_ValueStampWtagMtag;
};~%"
              (tags:namespace% class-tag) (tags:name% class-tag))))
  (format stream "#endif // EXPOSE_STATIC_CLASS_VARIABLES~%"))

(defun generate-expose-instance-method (stream method class-tag)
  (format stream "// ~a~%" (file-line method))
  (let* ((lisp-name (lisp-name% method))
         (lambda-list (lambda-list% method))
         (declare-form (declare% method)))
    (format stream "        .def(~a,~a,R\"lambda(~a)lambda\",R\"decl(~a)decl\")~%"
            lisp-name
            (if (typep method 'expose-defmethod)
                (format nil "&~a::~a"
                        (tags:name% class-tag) (method-name% method))
                (pointer% method))
            (if (string/= lambda-list "")
                (format nil "(~a)" lambda-list)
                lambda-list)
            declare-form)))

(defun generate-expose-class-method (stream class-method class-tag)
  (if (typep class-method 'expose-def-class-method)
      (let* ((lisp-name (lisp-name% class-method))
             (class-name (tags:name% class-tag))
             (method-name (method-name% class-method))
             (lambda-list (lambda-list% class-method))
             (declare-form (declare% class-method)))
        (declare (ignore declare-form))
        (format stream " expose_function(~a,&~a::~a,R\"lambda(~a)lambda\");~%"
                lisp-name
                class-name
                method-name
                (maybe-wrap-lambda-list lambda-list)))))

(defun generate-expose-methods-batch (stream partition batch-num)
  (format stream "#ifdef BATCH~a~%" batch-num)
  (format stream "#ifdef EXPOSE_METHODS~%")
  (dolist (exposed-class partition)
    (let ((class-tag (class-tag% exposed-class)))
      (format stream "namespace ~a {
void ~a::expose_to_clasp() {
// ~a
    core::~:[class~;externalClass~]_<~a>()~%"
              (tags:namespace% class-tag) (tags:name% class-tag)
              (file-line exposed-class)
              (typep exposed-class 'exposed-external-class)
              (tags:name% class-tag))
      (dolist (method (methods% exposed-class))
        (generate-expose-instance-method stream method class-tag))
      (format stream "     ;~%")
      (dolist (class-method (class-methods% exposed-class))
        (generate-expose-class-method stream class-method class-tag))
      (format stream "}~%")
      (format stream "};~%")))
  (format stream "#endif // EXPOSE_METHODS~%")
  (format stream "#ifdef EXPOSE_CLASSES_AND_METHODS~%")
  (dolist (exposed-class partition)
    (format stream "~a::~a::expose_to_clasp();~%"
            (tags:namespace% (class-tag% exposed-class))
            (tags:name% (class-tag% exposed-class))))
  (format stream "#endif //#ifdef EXPOSE_CLASSES_AND_METHODS~%")
  (format stream "#endif // BATCH~a~%" batch-num))

(defun generate-expose-methods (stream sorted-classes)
  (let ((partitions (partition-list sorted-classes *batch-classes*)))
    (loop for partition in partitions
          for batch-num from 1
          do (generate-expose-methods-batch stream partition batch-num))))

(defun generate-code-for-init-classes-and-methods
    (exposed-classes gc-managed-types)
  (declare (optimize (speed 0) (debug 3)))
  (with-output-to-string (sout)
    (multiple-value-bind (sorted-classes inheritance)
        (sort-classes-by-inheritance exposed-classes)
      (let (cur-package)
        (declare (ignorable cur-package))
        (generate-declare-forwards sout exposed-classes)
        (generate-declare-inheritance sout sorted-classes inheritance)
        (let ((sorted-classes (let (sc)
                                (maphash (lambda (key class)
                                           (declare (ignore key))
                                           (push class sc))
                                         exposed-classes)
                                (sort sc #'< :key #'stamp%))))
          (generate-gc-enum sout sorted-classes gc-managed-types)
          (generate-gc-enum-names sout sorted-classes gc-managed-types))
        (generate-gc-stamp-selectors sout sorted-classes gc-managed-types)
        (generate-gc-dynamic-cast sout sorted-classes)
        (generate-gc-typeq sout sorted-classes)
        (generate-code-for-init-class-kinds exposed-classes sout)
        (generate-code-for-init-classes-class-symbols exposed-classes sout)
        (generate-allocate-all-classes sout sorted-classes)
        (generate-set-bases-all-classes sout sorted-classes)
        (generate-calculate-class-precedence-all-classes sout sorted-classes)
        (generate-expose-static-class-variables sout sorted-classes)
        (generate-expose-methods sout sorted-classes)))))

(defun generate-bootstrap-packages (stream packages-to-create)
  (format stream "#if defined(BOOTSTRAP_PACKAGES)~%")
  (mapc (lambda (pkg)
          (format stream "{
  std::list<std::string> use_packages = {};
  bootStrapSymbolMap->add_package_info(~s,use_packages);
}~%"
                  (name% pkg)))
        packages-to-create)
  (format stream "#endif // #if defined(BOOTSTRAP_PACKAGES)~%"))

(defun generate-create-all-packages (stream packages-to-create)
  (format stream "#if defined(CREATE_ALL_PACKAGES)~%")
  (mapc (lambda (pkg)
          (format stream "{
  std::list<std::string> nicknames = {~{ ~s~^, ~}};
  std::list<std::string> use_packages = {~{ ~s~^, ~}};
  std::list<std::string> shadow = {~{ ~s~^, ~}};
  _lisp->finishPackageSetup(~s,nicknames,use_packages,shadow);
}~%"
                  (nicknames% pkg) (packages-to-use% pkg)
                  (shadow% pkg) (name% pkg)))
        packages-to-create)
  #+(or)
  (mapc (lambda (pkg)
          (when (packages-to-use% pkg)
            (mapc (lambda (use)
                    (format stream "  gc::As<core::Package_sp>(_lisp->findPackage(~s))->usePackage(gc::As<core::Package_sp>(_lisp->findPackage(~s)));~%" (name% pkg) use))
                  (packages-to-use% pkg))))
        packages-to-create)
  (format stream "#endif // CREATE_ALL_PACKAGES~%"))

(defun generate-declare-all-symbols (stream symbols-by-package
                                     symbols-by-namespace)
  (let ((symbol-count 0) (symbol-index 0))
    (maphash (lambda (key symbols)
               (declare (ignore key))
               (setf symbol-count (+ symbol-count (length symbols))))
             symbols-by-package)
    (format stream "#if defined(DECLARE_ALL_SYMBOLS)~%")
    (format stream "int global_symbol_count = ~d;~%" symbol-count)
    (format stream "core::Symbol_sp global_symbols[~d];~%" symbol-count)
    (maphash (lambda (namespace namespace-symbols)
               (format stream "namespace ~a {~%" namespace)
               (dolist (symbol namespace-symbols)
                 (format stream "core::Symbol_sp& _sym_~a = global_symbols[~d];~%"
                         (c++-name% symbol)
                         (1- (incf symbol-index))))
               (format stream "} // namespace ~a~%" namespace))
             symbols-by-namespace)
    (format stream "#endif // DECLARE_ALL_SYMBOLS~%")))

(defun generate-extern-all-symbols (stream symbols-by-namespace)
  (format stream "#if defined(EXTERN_ALL_SYMBOLS)~%")
  (maphash (lambda (namespace namespace-symbols)
             (format stream "namespace ~a {~%" namespace)
             (dolist (symbol namespace-symbols)
               (format stream "extern core::Symbol_sp& _sym_~a;~%"
                       (c++-name% symbol)))
             (format stream "} // namespace ~a~%" namespace))
           symbols-by-namespace)
  (format stream "#endif // EXTERN_ALL_SYMBOLS~%"))

(defun generate-code-for-symbols (packages-to-create symbols)
  (declare (optimize (speed 3)))
  ;; Uniqify the symbols
  (with-output-to-string (sout)
    (let ((symbols-by-package (make-hash-table :test #'equal))
          (symbols-by-namespace (make-hash-table :test #'equal))
          (index 0))
      ;; Organize symbols by package
      (dolist (symbol symbols)
        (pushnew symbol
                 (gethash (package% symbol) symbols-by-package)
                 :test #'string=
                 :key (lambda (x)
                        (c++-name% x)))
        (pushnew symbol
                 (gethash (namespace% symbol) symbols-by-namespace)
                 :test #'string=
                 :key (lambda (x)
                        (c++-name% x))))
      (generate-bootstrap-packages sout packages-to-create)
      (generate-create-all-packages sout packages-to-create)
      (generate-declare-all-symbols sout symbols-by-package
                                    symbols-by-namespace)
      (generate-extern-all-symbols sout symbols-by-namespace)
      (let ((helpers (make-hash-table :test #'equal))
            (index 0))
        (format sout "#if defined(ALLOCATE_ALL_SYMBOLS_HELPERS)~%")
        (dolist (p packages-to-create)
          (maphash (lambda (namespace namespace-symbols)
                     (dolist (symbol namespace-symbols)
                       (when (string= (name% p) (package-str% symbol))
                         (let ((helper-name (format nil "maybe_allocate_one_symbol_~d_helper" (incf index)))
                               (symbol-name (format nil "~a::_sym_~a" namespace (c++-name% symbol))))
                           (setf (gethash symbol-name helpers) helper-name)
                           (format sout "NOINLINE void ~a(core::BootStrapCoreSymbolMap* symbols) {~%" helper-name)
                           (format sout " ~a = symbols->maybe_allocate_unique_symbol(\"~a\",core::lispify_symbol_name(~s), ~a,~a);~%"
                                   symbol-name
                                   (package-str% symbol)
                                   (lisp-name% symbol)
                                   (if (exported% symbol) "true" "false")
                                   (if (shadow% symbol) "true" "false"))
                           (format sout "}~%")))))
                   symbols-by-namespace))
        (format sout "#endif // ALLOCATE_ALL_SYMBOLS_HELPERS~%")
        (format sout "#if defined(ALLOCATE_ALL_SYMBOLS)~%")
        (maphash (lambda (symbol-name helper-name)
                   (declare (ignore symbol-name))
                   (format sout " ~a(symbols);~%" helper-name))
                 helpers)
        (format sout "#endif // ALLOCATE_ALL_SYMBOLS~%"))
      #+(or)(progn
              (format sout "#if defined(GARBAGE_COLLECT_ALL_SYMBOLS)~%")
              (maphash (lambda (namespace namespace-symbols)
                         (dolist (symbol namespace-symbols)
                           (format sout "SMART_PTR_FIX(~a::_sym_~a);~%"
                                   namespace
                                   (c++-name% symbol))))
                       symbols-by-namespace)
              (format sout "#endif~% // defined(GARBAGE_COLLECT_ALL_SYMBOLS~%"))
      (progn
        (maphash (lambda (package package-symbols)
                   (format sout "#if defined(~a_SYMBOLS)~%" package)
                   (dolist (symbol package-symbols)
                     (format sout "DO_SYMBOL(~a,_sym_~a,~d,~a,~s,~a);~%"
                             (namespace% symbol)
                             (c++-name% symbol)
                             index
                             (package% symbol)
                             (lisp-name% symbol)
                             (if (typep symbol 'expose-internal-symbol)
                                 "false"
                                 "true"))
                     (incf index))
                   (format sout "#endif // ~a_SYMBOLS~%" package))
                 symbols-by-package)))))

(defun generate-code-for-enums (enums)
  (declare (optimize (speed 3)))
  ;; Uniqify the symbols
  (with-output-to-string (sout)
    (format sout "#ifdef ALL_ENUMS~%")
    (dolist (e enums)
      (format sout "core::enum_<~a>(~a,~s)~%"
              (type% (begin-enum% e))
              (symbol% (begin-enum% e))
              (description% (begin-enum% e)))
      (dolist (value (values% e))
        (format sout "  .value(~a,~a)~%"
                (symbol% value)
                (value% value)))
      (format sout ";~%"))
    (format sout "#endif //ifdef ALL_ENUMS~%")))

(defun generate-code-for-startups (startups)
  (declare (optimize (speed 3)))
  (let ((startups-by-namespace (make-hash-table :test #'equal)))
    (dolist (i startups)
      (push i (gethash (namespace% i) startups-by-namespace)))
    (with-output-to-string (sout)
      (format sout "#ifdef ALL_STARTUPS_EXTERN~%")
      (maphash (lambda (ns init-list)
                 (format sout "namespace ~a {~%" ns)
                 (dolist (ii init-list)
                   (format sout "   extern void ~a();~%" (function-name% ii)))
                 (format sout "};~%"))
               startups-by-namespace)
      (format sout "#endif // ALL_STARTUPS_EXTERN~%")
      (format sout "#ifdef ALL_STARTUPS_CALLS~%")
      (maphash (lambda (ns init-list)
                 (declare (ignore ns))
                 (dolist (ii init-list)
                   (format sout "    ~a::~a();~%" (namespace% ii) (function-name% ii))))
               startups-by-namespace)
      (format sout "#endif // ALL_STARTUPS_CALL~%"))))

(defun generate-code-for-initializers (initializers)
  (declare (optimize (speed 3)))
  (let ((initializers-by-namespace (make-hash-table :test #'equal)))
    (dolist (i initializers)
      (push i (gethash (namespace% i) initializers-by-namespace)))
    (with-output-to-string (sout)
      (format sout "#ifdef ALL_INITIALIZERS_EXTERN~%")
      (maphash (lambda (ns init-list)
                 (format sout "namespace ~a {~%" ns)
                 (dolist (ii init-list)
                   (format sout "   extern void ~a();~%" (function-name% ii)))
                 (format sout "};~%"))
               initializers-by-namespace)
      (format sout "#endif // ALL_INITIALIZERS_EXTERN~%")
      (format sout "#ifdef ALL_INITIALIZERS_CALLS~%")
      (maphash (lambda (ns init-list)
                 (declare (ignore ns))
                 (dolist (ii init-list)
                   (format sout "    ~a::~a();~%" (namespace% ii) (function-name% ii))))
               initializers-by-namespace)
      (format sout "#endif // ALL_INITIALIZERS_CALL~%"))))

(defun generate-code-for-exposes (exposes)
  (declare (optimize (speed 3)))
  (let ((exposes-by-namespace (make-hash-table :test #'equal)))
    (dolist (i exposes)
      (push i (gethash (namespace% i) exposes-by-namespace)))
    (with-output-to-string (sout)
      (format sout "#ifdef ALL_EXPOSES_EXTERN~%")
      (maphash (lambda (ns init-list)
                 (format sout "namespace ~a {~%" ns)
                 (dolist (ii init-list)
                   (format sout "   extern void ~a();~%" (function-name% ii)))
                 (format sout "};~%"))
               exposes-by-namespace)
      (format sout "#endif // ALL_EXPOSES_EXTERN~%")
      (format sout "#ifdef ALL_EXPOSES_CALLS~%")
      (maphash (lambda (ns init-list)
                 (declare (ignore ns))
                 (dolist (ii init-list)
                   (format sout "    ~a::~a();~%" (namespace% ii) (function-name% ii))))
               exposes-by-namespace)
      (format sout "#endif // ALL_EXPOSES_CALL~%"))))

(defun generate-code-for-terminators (terminators)
  (declare (optimize (speed 3)))
  (let ((terminators-by-namespace (make-hash-table :test #'equal)))
    (dolist (i terminators)
      (push i (gethash (namespace% i) terminators-by-namespace)))
    (with-output-to-string (sout)
      (format sout "#ifdef ALL_TERMINATORS_EXTERN~%")
      (maphash (lambda (ns init-list)
                 (format sout "namespace ~a {~%" ns)
                 (dolist (ii init-list)
                   (format sout "   extern void ~a();~%" (function-name% ii)))
                 (format sout "};~%"))
               terminators-by-namespace)
      (format sout "#endif // ALL_TERMINATORS_EXTERN~%")
      (format sout "#ifdef ALL_TERMINATORS_CALLS~%")
      (maphash (lambda (ns init-list)
                 (declare (ignore ns))
                 (dolist (ii init-list)
                   (format sout "    ~a::~a();~%" (namespace% ii) (function-name% ii))))
               terminators-by-namespace)
      (format sout "#endif // ALL_TERMINATORS_CALL~%"))))

(defun generate-layout-code (kind stream)
  (dolist (field (fixed-fields% kind))
    (format stream " {  fixed_field, ~a, sizeof(~a), __builtin_offsetof(SAFE_TYPE_MACRO(~a),~a), 0, \"~a\" },~%"
            (tags:offset-type-cxx-identifier field)
            (tags:offset-ctype field)
            (tags:offset-base-ctype field)
            (tags:layout-offset-field-names field)
            (tags:layout-offset-field-names field)))
  (let ((vinfo (variable-info% kind))
        (vcapacity (variable-capacity% kind))
        (vfields (variable-fields% kind)))
    (when vinfo
      (etypecase vinfo
        (tags:variable-bit-array0
         (format stream " {  variable_bit_array0, ~a, 0, __builtin_offsetof(SAFE_TYPE_MACRO(~a),~{~a~}), 0, \"~{~a~}\" },~%"
                 (tags:integral-value vinfo)
                 (tags:offset-base-ctype vinfo)
                 (tags:field-names vinfo) (tags:field-names vinfo)))
        (tags:variable-array0
         (format stream " {  variable_array0, 0, 0, __builtin_offsetof(SAFE_TYPE_MACRO(~a),~{~a~}), 0, \"~{~a~}\" },~%"
                 (tags:offset-base-ctype vinfo)
                 (tags:field-names vinfo) (tags:field-names vinfo))))
      (format stream " {  variable_capacity, sizeof(~a), __builtin_offsetof(SAFE_TYPE_MACRO(~a),~{~a~}), __builtin_offsetof(SAFE_TYPE_MACRO(~a),~{~a~}), 0, NULL },~%"
              (tags:ctype vcapacity)
              (tags:offset-base-ctype vcapacity)
              (tags:end-field-names vcapacity)
              (tags:offset-base-ctype vcapacity)
              (tags:length-field-names vcapacity))
      (etypecase vfields
        (tags:variable-field-only
         (format stream "{    variable_field, ~a, sizeof(~a), 0, 0, \"only\" },~%"
                 (tags:offset-type-cxx-identifier vfields)
                 (tags:fixup-type vfields)))
        (list
         (dolist (vfield vfields)
           (format stream "    {    variable_field, ~a, sizeof(~a), __builtin_offsetof(SAFE_TYPE_MACRO(~a),~a), 0, \"~a\" },~%"
                   (tags:offset-type-cxx-identifier vfield)
                   (tags:fixup-ctype-offset-type-key vfield)
                   (tags:fixup-ctype-key vfield)
                   (tags:layout-offset-field-names vfield)
                   (tags:layout-offset-field-names vfield))))))))

(defgeneric generate-kind-tag-code (kind stream))
(defmethod generate-kind-tag-code ((kind tags:class-kind) stream)
  (format stream "{ class_kind, ~a, sizeof(~a), 0, ~a, \"~a\" },~%"
          (tags:stamp-name kind) (tags:stamp-key kind)
          (tags:definition-data kind) (tags:stamp-key kind)))
(defmethod generate-kind-tag-code ((kind tags:container-kind) stream)
  (format stream "{ container_kind, ~a, sizeof(~a), 0, ~a, \"~a\" },~%"
          (tags:stamp-name kind) (tags:stamp-key kind)
          (tags:definition-data kind) (tags:stamp-key kind)))
(defmethod generate-kind-tag-code ((kind tags:bitunit-container-kind) stream)
  (format stream "{ bitunit_container_kind, ~a, sizeof(~a), ~a, ~a, \"~a\" },~%"
          (tags:stamp-name kind) (tags:stamp-key kind)
          (tags:bitwidth kind) (tags:definition-data kind)
          (tags:stamp-key kind)))
(defmethod generate-kind-tag-code ((kind tags:templated-kind) stream)
  (format stream "{ templated_kind, ~a, sizeof(~a), 0, ~a, \"~a\" },~%"
          (tags:stamp-name kind) (tags:stamp-key kind)
          (tags:definition-data kind) (tags:stamp-key kind)))

(defun generate-kind-code (kind stream)
  (generate-kind-tag-code (tag% kind) stream)
  (generate-layout-code kind stream))

(defun generate-gc-obj-scan (stream classes gc-managed-types)
  (format stream "#if defined(GC_OBJ_SCAN)
#endif // defined(GC_OBJ_SCAN)
#if defined(GC_OBJ_SCAN_HELPERS)~%")
  (loop for kind in classes do (generate-kind-code kind stream))
  (loop for kind in gc-managed-types do (generate-kind-code kind stream))
  (format stream "#endif // defined(GC_OBJ_SCAN_HELPERS)~%"))

(defconstant +ptr-name+
  (if (boundp '+ptr-name+) ; avoid redefinition warnings
      (symbol-value '+ptr-name+)
      "obj_gc_safe")
  "This variable is used to temporarily hold a pointer to a Wrapper<...> object - we want the GC to ignore it")

(defgeneric %generate-finalizer (stream kind tag)
  (:argument-precedence-order tag kind stream))
(defun generate-finalizer (stream kind)
  (%generate-finalizer stream kind (tag% kind)))

(defun strip-all-namespaces-from-name (name)
  (let ((pos (search "::" name :from-end t)))
    (if pos
        (subseq name (+ 2 pos)) ; +2 for the :: itself
        name)))

(defun generate-finalizer-for-lispalloc (stream kind)
  (format stream "obj_finalize_STAMPWTAG_~a:
{
    // stamp value ~a
    ~a* ~a = reinterpret_cast<~a*>(client);
#pragma clang diagnostic push
#pragma clang diagnostic ignored \"-Wignored-qualifiers\"
    ~a->~~~a();
#pragma clang diagnostic pop
    goto finalize_done;
}~%"
          (build-enum-name (class-key% kind)) (stamp-value% kind)
          (class-key% kind) +ptr-name+ (class-key% kind)
          +ptr-name+ (strip-all-namespaces-from-name (class-key% kind))))

(defmethod %generate-finalizer (stream kind (tag tags:class-kind))
  (generate-finalizer-for-lispalloc stream kind))
(defmethod %generate-finalizer (stream kind (tag tags:templated-kind))
  (generate-finalizer-for-lispalloc stream kind))

(defun generate-error-finalizer (stream kind)
  (format stream "obj_finalize_STAMPWTAG_~a:
{
    // stamp value ~a
    THROW_HARD_ERROR(BF(\"Should never finalize ~a\"));
}~%"
          (build-enum-name (class-key% kind)) (stamp-value% kind)
          (class-key% kind)))

(defmethod %generate-finalizer (stream kind (tag tags:container-kind))
  (generate-error-finalizer stream kind))
(defmethod %generate-finalizer (stream kind (tag tags:bitunit-container-kind))
  (generate-error-finalizer stream kind))

(defun generate-obj-finalize (stream classes gc-managed-types)
  (format stream "#if defined(GC_OBJ_FINALIZE)~%")
  (loop for k in classes do (generate-finalizer stream k))
  (loop for k in gc-managed-types do (generate-finalizer stream k))
  (format stream "#endif // defined(GC_OBJ_FINALIZE)
#if defined(GC_OBJ_FINALIZE_HELPERS)
#endif // defined(GC_OBJ_FINALIZE_HELPERS)~%"))

(defun generate-finalizer-table-entry (stream kind)
  (format stream "  /* ~d */ &&obj_finalize_STAMPWTAG_~a,~%"
          (stamp-value kind) (build-enum-name (class-key% kind))))

(defun generate-finalize-table (stream classes gc-managed-types)
  (format stream "#if defined(GC_OBJ_FINALIZE_TABLE)
static void* OBJ_FINALIZE_table[] = {~%")
  (loop for k in classes do (generate-finalizer-table-entry stream k))
  (loop for k in gc-managed-types do (generate-finalizer-table-entry stream k))
  (format stream "   NULL
};
#endif // defined(GC_OBJ_FINALIZE_TABLE)~%"))

(defgeneric %generate-deallocator (stream kind tag)
  (:argument-precedence-order tag kind stream))
(defun generate-deallocator (stream kind)
  (%generate-deallocator stream kind (tag% kind)))

(defun generate-deallocator-for-lispalloc (stream kind)
  (format stream "obj_deallocate_unmanaged_instance_STAMPWTAG_~a:
{
    // stamp value ~a
    ~a* ~a = reinterpret_cast<~a*>(client);
    GC<~a>::deallocate_unmanaged_instance(~a);
    return;
}~%"
          (build-enum-name (class-key% kind)) (stamp-value% kind)
          (class-key% kind) +ptr-name+ (class-key% kind)
          (class-key% kind) +ptr-name+))

(defmethod %generate-deallocator (stream kind (tag tags:class-kind))
  (generate-deallocator-for-lispalloc stream kind))
(defmethod %generate-deallocator (stream kind (tag tags:templated-kind))
  (generate-deallocator-for-lispalloc stream kind))

(defun generate-error-deallocator (stream kind)
  (format stream "obj_deallocate_unmanaged_instance_STAMPWTAG_~a:
{
    // do nothing stamp value ~a
    THROW_HARD_ERROR(BF(\"Should never deallocate object ~a\"));
}~%"
          (build-enum-name (class-key% kind)) (stamp-value% kind)
          (class-key% kind)))

(defmethod %generate-deallocator (stream kind (tag tags:container-kind))
  (generate-error-deallocator stream kind))
(defmethod %generate-deallocator (stream kind (tag tags:bitunit-container-kind))
  (generate-error-deallocator stream kind))

(defun generate-obj-deallocator (stream classes gc-managed-types)
  (format stream "#if defined(GC_OBJ_DEALLOCATOR)~%")
  (loop for k in classes do (generate-deallocator stream k))
  (loop for k in gc-managed-types do (generate-deallocator stream k))
  (format stream "#endif // defined(GC_OBJ_DEALLOCATOR)
#if defined(GC_OBJ_DEALLOCATOR_HELPERS)
#endif // defined(GC_OBJ_DEALLOCATOR_HELPERS)~%"))

(defun generate-deallocator-table-entry (stream kind)
  (format stream
          "  /* ~d */ &&obj_deallocate_unmanaged_instance_STAMPWTAG_~a,~%"
          (stamp-value kind) (build-enum-name (class-key% kind))))

(defun generate-deallocator-table (stream classes gc-managed-types)
  (format stream "#if defined(GC_OBJ_DEALLOCATOR_TABLE)
static void* OBJ_DEALLOCATOR_table[] + {~%")
  (loop for k in classes do (generate-deallocator-table-entry stream k))
  (loop for k in gc-managed-types
        do (generate-deallocator-table-entry stream k))
  (format stream "#endif // defined(GC_OBJ_DEALLOCATOR_TABLE)~%"))

(defun generate-gc-globals (stream)
  (format stream "#if defined(GC_GLOBALS)
 TAGGED_POINTER_FIX(_lisp);
#endif // defined(GC_GLOBALS)~%"))

(defun maybe-relative (dir)
  (cond
    ((eq (car dir) :absolute) (list* :relative (cdr dir)))
    ((eq (car dir) :relative) dir)
    (t (error "How do I convert this to a relative directory path: ~a" dir))))

(defun write-if-changed (code main-path app-relative)
  (let ((pn (merge-pathnames
             (make-pathname :name (pathname-name app-relative)
                            :type (pathname-type app-relative)
                            :directory (maybe-relative (pathname-directory app-relative)))
             (pathname main-path))))
    (ensure-directories-exist pn)
    (let ((data-in-file (when (probe-file pn)
                          (with-open-file (stream pn :direction :input :external-format :utf-8)
                            (let ((data (make-string (file-length stream))))
                              (read-sequence data stream)
                              data)))))
      (if (string= data-in-file code)
          (format t   "| -------- | There are no changes to ~a.~%" pn)
          (progn
            (format t "| UPDATING | There are changes to ~a.~%" pn)
            (let ((opn (make-pathname :type "bak" :defaults pn)))
              (when (probe-file opn)
                (delete-file opn))
              (when (probe-file pn)
                (rename-file pn opn)))
            (with-open-file (stream pn :direction :output :if-exists :supersede :external-format :utf-8)
              (write-sequence code stream)))))))

(defun safe-app-config (key app-config)
  (let ((value (gethash key app-config)))
    (unless value
      (error "Could not get key: ~s from app-config" key))
    value))

(defun generate-gc-code  (classes gc-managed-types)
  (with-output-to-string (s)
    (generate-declare-forwards s classes)
    (multiple-value-bind (sorted-classes inheritance)
        (sort-classes-by-inheritance classes)
      (declare (ignore inheritance))
      (generate-gc-enum s sorted-classes gc-managed-types)
      (generate-gc-enum-names s sorted-classes gc-managed-types)
      (generate-gc-dynamic-cast s sorted-classes)
      (generate-gc-typeq s sorted-classes)
      (generate-gc-stamp-selectors s sorted-classes gc-managed-types)
      (generate-gc-obj-scan s sorted-classes gc-managed-types)
      (generate-obj-finalize s sorted-classes gc-managed-types)
      (generate-finalize-table s sorted-classes gc-managed-types)
      (generate-obj-deallocator s sorted-classes gc-managed-types)
      (generate-deallocator-table s sorted-classes gc-managed-types)
      (generate-gc-globals s))))

(defun generate-code (packages-to-create functions symbols classes gc-managed-types enums startups initializers exposes terminators build-path app-config)
  (let ((init-functions (generate-code-for-init-functions functions))
        (init-classes-and-methods (generate-code-for-init-classes-and-methods classes gc-managed-types))
        (source-info (generate-code-for-source-info functions classes))
        (symbol-info (generate-code-for-symbols packages-to-create symbols))
        (enum-info (generate-code-for-enums enums))
        (startups-info (generate-code-for-startups startups))
        (initializers-info (generate-code-for-initializers initializers))
        (exposes-info (generate-code-for-exposes exposes))
        (terminators-info (generate-code-for-terminators terminators)))
    (write-if-changed init-functions build-path (safe-app-config :init_functions_inc_h app-config))
    (write-if-changed init-classes-and-methods build-path (safe-app-config :init_classes_inc_h app-config))
    (write-if-changed source-info build-path (safe-app-config :source_info_inc_h app-config))
    (write-if-changed symbol-info build-path (safe-app-config :symbols_scraped_inc_h app-config))
    (write-if-changed enum-info build-path (safe-app-config :enum_inc_h app-config))
    (write-if-changed startups-info build-path (safe-app-config :pregcstartup_inc_h app-config))
    (write-if-changed initializers-info build-path (safe-app-config :initializers_inc_h app-config))
    (write-if-changed exposes-info build-path (safe-app-config :expose_inc_h app-config))
    (write-if-changed terminators-info build-path (safe-app-config :terminators_inc_h app-config))
    (multiple-value-bind (direct-call-c-code direct-call-cl-code c-code-info cl-code-info)
        (generate-code-for-direct-call-functions functions)
      (write-if-changed direct-call-c-code build-path (safe-app-config :c_wrappers app-config))
      (write-if-changed direct-call-cl-code build-path (safe-app-config :lisp_wrappers app-config))
      (write-if-changed c-code-info build-path (merge-pathnames (make-pathname :type "txt") (safe-app-config :c_wrappers app-config)))
      (write-if-changed cl-code-info build-path (merge-pathnames (make-pathname :type "txt") (safe-app-config :lisp_wrappers app-config))))))
