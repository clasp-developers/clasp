;;;
;;; Any symbols we want to export from EXT must be done in init.lsp
;;;

(in-package :core)
;;; Temporary alias for SLIME compatibility- remove as soon as possible
(setf (fdefinition 'function-lambda-list) #'ext:function-lambda-list)
(export 'function-lambda-list)

(in-package :ext)

(defun compiled-function-name (x)
  (core:function-name x))

(defun compiled-function-file (xfunction)
  (if (and xfunction (functionp xfunction))
      (multiple-value-bind (src-pathname pos lineno)
          (core:function-source-pos xfunction)
        (when (null src-pathname)
          ;; e.g., a repl function - no source location.
          (return-from compiled-function-file nil))
        ;; FIXME: This indicates an internal bookkeeping problem, i.e., a bug.
        (unless (typep src-pathname 'pathname)
          (error "The source-debug-pathname for ~a was ~a - it needs to be a pathname"
                 xfunction src-pathname))
        (let ((src-directory (pathname-directory src-pathname))
              (src-name (pathname-name src-pathname))
              (src-type (pathname-type src-pathname))
          (filepos pos))
          (let ((pn (if (eq (car src-directory) :relative)
                        (merge-pathnames src-pathname (translate-logical-pathname "source-dir:"))
                        src-pathname)))
            (values pn filepos lineno))))
      (progn
        (warn "compiled-function-file expected a function as argument - but it got ~a - there may not be any backtrace available" xfunction)
        (values nil 0 0))))

;;; This happens to be the first non-:TYPE defstruct during build.
;;; But FIXME: It's redundant to core:source-pos-info and should be vaporized.
(defstruct source-location
  pathname offset)

(defun function-source-locations (function)
  (multiple-value-bind (file pos)
      (compiled-function-file function)
    (if file
        (list (make-source-location :pathname file :offset pos))
        nil)))

;;; Class source positions are just stored in a slot.
(defun class-source-location (class)
  (let ((csp (clos:class-source-position class)))
    (when csp
      ;; FIXME: Move this source debug stuff to an interface
      ;; (in SPI, probably)
      (let ((csi (core:file-scope
                  (core:source-pos-info-file-handle csp))))
        (make-source-location
         :pathname (core:file-scope-source-debug-pathname csi)
         :offset (+ (core:file-scope-source-debug-offset csi)
                    (core:source-pos-info-filepos csp)))))))

;;; Method combinations don't have source positions. In fact,
;;; they don't even exist as objects globally. The only global
;;; is the "compiler", which is an ordinary function that does
;;; the computation. find-method-combination always makes a fresh
;;; object.
;;; As such, we use the compiler's source info.
;;; Note that due to this, when provided a name, we don't go through
;;; this function- check source-location-impl.
(defun method-combination-source-location (method-combination)
  (source-location (clos::method-combination-compiler method-combination) t))

(defun method-source-location (method)
  ;; NOTE: Because we return this to MAPCON, make sure the lists are fresh.
  (source-location (clos:method-function method) t))

(defun generic-function-source-locations (gf)
  ;; FIXME: Include the actual defgeneric's location too.
  (mapcan #'method-source-location (clos:generic-function-methods gf)))

(defun source-location-impl (name kind)
  "* Arguments
- name : A symbol.
- kind : A symbol (:function :method :class)
Return the source-location for the name/kind pair"
  (labels ((fix-paths-and-make-source-locations (rels)
             (declare (core:lambda-name 'fix-paths-and-make-source-locations))
             (let ((source-dir (translate-logical-pathname #P"source-dir:")))
               (mapcar (lambda (dir-pos)
                         (let ((dir (first dir-pos))
                               (pos (second dir-pos)))
                           (make-source-location :pathname (merge-pathnames dir source-dir)
                                                 :offset pos)))
                       rels))))
    (case kind
      (:class
       (let ((class (find-class name nil)))
         (when class
           (let ((source-loc (class-source-location class)))
             (when source-loc (list source-loc))))))
      (:method
          (let ((source-loc (core:get-sysprop name 'core:cxx-method-source-location)))
            (fix-paths-and-make-source-locations source-loc)))
      (:function
       (when (fboundp name)
         (let ((func (fdefinition name)))
           (cond ((core:single-dispatch-generic-function-p func)
                  (source-location name :method))
                 ((typep func 'generic-function)
                  (generic-function-source-locations func))
                 (t ; normal function
                  (function-source-locations (fdefinition name)))))))
      (:compiler-macro
       (when (fboundp name)
         (let ((cmf (compiler-macro-function name)))
           (when cmf
             (source-location cmf t)))))
      (:setf-expander
       (let ((expander (ext:setf-expander name)))
         (when expander
           (source-location expander t))))
      (:method-combination
       ;; See comment on method-combination-source-position
       (let ((method-combination-compiler (clos::search-method-combination name)))
         (when method-combination-compiler
           (source-location method-combination-compiler t))))
      (:type
       ;; We use the source location of the expander function.
       (let ((expander (ext:type-expander name)))
         (when expander
           (source-location expander t)))))))

(defparameter *source-location-kinds* '(:class :method :function :compiler-macro
                                        :method-combination :type :setf-expander))

(defun source-location (obj kind)
  "* Arguments
- obj : A symbol or object.
- kind : A symbol (:function :method :class t)
Return the source-location for the name/kind pair"
  (cond
    ((eq kind t)
     (cond
       ((clos:classp obj)
        (let ((source-loc (class-source-location obj)))
          (when source-loc (list source-loc))))
       ((core:single-dispatch-generic-function-p obj)
        (source-location (core:function-name obj) :method))
       ((typep obj 'generic-function)
        (generic-function-source-locations obj))
       ((functionp obj) (function-source-locations obj))
       ((typep obj 'clos:method-combination)
        (let ((source-loc (method-combination-source-location obj)))
          (when source-loc (list source-loc))))))
    ((symbolp kind) (source-location-impl obj kind))
    (t (error "Cannot obtain source-location for ~a of kind ~a" obj kind))))
