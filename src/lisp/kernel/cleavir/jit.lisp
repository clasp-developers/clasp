(in-package #:clasp-cleavir)

(defun escape-and-join-jit-name (names)
  (with-output-to-string (all)
    (dolist (name names)
      (loop for c across name
            do (case c
                 ((#\\) (write-string "\\\\" all))
                 ((#\^) (write-string "\\^" all))
                 (otherwise (write-char c all))))
      (write-char #\^ all))
    (write-char #\^ all)))

;;; FIXME: Get the filename from source-pos-info also.
(defun function-name-from-source-info (lname)
  (declare (ignore lname))
  (if core:*current-source-pos-info*
      (let ((lineno (core:source-pos-info-lineno *current-source-pos-info*)))
        (cond
          (*compile-file-pathname*
           (core:fmt nil "___LAMBDA___{}.{}-{}^{}^{}"
                         (pathname-name *compile-file-pathname*)
                         (pathname-type *compile-file-pathname*)
                         cmp:*compile-file-unique-symbol-prefix*
                         lineno
                         (sys:next-number)))
          ;; Is this even possible?
          (*load-pathname*
           (core:fmt nil "{}.{}^{}^TOP-LOAD-{}"
                         (pathname-name *load-pathname*)
                         (pathname-type *load-pathname*)
                         lineno
                         (sys:next-number)))
          (t
           (core:fmt nil "UNKNOWN^{}^TOP-UNKNOWN" lineno))))
      "UNKNOWN??LINE^TOP-UNKNOWN"))

(defun jit-function-name (lname)
  "Depending on the type of LNAME an actual LLVM name is generated"
  (typecase lname
    (pathname
     (format nil "MAIN-~a" (string-upcase (pathname-name lname))))
    (string
     (cond
       ((string= lname core:+run-all-function-name+) lname) ; this one is ok
       ((string= lname core:+clasp-ctor-function-name+) lname) ; this one is ok
       ((string= lname "IMPLICIT-REPL") lname)  ; this one is ok
       ((string= lname "TOP-LEVEL")
        (function-name-from-source-info lname))
       ((string= lname "UNNAMED-LAMBDA") lname) ; this one is ok
       ((string= lname "lambda") lname)         ; this one is ok
       ((string= lname "ltv-literal") lname)    ; this one is ok
       ((string= lname "disassemble") lname)    ; this one is ok
       (t lname)))
    (symbol
     (case lname
       ((core::top-level cmp::repl)
        (function-name-from-source-info lname))
       (otherwise
        (let* ((sym-pkg (symbol-package lname))
               (sym-name (symbol-name lname))
               (pkg-name (if sym-pkg
                             (string (package-name sym-pkg))
                             ;; KNPK I don't undestand why "KEYWORD" is used here
                             ;; (package-name (symbol-package :test)) -> "KEYWORD", so how can sym-pkg be empty for this case
                             ;; More likely it is an uninterned symbol
                             "KEYWORD")))
          (escape-and-join-jit-name (list sym-name pkg-name "FN"))))))
    ;; (SETF symbol)
    ((cons (eql setf) (cons symbol))
     (let* ((sn (cadr lname))
            (sym-pkg (symbol-package sn))
            (sym-name (symbol-name sn))
            (pkg-name (if sym-pkg
                          (string (package-name sym-pkg))
                          "KEYWORD")))
       (escape-and-join-jit-name (list sym-name pkg-name "SETF"))))
    ;; (SETF (symbol ...))
    ((cons (eql setf) (cons cons))
     (let* ((sn (second lname))
            (sn-sym (first sn))
            (sym-pkg (symbol-package sn-sym))
            (sym-name (symbol-name sn-sym))
            (pkg-name (if sym-pkg
                          (string (package-name sym-pkg))
                          "KEYWORD")))
       (escape-and-join-jit-name (list sym-name pkg-name "SETFCONS"))))
    ;; (METHOD symbol . specializer-list): a method function
    ((cons (eql method) (cons symbol))
     (let* ((symbol (second lname))
            (sym-pkg (symbol-package symbol))
            (pkg-name (if sym-pkg
                      (string (package-name sym-pkg))
                      "UNINTERNED"))
           (name (symbol-name symbol))
           (specializers (core:fmt nil "{}" (cddr lname))))
       (escape-and-join-jit-name (list name pkg-name specializers "METHOD"))))
    ;; (METHOD (SETF symbol) . specializer-list): a method function
    ((cons (eql method) (cons cons))
     (let* ((name-list (second lname))
            (setf-name-symbol (second name-list))
            (pkg-symbol (symbol-package setf-name-symbol))
            ;;; e.g. lname = (METHOD (SETF #:G4336) (T CONS))
            (pkg-name (if pkg-symbol
                          (string (package-name pkg-symbol))
                          "UNINTERNED"))
            (specializers (core:fmt nil "{}" (cddr lname))))
       (escape-and-join-jit-name (list (string setf-name-symbol) pkg-name specializers "SETFMETHOD"))))
    ;; (LAMBDA lambda-list): an anonymous function
    ((cons (eql lambda)) (jit-function-name 'cl:lambda))
    ;; (FLET name) or (LABELS name): a local function
    ;; plus various more little extensions for readability.
    ;; See defmacro.lisp for occurrence.
    ((cons (member flet labels
                   cl:macro-function cl:compiler-macro-function
                   ext::type-expander ext::setf-expander))
     (jit-function-name (second lname)))
    #+(or) ;; uncomment this to be more forgiving
    (cons
     (core:fmt t "jit-function-name handling UNKNOWN: {}%N" lname)
     ;; What is this????
     (core:fmt nil "{}_CONS-LNAME?" lname))
    (t (error "Illegal lisp function name[~a]" lname))))

(defparameter *dump-compile-module* nil)

(defparameter *jit-lock* (mp:make-recursive-mutex 'jit-lock))

(defun jit-add-module (module startup-shutdown-id ctable-name fvector-name)
  (cmp:irc-verify-module-safe module)
  (unwind-protect
       (let ((jit-engine (llvm-sys:clasp-jit)))
         (cmp:with-track-llvm-time
           (when *dump-compile-module*
             (format t "About to dump module~%")
             (llvm-sys:dump-module module)
             (format t "Done dump module~%"))
           (mp:with-lock (*jit-lock*)
             (let ((dylib (llvm-sys:get-main-jitdylib jit-engine)))
               ;; Install the literals, and as a bonus, collect the constants table
               ;; and function vector so the caller can make or retrieve objects.
               (let ((object-file
                       (llvm-sys:add-irmodule
                        jit-engine dylib module
                        cmp:*thread-safe-context* startup-shutdown-id))
                     (litarr (llvm-sys:lookup jit-engine dylib ctable-name))
                     (fvector
                       (llvm-sys:lookup jit-engine dylib fvector-name)))
                 (values object-file litarr fvector))))))
    (gctools:thread-local-cleanup)))
