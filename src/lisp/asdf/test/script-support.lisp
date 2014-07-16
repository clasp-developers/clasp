;;;;; Minimal life-support for testing ASDF from a blank Lisp image.
#|
Some constraints:
* We cannot rely on any test library that could be loaded by ASDF.
 And we cannot even rely on ASDF being present until we load it.
 But we *can* rely on ASDF being present *after* we load it.
* evaluating this file MUST NOT print anything,
 because we use it in the forward-ref test to check that nothing is printed.
* We make sure that none of our symbols clash with uiop or asdf,
 so we may use-package them during testing.
|#

(defpackage :asdf-test
  (:use :common-lisp)
  (:export
   #:asym #:acall #:asymval
   #:*test-directory* #:*asdf-directory* #:*build-directory* #:*implementation*
   #:deftest #:is #:signals
   #:assert-compare #:assert-equal #:assert-pathname-equal #:assert-pathnames-equal
   #:hash-table->alist
   #:load-asdf #:maybe-compile-asdf
   #:load-asdf-lisp #:compile-asdf #:load-asdf-fasl
   #:compile-load-asdf #:compile-load-asdf-upgrade
   #:load-asdf-system #:clean-load-asdf-system
   #:register-directory #:load-test-system
   #:with-test #:test-asdf #:debug-asdf
   #:run-test-script #:interactive-test #:test-load-systems
   #:verbose #:exit-lisp
   #:assert-compare
   #:assert-equal
   #:leave-test #:def-test-system
   #:action-name #:in-plan-p
   #:test-source #:test-fasl #:resolve-output #:output-location
   #:quietly #:join-namestrings))

(in-package :asdf-test)

(declaim (optimize (speed 2) (safety #-gcl 3 #+gcl 0) #-(or allegro gcl genera) (debug 3)
                   #+(or cmu scl) (c::brevity 2)))
(proclaim '(optimize (speed #-gcl 2 #+gcl 1) (safety #-gcl 3 #+gcl 0) #-(or allegro gcl genera) (debug 3)
                     #+(or cmu scl) (c::brevity 2) #+(or cmu scl) (ext:inhibit-warnings 3)))

(defvar *trace-symbols*
  `(;; If you want to trace some stuff while debugging ASDF,
    ;; here's a nice place to say what.
    ;; These string designators will be interned in ASDF after it is loaded.

    ;;#+ecl ,@'( :perform :input-files :output-files :compile-file* :compile-file-pathname* :load*)
    ))

(defvar *debug-asdf* nil)
(defvar *quit-when-done* t)

(defun verbose (&optional (verbose t) (print verbose))
  (setf *load-verbose* verbose *compile-verbose* verbose)
  (setf *load-print* print *compile-print* print))

(verbose nil)

;;; Minimal compatibility layer
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+allegro
  (setf excl:*warn-on-nested-reader-conditionals* nil
        excl::*autoload-package-name-alist*
        (remove "asdf" excl::*autoload-package-name-alist*
                :test 'equalp :key 'car)) ; We need that BEFORE any mention of package ASDF.
  #+cmucl (setf ext:*gc-verbose* nil)
  #+gcl
  (si::use-fast-links nil)
  #+lispworks
  (setf system:*stack-overflow-behaviour* :warn))

#+genera
(unless (fboundp 'ensure-directories-exist)
  (defun ensure-directories-exist (path)
    #+genera (fs:create-directories-recursively (pathname path))))

;;; Survival utilities
(defun asym (name &optional package errorp)
  (let* ((pname (or package :asdf))
         (package (find-package pname)))
    (if package
        (or (find-symbol (string name) package)
            (when errorp (error "Can't find symbol ~A in ~A" name pname)))
        (when errorp (error "Can't find package ~A" pname)))))
(defun acall (name &rest args)
  (apply (apply 'asym (if (consp name) name (list name))) args))
(defun asymval (name &optional package)
  (symbol-value (asym name package)))
(defsetf asymval (name &optional package) (new-value)
  (let ((sym (gensym "SYM")))
    `(let ((,sym (asym ,name ,package)))
       (if ,sym
           (setf (symbol-value ,sym) ,new-value)
           (error "NIHIL EX NIHILO")))))

(defun finish-outputs* ()
  (loop :for s :in (list *standard-output* *error-output* *trace-output* *debug-io*)
        :do (finish-output s)))
(defun redirect-outputs ()
  (finish-outputs*)
  (setf *error-output* *standard-output*
        *trace-output* *standard-output*))

(redirect-outputs) ;; Put everything on standard output, for the sake of scripts

;;; Poor man's test suite, lacking stefil.
(defmacro deftest (name formals &body body)
  `(defun ,name ,formals ,@body))
(defmacro is (x)
  `(progn
     (format *error-output* "~&Checking whether ~S~%" ',x)
     (finish-output *error-output*)
     (assert ,x)))
(defmacro signals (condition sexp &aux (x (gensym)))
  `(progn
     (format *error-output* "~&Checking whether ~S signals ~S~%" ',sexp ',condition)
     (finish-output *error-output*)
     (handler-case
         ,sexp
       (,condition (,x)
         (format *error-output* "~&Received signal ~S~%" ,x)
         (finish-output *error-output*)
         t)
       (:no-error (&rest ,x)
         (error "Expression ~S fails to raise condition ~S, instead returning~{ ~S~}"
                ',sexp ',condition ,x))
       (t (,x)
         (error "Expression ~S raises signal ~S, not ~S" ',sexp ,x ',condition)))))


;;; Helpful for debugging
(defun pathname-components (p)
  (when p
    (let ((p (pathname p)))
      (list :host (pathname-host p)
            :device (pathname-device p)
            :directory (pathname-directory p)
            :name (pathname-name p)
            :type (pathname-type p)
            :version (pathname-version p)))))

(defun assert-pathname-equal-helper (qx x qy y)
  (cond
    ((equal x y)
     (format t "~S and~% ~S both evaluate to same path:~%  ~S~%" qx qy x))
    #+mkcl
    ((acall :pathname-equal x y)
     (format t "~S and ~S evaluate to functionaly equivalent paths, respectively:~%  ~S~%and~%  ~S~%" qx qy x y))
    ((acall :pathname-equal x y)
     (warn "These two expressions yield pathname-equal yet not equal path~%~
        the first expression ~S yields this:~%  ~S~%  ~S~%
        the other expression ~S yields that:~%  ~S~%  ~S~%"
        qx x (pathname-components x)
        qy y (pathname-components y)))
    (t
     (error "These two expressions yield paths that are not pathname-equal~%~
        the first expression ~S yields this:~%  ~S~%  ~S~%
        the other expression ~S yields that:~%  ~S~%  ~S~%"
        qx x (pathname-components x)
        qy y (pathname-components y)))))
(defmacro assert-pathname-equal (x y)
  `(assert-pathname-equal-helper ',x ,x ',y ,y))
(defun assert-length-equal-helper (qx x qy y)
  (unless (= (length x) (length y))
    (error "These two expressions yield sequences of unequal length~%
        The first, ~S, has value ~S of length ~S~%
        The other, ~S, has value ~S of length ~S~%"
           qx x (length x) qy y (length y))))
(defun assert-pathnames-equal-helper (qx x qy y)
  (assert-length-equal-helper qx x qy y)
  (loop :for n :from 0
        :for qpx = `(nth ,n ,qx)
        :for qpy = `(nth ,n ,qy)
        :for px :in x
        :for py :in y :do
        (assert-pathname-equal-helper qpx px qpy py)))
(defmacro assert-pathnames-equal (x y)
  `(assert-pathnames-equal-helper ',x ,x ',y ,y))

;; More pathname madness.
;; We can't use goodies from asdf/pathnames because ASDF isn't loaded yet.
;; We still want to work despite and host/device funkiness,
;; so we do it the hard way.
(defparameter *test-directory*
  (truename
   (make-pathname :name nil :type nil :version nil
                  :defaults (or *load-pathname* *compile-file-pathname* *default-pathname-defaults*))))
(defun make-sub-pathname (&rest keys &key defaults &allow-other-keys)
  (merge-pathnames (apply 'make-pathname keys) defaults))
(defparameter *asdf-directory*
  (truename (make-sub-pathname :directory '(:relative :back) :defaults *test-directory*)))
(defparameter *uiop-directory*
  (truename (make-sub-pathname :directory '(:relative "uiop") :defaults *asdf-directory*)))
(defparameter *build-directory*
  (make-sub-pathname :directory '(:relative "build") :defaults *asdf-directory*))
(defparameter *implementation*
  (or #+allegro
      (ecase excl:*current-case-mode*
        (:case-sensitive-lower :mlisp)
        (:case-insensitive-upper :alisp))
      #+armedbear :abcl
      #+clisp :clisp
      #+clozure :ccl
      #+cmu :cmucl
      #+corman :cormanlisp
      #+digitool :mcl
      #+ecl (or #+ecl-bytecmp :ecl_bytecodes :ecl)
      #+gcl :gcl
      #+lispworks :lispworks
      #+mkcl :mkcl
      #+sbcl :sbcl
      #+scl :scl
      #+xcl :xcl))
(defparameter *early-fasl-directory*
  (make-sub-pathname :directory `(:relative "fasls" ,(string-downcase *implementation*))
                     :defaults *build-directory*))

(defun asdf-name (&optional tag)
  (format nil "asdf~@[-~A~]" tag))
(defun asdf-lisp (&optional tag)
  (make-pathname :name (asdf-name tag) :type "lisp" :defaults *build-directory*))
(defun debug-lisp ()
  (make-sub-pathname :directory '(:relative "contrib") :name "debug" :type "lisp" :defaults *uiop-directory*))
(defun early-compile-file-pathname (file)
  (compile-file-pathname
   (make-pathname :name (pathname-name file) :type "lisp" :defaults *early-fasl-directory*)))
(defun asdf-fasl (&optional tag)
  (early-compile-file-pathname (asdf-lisp tag)))

;;; Test helper functions

(load (debug-lisp))
(verbose t nil)

(defmacro assert-compare (expr)
  (destructuring-bind (op x y) expr
    `(assert-compare-helper ',op ',x ',y ,x ,y)))

(defun assert-compare-helper (op qx qy x y)
  (unless (funcall op x y)
    (error "These two expressions fail comparison with ~S:~% ~
            ~S evaluates to ~S~% ~S evaluates to ~S~%"
            op qx x qy y)))

(defmacro assert-equal (x y)
  `(assert-compare (equal ,x ,y)))

(defun set-equality (s1 s2 &key (test 'eql))
  (and (= (length s1) (length s2))
       (every #'(lambda (x)
                  (some #'(lambda (y)
                            (funcall test x y))
                        s2))
              s1)))

(defun touch-file (file &key offset timestamp in-filesystem)
  (let* ((base (or timestamp (get-universal-time)))
         (stamp (if offset (+ base offset) base)))
    (if (and (asymval :*asdf-cache*) (not in-filesystem))
        (acall :register-file-stamp file stamp)
        (multiple-value-bind (sec min hr day month year)
            (decode-universal-time stamp)
          (unless in-filesystem
            (error "Y U NO use stamp cache?"))
          (acall :run-program
                 `("touch" "-t" ,(format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D.~2,'0D"
                                         year month day hr min sec)
                           ,(acall :native-namestring file)))
          (assert-equal (file-write-date file) stamp)))))
(defun mark-file-deleted (file)
  (unless (asymval :*asdf-cache*) (error "Y U NO use asdf cache?"))
  (acall :register-file-stamp (acall :normalize-namestring file) nil))

(defun hash-table->alist (table)
  (loop :for key :being :the :hash-keys :of table :using (:hash-value value)
    :collect (cons key value)))

(defun exit-lisp (&optional (code 0)) ;; Simplified from asdf/image:quit
  (finish-outputs*)
  #+(or abcl xcl) (ext:quit :status code)
  #+allegro (excl:exit code :quiet t)
  #+clisp (ext:quit code)
  #+clozure (ccl:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+(or cmu scl) (unix:unix-exit code)
  #+ecl (si:quit code)
  #+gcl (system:quit code)
  #+genera (error "You probably don't want to Halt the Machine. (code: ~S)" code)
  #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
  #+mcl (ccl:quit) ;; or should we use FFI to call libc's exit(3) ?
  #+mkcl (mk-ext:quit :exit-code code)
  #+sbcl #.(let ((exit (find-symbol "EXIT" :sb-ext))
                 (quit* (find-symbol "QUIT" :sb-ext)))
             (cond
               (exit `(,exit :code code :abort t))
               (quit* `(,quit* :unix-status code :recklessly-p t))))
  #-(or abcl allegro clisp clozure cmu ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (error "~S called with exit code ~S but there's no quitting on this implementation" 'quit code))


(defun leave-test (message return)
  "Print MESSAGE and throw RETURN, which should be a POSIX error
code (an integer, 0 for success), up as exit code."
  (finish-outputs*)
  (fresh-line *error-output*)
  (when message
    (format *error-output* message)
    (fresh-line *error-output*))
  (finish-outputs*)
  (throw :asdf-test-done return))

(defmacro with-test ((&optional) &body body)
  `(call-with-test (lambda () ,@body)))

(defun call-with-test (thunk)
  "Unless the environment variable DEBUG_ASDF_TEST
is bound, write a message and exit on an error.  If
*asdf-test-debug* is true, enter the debugger."
  (redirect-outputs)
  (let ((result
          (catch :asdf-test-done
            (handler-bind
                ((error (lambda (c)
                          (ignore-errors
                           (format *error-output* "~&TEST ABORTED: ~A~&" c))
                          (finish-outputs*)
                          (cond
                            (*debug-asdf*
                             (format t "~&It's your baby, fix it!~%")
                             (break))
                            (t
                             (ignore-errors
                              (acall :print-condition-backtrace
                                     c :count 69 :stream *error-output*))
                             (leave-test "Script failed" 1))))))
              (funcall (or (asym :call-with-asdf-cache) 'funcall) thunk)
              (leave-test "Script succeeded" 0)))))
    (when *quit-when-done*
      (exit-lisp result))))

;;; These are used by the upgrade tests

(defmacro quietly (&body body)
  `(call-quietly #'(lambda () ,@body)))

(defun call-quietly (thunk)
  #-allegro
  (handler-bind (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning))
    (funcall thunk))
  #+allegro
  (excl:without-redefinition-warnings
   (funcall thunk)))

(defun interactive-test (&optional files)
  (verbose t nil)
  (loop :for file :in files :do
    (load (string-downcase file)))
  (setf *package* (some 'find-package '(:asdf :uiop :asdf/utility :asdf/package :asdf-test)))
  (load "contrib/debug.lisp"))

(defun run-test-script (file)
  (with-test ()
    (let ((name (string file)))
      (format t "Running ~A with ~A~%" name (acall :implementation-identifier))
      (load name))))

(defun load-asdf-lisp (&optional tag)
  (quietly (load (asdf-lisp tag) :verbose *load-verbose* :print *load-print*)))

(defun load-asdf-fasl (&optional tag)
  (quietly (load (asdf-fasl tag))))

(defun register-directory (dir)
  (pushnew dir (symbol-value (asym :*central-registry*))))

(defun load-asdf-system (&rest keys)
  (quietly
   (register-directory *asdf-directory*)
   (register-directory *uiop-directory*)
   (apply (asym :oos) (asym :load-op) :asdf keys)))

(defun call-with-asdf-conditions (thunk &optional verbose)
  (declare (ignorable verbose))
  (handler-bind (#+sbcl
                 ((or sb-c::simple-compiler-note sb-kernel:redefinition-warning)
                   #'muffle-warning)
                 #+(and ecl (not ecl-bytecmp))
                 ((or c::compiler-note c::compiler-debug-note
                      c::compiler-warning) ;; ECL emits more serious warnings than it should.
                   #'muffle-warning)
                 #+mkcl
                 ((or compiler:compiler-note) #'muffle-warning)
                 #-(or cmu scl)
                 ;; style warnings shouldn't abort the compilation [2010/02/03:rpg]
                 (style-warning
                   #'(lambda (w)
                       ;; escalate style-warnings to warnings - we don't want them.
                       (when verbose
                         (warn "Can you please fix ASDF to not emit style-warnings? Got a ~S:~%~A"
                               (type-of w) w))
                       (muffle-warning w))))
    (funcall thunk)))

(defmacro with-asdf-conditions ((&optional verbose) &body body)
  `(call-with-asdf-conditions #'(lambda () ,@body) ,verbose))

(defun compile-asdf (&optional tag verbose upgradep)
  (let* ((alisp (asdf-lisp tag))
         (afasl (asdf-fasl tag))
         (tmp (make-pathname :name "asdf-tmp" :defaults afasl)))
    (ensure-directories-exist afasl)
    (multiple-value-bind (result warnings-p failure-p)
        (compile-file alisp :output-file tmp :verbose verbose :print verbose)
      (flet ((bad (key)
               (when result (ignore-errors (delete-file result)))
               key)
             (good (key)
               (when (probe-file afasl) (delete-file afasl))
               (rename-file tmp afasl)
               key))
        (cond
          ((null result)
           (bad :no-output))
          (failure-p
           (or
            #+clisp (good :expected-full-warnings)
            (bad :unexpected-full-warnings)))
          (warnings-p
           (or
            ;; CLISP has full warnings for method redefinition in eval-when.
            ;; CMUCL: ?
            ;; ECL 11.1.1 has spurious warnings, same with XCL 0.0.0.291.
            ;; SCL has no warning but still raises the warningp flag since 2.20.15 (?)
            #+(or clisp cmu ecl scl xcl) (good :expected-style-warnings)
            (and upgradep (good :unexpected-style-warnings))
            (bad :unexpected-style-warnings)))
          (t (good :success)))))))

(defun maybe-compile-asdf (&optional tag)
  (let ((alisp (asdf-lisp tag))
        (afasl (asdf-fasl tag)))
    (cond
      ((not (probe-file alisp))
       :not-found)
      ((and (probe-file afasl)
            (> (file-write-date afasl) (file-write-date alisp))
            (ignore-errors (load-asdf-fasl tag)))
       :previously-compiled)
      (t
       (compile-asdf tag)))))

(defun compile-asdf-script ()
  (with-test ()
    (ecase (with-asdf-conditions () (maybe-compile-asdf))
      (:not-found
       (leave-test "Testsuite failed: unable to find ASDF source" 3))
      (:previously-compiled
       (leave-test "Reusing previously-compiled ASDF" 0))
      (:no-output
       (leave-test "Testsuite failed: ASDF compilation failed without output" 1))
      (:unexpected-full-warnings
       (leave-test "Testsuite failed: ASDF compiled with unexpected full warnings" 1))
      (:expected-full-warnings
       (leave-test "ASDF compiled with full warnings, ignored for your implementation" 0))
      (:unexpected-style-warnings
       (leave-test "Testsuite failed: ASDF compiled with unexpected warnings" 1))
      (:expected-style-warnings
       (leave-test "ASDF compiled with style-warnings, ignored for your implementation" 0))
      (:success
       (leave-test "ASDF compiled cleanly" 0)))))

(defun compile-load-asdf (&optional tag upgradep)
  ;; emulate the way asdf upgrades itself: load source, compile, load fasl.
  (load-asdf-lisp tag)
  (let ((results (compile-asdf tag nil upgradep)))
    (ecase results
      ((:no-output :unexpected-full-warnings :unexpected-style-warnings)
         (warn "ASDF compiled with ~S" results)
         (unless (and upgradep (eq results :unexpected-style-warnings))
           (leave-test "failed to compile ASDF" 1)))
      ((:expected-full-warnings :expected-style-warnings :success)))
    (load-asdf-fasl tag)))

(defun compile-load-asdf-upgrade (&optional tag)
  (compile-load-asdf tag t))

;;; Now, functions to compile and load ASDF.

(defun load-test-system (x &key verbose)
  (let ((*load-print* verbose)
        (*load-verbose* verbose))
    (register-directory *test-directory*)
    (acall :oos (asym :load-op) x :verbose verbose)))

(defun get-asdf-version ()
  (when (find-package :asdf)
    (or (symbol-value (find-symbol (string :*asdf-version*) :asdf))
        (let ((ver (symbol-value (find-symbol (string :*asdf-revision*) :asdf))))
          (etypecase ver
            (string ver)
            (cons (format nil "~{~D~^.~}" ver))
            (null "1.0"))))))

(defun output-location (&rest sublocation)
  (list* *asdf-directory* "build/fasls" :implementation sublocation))
(defun resolve-output (&rest sublocation)
  (acall :resolve-location (apply 'output-location sublocation)))

(defun test-source (file)
  (acall :subpathname *test-directory* file))
(defun test-output-dir ()
  (resolve-output "asdf" "test"))
(defun test-output (file)
  (acall :subpathname (test-output-dir) file))
(defun test-fasl (file)
  (acall :compile-file-pathname* (test-source file)))

(defun clean-asdf-system ()
  ;; If compiled by an antique ASDFs without output translations:
  (flet ((d (x) (acall :delete-file-if-exists x)))
    (let ((asdf-fasl (compile-file-pathname (asdf-lisp))))
      (d asdf-fasl)
      (d (acall :apply-output-translations asdf-fasl))
      (d (asdf-fasl)))))

(defun load-asdf-lisp-clean ()
  (load-asdf-lisp)
  (clean-asdf-system))

(defun configure-asdf ()
  (format t "Configuring ASDF~%")
  (when (asym :getenvp)
    (format t "Enabling debugging~%")
    (setf *debug-asdf* (or *debug-asdf* (acall :getenvp "DEBUG_ASDF_TEST"))))
  (when *trace-symbols*
    (format t "Tracing~{ ~A~}~%" *trace-symbols*)
    (eval `(trace ,@(loop :for s :in *trace-symbols* :collect (asym s)))))
  (when (asym :initialize-source-registry)
    (acall :initialize-source-registry
           `(:source-registry :ignore-inherited-configuration)))
  (when (asym :initialize-output-translations)
    (acall :initialize-output-translations
           `(:output-translations
             (,(acall :wilden *asdf-directory*) ,(acall :wilden (resolve-output "asdf/")))
             (t ,(acall :wilden (resolve-output "root")))
             :ignore-inherited-configuration)))
  (when (asym :*central-registry*)
    (set (asym :*central-registry*) `(,*test-directory*)))
  (format t "Being a bit verbose~%")
  (when (asym :*asdf-verbose*) (setf (asymval :*asdf-verbose*) t))
  (when (asym :*verbose-out*) (setf (asymval :*verbose-out*) *standard-output*))
  (when (and (asym :locate-system) (asym :pathname-directory-pathname) (asym :pathname-equal))
    (format t "Comparing directories~%")
    (acall :call-with-asdf-cache
             #'(lambda ()
                 (let ((x (acall :pathname-directory-pathname (nth-value 2 (acall :locate-system :test-asdf)))))
                   (assert-pathname-equal-helper ;; not always EQUAL (!)
                    '*test-directory* *test-directory*
                    '(:pathname-directory-pathname (nth-value 2 (:locate-system :test-asdf))) x)
                   (unless (equal *test-directory* x)
                     (format t "Interestingly, while *test-directory* has components~% ~S~%~
                 ASDF finds the ASDs in~% ~S~%Using the latter.~%"
                             (pathname-components *test-directory*)
                             (pathname-components x)))
                   (setf *test-directory* x)))))
  t)

(defun frob-packages ()
  (format t "Frob packages~%")
  (use-package :asdf :asdf-test)
  (when (find-package :uiop) (use-package :uiop :asdf-test))
  (when (find-package :asdf/cache) (use-package :asdf/cache :asdf-test))
  (setf *package* (find-package :asdf-test))
  t)

(defun load-asdf-lisp-and-test-uiop (&optional tag)
  (load-asdf-lisp tag)
  (unless (and (member :asdf *features*)
               (or (member :asdf3 *features*)
                   (and (member :asdf2 *features*) (acall :version-satisfies (acall :asdf-version) "2.11.4"))))
    (leave-test "UIOP will break ASDF < 2.011.4 - skipping test." 0))
  (configure-asdf)
  (register-directory *asdf-directory*)
  (register-directory *uiop-directory*)
  (register-directory *test-directory*)
  (quietly
   (acall :oos (asym :load-op) :uiop))
  (acall :oos (asym :load-op) :test-module-depend))

(defun load-asdf (&optional tag)
  (load-asdf-fasl tag)
  (configure-asdf))

(defun debug-asdf ()
  (setf *debug-asdf* t)
  (setf *quit-when-done* nil)
  (setf *package* (find-package :asdf-test)))

(defun just-load-asdf-fasl () (load-asdf-fasl))

;; Actual scripts rely on this function:
(defun common-lisp-user::load-asdf () (load-asdf))

(setf *package* (find-package :asdf-test))

(defmacro def-test-system (name &rest rest)
  (etypecase name
    (symbol
     `(apply (asym :register-system-definition) ',name
             :pathname ,*test-directory*
             :source-file nil ',rest))
    (string
     `(apply (asym :register-system-definition) ,name
             :pathname ,*test-directory*
             :source-file nil ',rest))))

(defun in-plan-p (plan x) (member x (acall :plan-actions plan) :key (asym :action-path) :test 'equal))

(defmacro test-load-systems (&rest x)
  `(do-test-load-systems ',x))

(defun do-test-load-systems (systems)
  (load-asdf-lisp)
  (dolist (sys systems)
    (format t "~&Trying to load ~A~%" sys)
    (acall :load-system sys))
  (format t "~&Done!~%"))

(defun test-upgrade (old-method new-method tag) ;; called by run-test
  (with-test ()
    (verbose t nil)
    (setf tag (string tag))
    (when old-method
      (cond
        ((string-equal tag "REQUIRE")
         (format t "Requiring some previous ASDF ~A~%" tag)
         (ignore-errors (funcall 'require "asdf"))
         (if (member "ASDF" *modules* :test 'equalp)
             (format t "Your Lisp implementation provided ASDF ~A~%" (get-asdf-version))
             (leave-test "Your Lisp implementation does not provide ASDF. Skipping test.~%" 0)))
        (t
         (format t "Loading old asdf ~A via ~A~%" tag old-method)
         (funcall old-method tag))))
    (when (find-package :asdf)
      (configure-asdf))
    (when (and (null old-method) (eq 'load-asdf-fasl new-method) (not (probe-file (asdf-fasl))))
      (if (ignore-errors (funcall 'require "asdf") t)
          (leave-test "Your failed to compile ASDF before your run (test-upgrade ()'load-asdf-fasl ...)"  1)
          (leave-test "Your Lisp doesn't provide ASDF. Skipping (test-upgrade ()'load-asdf-fasl ...)"  0)))
    (format t "Now loading new asdf via method ~A~%" new-method)
    (funcall new-method)
    (format t "Testing it~%")
    (register-directory *test-directory*)
    (load-test-system :test-asdf/all)
    (assert (asymval '#:*file1* :test-package))
    (assert (asymval '#:*file3* :test-package))))

(defun join-namestrings (namestrings)
  (with-output-to-string (s)
    (loop :with separator = (acall :inter-directory-separator)
          :for (n . morep) :on namestrings
          :do (format s "~A~@[~C~]" n (and morep separator)))))

;; These are shorthands for interactive debugging of test scripts:
(!a
 common-lisp-user::debug-asdf debug-asdf
 da debug-asdf common-lisp-user::da debug-asdf
 la load-asdf common-lisp-user::la load-asdf
 ll load-asdf-lisp
 v verbose)

#| For the record, the following form is sometimes useful to insert in
 asdf/plan:compute-action-stamp to find out what's happening.
 It depends on the DBG macro in contrib/debug.lisp,
 that you should load in your asdf/plan by inserting an (uiop-debug) form in it.

 (let ((action-path (action-path (cons o c)))) (DBG :cas action-path just-done plan stamp-lookup out-files in-files out-op op-time dep-stamp out-stamps in-stamps missing-in missing-out all-present earliest-out latest-in up-to-date-p done-stamp (operation-done-p o c)
;;; blah
))
|#
