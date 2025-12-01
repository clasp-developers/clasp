(in-package #:cross-clasp)

;;; Compute simple instructions for the builder. Each instruction is either
;;; (:load "whatever.cfasl") meaning to load an existing cfasl, or
;;; (:compile-file "whatever.lisp" "source" "whatever.fasl" "whatever.cfasl")
;;; meaning to compile a new one. If CLEAN is true, compile everything.
;;; To figure out what to do, we just compare file write dates. If a FASL and
;;; CFASL exist, and were written after the source file, just load the CFASL.
;;; Otherwise, compile the source anew, and then compile all remaining files
;;; regardless of how new their FASLs are (in case of compile time dependency).
(defun compute-system (input-files output-files source-pathnames cfasls
                       &key clean)
  (loop for input in input-files
        for source in source-pathnames
        for output in output-files
        for cfasl in cfasls
        if (or clean
             (not (probe-file output)) (not (probe-file cfasl)))
          do (setf clean t)
          and collect `(:compile-file ,input ,source ,output ,cfasl)
        else collect (let ((input-date (ignore-errors (file-write-date input)))
                           (output-date (ignore-errors (file-write-date output)))
                           (cfasl-date (ignore-errors (file-write-date cfasl))))
                       (cond ((and input-date
                                output-date (> output-date input-date)
                                cfasl-date (> cfasl-date input-date))
                              `(:load ,cfasl))
                             (t (setf clean t)
                                `(:compile-file ,input ,source
                                                ,output ,cfasl))))))

;;; Execute the system's build instruction.
(defun build-system (system &key native)
  (let ((*compile-verbose* t) (*compile-print* t)
        (*load-verbose* t) (*load-print* t)
        ;; COMPILER instead of CMP so that we get Clasp's package,
        ;; not the local package nickname.
        ;; We disable Cleavir because it's too slow:
        ;; The build compiles a _lot_ of macroexpanders, mostly for CLOS,
        ;; and constructing CSTs for their bodies takes positively
        ;; stupid amounts of time. FIXME?
        #+clasp(compiler:*cleavir-compile-hook* nil)
        #+clasp(maclina.compile-file::*module-native-compiler*
                 (if native #'module-native-compiler nil)))
    (handler-bind
        (;; SBCL's script processor muffles style warnings, which is
         ;; pretty unfortunate for us, so print them ourselves here.
         #+sbcl
         (style-warning (lambda (w)
                          (format *error-output* "~&WARNING: ~a~%" w)
                          (muffle-warning w))))
      (maclina.compile:with-compilation-unit ()
        (loop with ct-client = (make-instance 'ct-client)
              with compiler = (if native
                                  #'build-native-file
                                  #'build-bytecode-file)
              for (command . rest) in system
              do (ecase command
                   ((:compile-file)
                    (destructuring-bind (input source output cfasl) rest
                      (funcall compiler input output source
                               cfasl ct-client)))
                   ((:load)
                    (destructuring-bind (cfasl) rest
                      (maclina.load:load-bytecode
                       cfasl :environment *build-rte*)))))))))

(defun build-bytecode-file (input output source cfasl ct-client)  
  (maclina.compile-file:compile-file
   input :output-file output
         :environment *build-rte*
         :evaluation-client ct-client
         :output-cfasl cfasl
         :source-pathname source))

(defun build-native-file (input output source cfasl ct-client)
  ;; We don't want native CFASLs so don't do that part.
  (declare (ignore client))
  (maclina.compile-file:compile-file
   input :output-file output
         :environment *build-rte*
         :evaluation-client ct-client
         :source-pathname source))

(defun build (input-files output-files source-pathnames)
  (multiple-value-bind (fasls cfasls)
      (loop for (output cfasl) on output-files by #'cddr
            collect output into fasls
            collect cfasl into cfasls
            finally (return (values fasls cfasls)))
    (let ((system (compute-system input-files fasls
                                  source-pathnames cfasls)))
      (build-system system))))

(defun build-native (input-files output-files source-pathnames cfasls)
  (let ((system (compute-system input-files output-files
                                source-pathnames cfasls)))
    (build-system system :native t)))
