;;;
;;;    File: cmpbundle.lsp
;;;

;; Copyright (c) 2014, Christian E. Schafmeister
;;
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; See directory 'clasp/licenses' for full details.
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; -^-

(in-package :cmp)


(defun bundle-type-as-string (bundle-type)
  (cond
    ((eq bundle-type 'kernel) "kernel")
    ((eq bundle-type 'user) "user")
    (t (error "Illegal bundle-type ~a - only kernel or user are allowed" bundle-type))))

(defun bundle-name-as-string (bundle-name)
  (string-downcase (symbol-name bundle-name)))


(defun as-shell-command (list-of-args)
  (with-output-to-string (sout)
    (princ (car list-of-args) sout)
    (dolist (c (cdr list-of-args))
      (bformat sout " %s" c))))

(defvar *safe-system-echo* nil)
(defvar *safe-system-max-retries* 4)
(defvar *safe-system-retry-wait-time* 0.1d0) ;; 100 milliseconds
;; The wait time will be doubled at each retry!

(defun safe-system (cmd-list &key output-file-name)
  (if *safe-system-echo*
      (bformat t "safe-system: %s%N" cmd-list))

  (multiple-value-bind (retval error-message)
      (ext:vfork-execvp cmd-list)

    (unless (eql retval 0)
      (error "Could not execute command with ext:vfork-execvp with ~s~%  return-value: ~d  error-message: ~s~%" cmd-list retval error-message)))

  (when output-file-name
    (let ((sleep-time *safe-system-retry-wait-time*))
      (dotimes (nm1 (- *safe-system-max-retries* 1))
        (let ((n (+ nm1 1)))
          (unless (probe-file output-file-name)
            (if (>= n *safe-system-max-retries*)
                (error "The file ~a was not created by shell command: ~a" output-file-name (as-shell-command cmd-list))
                (progn
                  (if *safe-system-echo*
                      (bformat t "safe-system: Retry count = %d of %d%N" n *safe-system-max-retries*))
                  (core::sleep sleep-time)
                  (setq sleep-time (* 2 sleep-time)))))))))

  ;; Return T if all went well
  t)

;;; This function will compile a bitcode file in PART-BITCODE-PATHNAME with clang and put the output in the
;;; same directory as PART-BITCODE-PATHNAME
#+(or)
(defun generate-object-file (part-bitcode-pathname &key test)
  (let ((output-pathname (compile-file-pathname part-bitcode-pathname :output-type :object))
        (reloc-model (cond
                       ((or (member :target-os-linux *features*) (member :target-os-freebsd *features*))
                        'llvm-sys:reloc-model-pic-)
                       (t 'llvm-sys:reloc-model-undefined))))
    (bitcode-to-obj-file part-bitcode-pathname output-pathname :reloc-model reloc-model)
    (truename output-pathname)))

(defun ensure-string (name)
  (cond
    ((pathnamep name) (core:coerce-to-filename name))
    ((stringp name) name)
    ((null name) (error "The argument to ensure-string is NIL"))
    (t (string name))))

(defparameter *link-options* nil)

(defun execute-link-library (in-bundle-file in-all-names &key input-type (output-type :dynamic))
  ;; options are a list of strings like (list "-v")
  (let ((output-flag (cond
                       ((member :target-os-darwin *features*)
                        (case output-type
                          (:dynamic "-bundle")
                          (:static "-static")
                          (otherwise (error "Unknown output-type ~a for darwin" output-type))))
                       ((or (member :target-os-linux *features*)
                            (member :target-os-freebsd *features*))
                        (case output-type
                          (:dynamic "-shared")
                          (:static "-static")
                          (otherwise (error "Unknown output-type ~a for linux or freebsd" output-type))))
                       (t (error "Unknown system type")))))
    (let* ((options *link-options*)
           (all-object-files (mapcar (lambda (n)
                                       (ensure-string n))
                                     (if (listp in-all-names)
                                         in-all-names
                                         (list in-all-names))))
           (bundle-file (ensure-string in-bundle-file))
           (temp-bundle-file (ensure-string (core:mkstemp bundle-file))))
      (let ((clang-args (cond
                          ((member :target-os-darwin *features*)
                           (let* ((object-lto-pathname (make-pathname :type "o"
                                                                      :name (sys:bformat nil "%s-lto" (pathname-name bundle-file))
                                                                      :defaults bundle-file))
                                  (clang-args `( "-flto"
                                                 ,(sys:bformat nil "-Wl,-object_path_lto,%s" (namestring object-lto-pathname))
                                                 ,@options
                                                 ;; Disable the BranchFolding optimization that
                                                 ;; merges tails of branches as they join
                                                 ;; and was messing up debug source location info.
                                                 "-Wl,-mllvm,-enable-tail-merge=false"
                                                 ,(core:bformat nil "-O%d" *optimization-level*)
                                                 ,@all-object-files
;;;                                 "-macosx_version_min" "10.10"
                                                 "-flat_namespace"
                                                 #+(or)"-fvisibility=default"
                                                 "-undefined" "suppress"
                                                 ,@*debug-link-options*
                                                 #+(or)"-Wl,-save-temps"
                                                 ,output-flag
;;;                        ,@link-flags
;;;                        ,(bformat nil "-Wl,-object_path_lto,%s.lto.o" exec-file)
                                                 "-o"
                                                 ,temp-bundle-file)))
                             clang-args))
                          ((or (member :target-os-linux *features*)
                               (member :target-os-freebsd *features*))
                           ;; Linux needs to use clang to link
                           ;; FreeBSD might
                           (let ((clang-args `(#+(or)"-v"
                                                 ,@options
                                                 ,(core:bformat nil "-O%d" *optimization-level*) 
                                                 ,@all-object-files
                                                 "-flto"
                                                 "-fuse-ld=gold"
                                                 ,@*debug-link-options*
                                                 #+(or)"-fvisibility=default"
                                                 ,output-flag
                                                 "-o"
                                                 ,temp-bundle-file)))
                             clang-args))
                          (t (error "Add support for this operating system to cmp:generate-link-command")))))
        (ext:run-clang clang-args :output-file-name temp-bundle-file)
        (unless (probe-file temp-bundle-file)
          ;; I hate what I'm about to do - but on macOS -flto=thin can sometimes crash the linker
          ;; so get rid of that option (IT MUST BE THE FIRST ONE!!!!) and try again
          (when (member :target-os-darwin *features*)
            (warn "There was a HUGE problem in execute-link-fasl to generate ~a~% with the arguments: /path-to-clang ~a~%  I'm going to try removing the -flto=thin argument and try linking again~%" temp-bundle-file clang-args)
            (ext:run-clang (cdr clang-args) :output-file-name temp-bundle-file)
            (when (probe-file temp-bundle-file)
              (warn "execute-link-fasl worked after removing ~a from the argument list --- FIGURE OUT WHAT IS GOING WRONG WITH THAT ARGUMENT!!!" (car clang-args))))
          (unless (probe-file temp-bundle-file)
            (error "~%!~%!~%! There is a HUGE problem - an execute-link-fasl command with the arguments:   /path-to-clang ~a~%~%!        failed to generate the output file ~a~%" clang-args temp-bundle-file)))
        ;; Now rename the library to make compilation atomic
        ;; Run dsymutil on darwin
        (rename-file temp-bundle-file bundle-file :if-exists :supersede)
        #+target-os-darwin
        (ext:run-dsymutil (list "-f" (namestring bundle-file)))
        (truename bundle-file)))))

(defun execute-link-fasl (in-bundle-file in-all-names &key input-type)
  (execute-link-library in-bundle-file in-all-names :input-type input-type :output-type :dynamic))

(defun execute-link-static (in-bundle-file in-all-names &key input-type)
  (execute-link-library in-bundle-file in-all-names :input-type input-type :output-type :static))

(defun execute-link-object (output-filename in-all-names &key input-type)
  (safe-system (list* "ld" "-r"
                           "-o" (namestring output-filename)
                           (mapcar (lambda (f) (namestring (make-pathname :type "o" :defaults f)))
                                   in-all-names))))
                            
(defun execute-link-executable (output-file-name in-bitcode-names)
  ;; options are a list of strings like (list "-v")
  (let ((options nil)
        (all-names (mapcar (lambda (n)
                             (ensure-string n))
                           (if (listp in-bitcode-names)
                               in-bitcode-names
                               (list in-bitcode-names))))
        (exec-file (ensure-string output-file-name)))
    (multiple-value-bind (link-flags link-lib-path library-extension)
        (core:link-flags)
      (cond
        ((member :target-os-darwin *features*)
         (ext:run-clang `(,@options
                          ,(core:bformat nil "-O%d" *optimization-level*)
                          ,@all-names
                          #+(or)"-v"
                          ,@link-flags
                          ,(bformat nil "-Wl,-object_path_lto,%s.lto.o" exec-file)
                          "-o"
                          ,exec-file)
                        :output-file-name exec-file))
        ((or (member :target-os-linux *features*)
             (member :target-os-freebsd *features*))
         ;; Linux needs to use clang to link
         ;; FreeBSD might
         (ext:run-clang `(,@options
                          ,(core:bformat nil "-O%d" *optimization-level*)
                          ,@all-names
                          ,@link-flags
                          "-o"
                          ,exec-file)
                        :output-file-name exec-file))
        (t (error "Add support for this operating system to cmp:generate-link-command"))))
    (truename output-file-name)))

(defun link-bitcode-modules (output-pathname part-pathnames
                             &key additional-bitcode-pathnames)
  "Link a bunch of modules together, return the linked module"
  (with-compiler-env ()
    (let* ((module (llvm-create-module (pathname-name output-pathname)))
           (*compile-file-pathname* (pathname (merge-pathnames output-pathname)))
           (*compile-file-truename* (translate-logical-pathname *compile-file-pathname*))
           (bcnum 0))
      (with-module ( :module module
                     :optimize nil)
        (with-source-pathnames (:source-pathname (pathname output-pathname))
          (with-debug-info-generator (:module module :pathname output-pathname)
            (let* ((linker (llvm-sys:make-linker module))
                   (part-index 1))
              ;; Don't enforce .bc extension for additional-bitcode-pathnames
              ;; This is where I used to link the additional-bitcode-pathnames
              (dolist (part-pn part-pathnames)
                (let* ((bc-file (make-pathname :type (if cmp::*use-human-readable-bitcode* "ll" "bc") :defaults part-pn)))
;;;                (bformat t "Linking %s%N" bc-file)
                  (let* ((part-module (parse-bitcode (namestring (truename bc-file)) *llvm-context*)))
                    (incf part-index)
                    (multiple-value-bind (failure error-msg)
                        (llvm-sys:link-in-module linker part-module)
                      #+(or)
                      (let ((global-ctor (find-global-ctor-function part-module))
                            (priority part-index))
                        (remove-llvm.global_ctors-if-exists part-module)
                        (add-llvm.global_ctors part-module priority global-ctor)
                        (llvm-sys:link-in-module linker part-module))
                      (when failure
                        (error "While linking part module: ~a  encountered error: ~a" part-pn error-msg))))))
              ;; The following links in additional-bitcode-pathnames
              (dolist (part-pn additional-bitcode-pathnames)
                (let* ((bc-file part-pn))
;;;                (bformat t "Linking %s%N" bc-file)
                  (let* ((part-module (llvm-sys:parse-bitcode-file (namestring (truename bc-file)) *llvm-context*)))
                    (multiple-value-bind (failure error-msg)
                        (llvm-sys:link-in-module linker part-module)
                      (when failure
                        (error "While linking additional module: ~a  encountered error: ~a" bc-file error-msg))
                      ))))
              (write-bitcode module (core:coerce-to-filename (pathname (if output-pathname
                                                                           output-pathname
                                                                           (error "The output pathname is NIL")))))))))
      module)))
(export 'link-bitcode-modules)



;;;
;;; Gather a list of boot parts as pathnames
;;; Skip over keyword symbols in the boot part lists
;;;
(defun boot-bitcode-pathnames (last-file &key first-file target-backend)
  (or first-file (error "You must provide first-file"))
  (let* ((source-files (mapcan #'(lambda (part) (and (not (keywordp part)) (list (core::get-pathname-with-type part "lsp"))))
                               (core::select-source-files first-file last-file )))
         (bitcode-files (mapcar (lambda (k) (compile-file-pathname k :target-backend target-backend))
                                source-files)))
    bitcode-files))
(export 'boot-bitcode-pathnames)



(defun llvm-link (output-pathname
                  &key (link-type :fasl)
                    (input-type :bitcode)
                    input-files
                    (target-backend (default-target-backend)))
  "Link a collection of files together into a fasl or executable.
The input files are passed as a list in **input-files**.
The type of file generated is specified by **link-type** (:fasl|:executable).
The type of the files to be linked is defined with **input-type** (:bitcode|:object).
The **target-backend** indicates if we are linking for aclasp, bclasp or cclasp.
Return the **output-pathname**."
  (let* ((*target-backend* target-backend)
         (start-time (get-internal-real-time))
         (output-pathname (pathname output-pathname)))
    (cond
      ((eq link-type :executable)
       (push (core:build-inline-bitcode-pathname link-type :intrinsics) input-files)
       (when (eq input-type :bitcode)
         (push (core:build-inline-bitcode-pathname link-type :builtins) input-files))
       (execute-link-executable output-pathname input-files :input-type input-type))
      ((eq link-type :fasl)
       (push (core:build-inline-bitcode-pathname link-type :intrinsics) input-files)
       (when (eq input-type :bitcode)
         (push (core:build-inline-bitcode-pathname link-type :builtins) input-files))
       (when (member :debug-run-clang *features*)
         (bformat t "In llvm-link -> link-type :fasl input-files -> %s%N" input-files))
       (execute-link-fasl output-pathname input-files :input-type input-type))
      ((eq link-type :object)
       (when (member :debug-run-clang *features*)
         (bformat t "In llvm-link -> link-type :object input-files -> %s%N" input-files))
       (execute-link-object output-pathname input-files :input-type input-type))
      ((eq link-type :bitcode)
       (link-bitcode-modules output-pathname input-files))
      (t (error "Cannot link format ~a" link-type)))
    (let ((link-time (/ (- (get-internal-real-time) start-time) (float internal-time-units-per-second))))
;;;      (format t "llvm-link link-time -> ~a~%" link-time)
      (incf llvm-sys:*accumulated-clang-link-time* link-time))
    output-pathname))

(export '(llvm-link))


(defun builder (kind destination &rest keywords)
  "This is used by ASDF to build fasl files."
  (declare (optimize (debug 3)))
  (apply 'build-fasl destination keywords))

(export '(builder))


(defun build-fasl (out-file &key lisp-files init-name)
  "Link the object files in lisp-files into a shared library in out-file.
Note: 'object-files' would be a better name than 'lisp-files' - but 'lisp-files' is what asdf provides.
Return the truename of the output file.
NOTE: On Linux it looks like we MUST link all of the bitcode files first into one and then convert that into a fasb.
This is to ensure that the RUN-ALL functions are evaluated in the correct order."
  ;; fixme cracauer - figure out what FreeBSD needs here
  (declare (ignore init-name))
  ;;  (bformat t "cmpbundle.lsp:build-fasl  building fasl for %s from files: %s%N" out-file lisp-files)
  (with-compiler-timer (:message "build-fasl" :report-link-time t :verbose t)
    (let ((bitcode-files (mapcar (lambda (p) (make-pathname :type (core:bitcode-extension) :defaults p))
                                 lisp-files))
          (temp-bitcode-file (make-pathname :type (core:bitcode-extension) :defaults out-file)))
      (link-bitcode-modules temp-bitcode-file bitcode-files)
      (execute-link-fasl out-file (list temp-bitcode-file)))))

(export 'build-fasl)
