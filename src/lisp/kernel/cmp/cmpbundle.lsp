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


#||
(defun make-bundle-wrapper (parts bundle-name)
  (let* ((module (create-bundle-module parts :output-pathname bundle-name))
	 (wrapper-name (pathname-name bundle-name))
	 (wrapper-pathname (make-pathname :name wrapper-name
                                          :type "bc"
                                          :defaults bundle-name)))
    (bformat t "Writing bundle module to %s\n" (namestring wrapper-pathname))
    (llvm-sys:write-bitcode-to-file module (core:coerce-to-filename wrapper-pathname))
    wrapper-pathname)
  )
||#

(defun as-shell-command (list-of-args)
  (with-output-to-string (sout)
    (princ (car list-of-args) sout)
    (dolist (c (cdr list-of-args))
      (bformat sout " %s" c))))

(defvar *echo-system* nil)
(defun safe-system (cmd-list &key output-file-name)
  (if *echo-system*
      (bformat t "safe-system: %s\n" cmd-list))
  (multiple-value-bind (retval error-message)
      (ext:vfork-execvp cmd-list)
    (when retval
      (error "Could not execute command with ext:vfork-execvp with ~s~%  return-value: ~d  error-message: ~s~%" cmd-list retval error-message)))
  (when output-file-name
    (unless (probe-file output-file-name)
      (error "The file ~a was not created by shell command: ~a" output-file-name (as-shell-command cmd-list)))))

;;; This function will compile a bitcode file in PART-BITCODE-PATHNAME with clang and put the output in the
;;; same directory as PART-BITCODE-PATHNAME
(defun generate-object-file (part-bitcode-pathname &key test)
  (let ((output-pathname (compile-file-pathname part-bitcode-pathname :output-type :object))
        (reloc-model (cond
                      ((member :target-os-linux *features*) 'llvm-sys:reloc-model-pic-)
                      (t 'llvm-sys:reloc-model-default))))
    (bitcode-to-obj-file part-bitcode-pathname output-pathname :reloc-model reloc-model)
    (truename output-pathname)))

(defun ensure-string (name)
  (cond
    ((pathnamep name) (core:coerce-to-filename name))
    ((stringp name) name)
    (t (string name))))

(in-package :ext)

(defun run-ld (args &key output-file-name)
  (cmp:safe-system `("ld" ,@args :output-file-name output-file-name)))

(export 'run-ld)

(in-package :cmp)

(defun execute-link-fasl (in-bundle-file in-all-names &key (link-type :executable))
  ;; options are a list of strings like (list "-v")
  (let ((options nil)
        (all-object-files (mapcar (lambda (n)
                                    (ensure-string n))
                                  (if (listp in-all-names)
                                      in-all-names
                                      (list in-all-names))))
        (bundle-file (ensure-string in-bundle-file)))
    (cond
      ((member :target-os-darwin *features*)
       (ext:run-clang `(,@options
                        ,@all-object-files
;;;                                 "-macosx_version_min" "10.10"
                        "-flto"
                        "-flat_namespace" 
                        "-undefined" "warning"
                        "-bundle"
                        "-o"
                        ,bundle-file)
                      :output-file-name bundle-file))
      ((member :target-os-linux *features*)
       ;; Linux needs to use clang to link
       (ext:run-clang `(,@options
                        ,@all-object-files
                        "-shared"
                        "-o"
                        ,bundle-file)
                      :output-file-name bundle-file))
      (t (error "Add support for this operating system to cmp:generate-link-command")))
    (truename in-bundle-file)))

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
                          ,@all-names
                          #+(or)"-v"
                          "-flto"
                          "-Wl,-export_dynamic"
                          ,@link-flags
                          ,(bformat nil "-Wl,-lto_library,%s/libLTO.%s" link-lib-path library-extension)
                          ,(bformat nil "-Wl,-object_path_lto,%s.lto.o" exec-file)
                          "-o"
                          ,exec-file)
                        :output-file-name exec-file))
        ((member :target-os-linux *features*)
         ;; Linux needs to use clang to link
         (ext:run-clang `(,@options
                          ,@all-names
                          "-v"
                          "-flto"
                          "-Wl,-export_dynamic"
                          ,@link-flags
                          ,(bformat nil "-Wl,-lto_library,%s/libLTO.%s" link-lib-path library-extension)
                          ,(bformat nil "-Wl,-object_path_lto,%s.lto.o" exec-file)
                          "-o"
                          ,exec-file)
                        :output-file-name exec-file))
        (t (error "Add support for this operating system to cmp:generate-link-command"))))
    (truename output-file-name)))

;;;
;;; Gather a list of boot parts as pathnames
;;; Skip over keyword symbols in the boot part lists
;;;
(defun boot-bitcode-pathnames (last-file &key first-file target-backend)
  (or first-file (error "You must provide first-file"))
  (let* ((source-files (mapcan #'(lambda (part) (and (not (keywordp part)) (list (core::get-pathname-with-type part "lsp"))))
                               (core::select-source-files last-file :first-file first-file)))
         (bitcode-files (mapcar (lambda (k) (compile-file-pathname k :target-backend target-backend))
                                source-files)))
    bitcode-files))
(export 'boot-bitcode-pathnames)



(defun llvm-link (output-pathname
                  &key (link-type :fasl)
                    lisp-bitcode-files
                    debug-ir
                    link-time-optimization
                    (target-backend (default-target-backend)))
  (let* ((*target-backend* target-backend)
         (intrinsics-bitcode-path (core:build-intrinsics-bitcode-pathname link-type))
         (all-bitcode (list* intrinsics-bitcode-path lisp-bitcode-files))
         (output-pathname (pathname output-pathname)))
    (if (eq link-type :executable)
        (execute-link-executable output-pathname all-bitcode)
        (execute-link-fasl output-pathname all-bitcode))
    output-pathname))

(export '(llvm-link))


(defun builder (kind destination &rest keywords)
  (bformat t "builder kind[%s] destination[%s] keywords: %s\n" kind destination keywords)
  (break "Check parameters"))

(export '(builder))


(defun build-fasl (out-file &key lisp-files)
  "Link the object files in lisp-files into a shared library in out-file.
Return the truename of the output file"
;;  (bformat t "cmpbundle.lsp:build-fasl  building fasl for %s from files: %s\n" out-file lisp-files)
  (execute-link out-file lisp-files))

(export 'build-fasl)
