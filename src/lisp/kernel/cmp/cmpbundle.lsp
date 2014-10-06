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

(defvar *echo-system* t)
(defun safe-system (cmd)
  (if *echo-system*
      (bformat t "%s\n" cmd))
  (let ((ret (system cmd)))
    (unless (eql ret 0)
      (error "Could not execute command with system: ~s" cmd))))




(defun generate-compile-command (bitcode-pathname)
  (let* ((bitcode-physical-pathname (translate-logical-pathname bitcode-pathname))
         (file (namestring bitcode-physical-pathname))
         (output-file (namestring (make-pathname :type "o" :defaults bitcode-physical-pathname))))
    #+(and :target-os-linux :address-model-64)
    (return-from generate-compile-command (values (bformat nil "llc -filetype=obj -relocation-model=pic -o %s %s" output-file file) output-file))
    #+(and target-os-darwin use-clang)
    (let* ((clasp-clang-path (core:getenv "CLASP_CLANG_PATH"))
           (clang-executable (if clasp-clang-path
                                 clasp-clang-path
                                 "clang")))
      (return-from generate-compile-command (values (bformat nil "%s -c -o %s %s" clang-executable output-file file) output-file)))
    #+(and target-os-darwin (not use-clang))
    (return-from generate-compile-command (values (bformat nil "llc -filetype=obj -relocation-model=pic -o %s %s" output-file file) output-file))
    (error "Add support for running external clang to cmpbundle.lsp>generate-compile-command on this system")))




;;; This function will compile a bitcode file in PART-BITCODE-PATHNAME with clang and put the output in the
;;; same directory as PART-BITCODE-PATHNAME
(defun generate-object-file (part-bitcode-pathname &key test)
  (multiple-value-bind (compile-command object-filename)
      (generate-compile-command part-bitcode-pathname)
    (if test
        (bformat t "About to evaluate: %s\n" compile-command)
        (safe-system compile-command))
    object-filename))



(defun generate-link-command (all-names bundle-file)
  #+target-os-darwin
  (return-from generate-link-command
    (bformat nil "ld -v %s -macosx_version_min 10.7 -flat_namespace -undefined warning -bundle -o %s" all-names bundle-file))
  #+target-os-linux
  (let* ((clasp-clang-path (core:getenv "CLASP_CLANG_PATH"))
         (clang-executable (if clasp-clang-path
                               clasp-clang-path
                               "clang")))
    (return-from generate-link-command (bformat nil "%s -v %s -shared -o %s" clang-executable all-names bundle-file)))
  (error "Add support for this operating system to cmp:execute-link")
  )


(defun execute-link (bundle-pathname all-part-pathnames &key test)
  (let* ((part-files (mapcar #'(lambda (pn) (namestring (translate-logical-pathname (make-pathname :type "o" :defaults pn))))
                             all-part-pathnames))
         (bundle-file (core:coerce-to-filename bundle-pathname))
         (all-names (make-array 256 :element-type 'character :adjustable t :fill-pointer 0)))
    (dolist (f part-files) (push-string all-names (bformat nil "%s " f)))
    (let ((link-command (generate-link-command all-names bundle-file)))
      (if test
          (bformat t "About to execute: %s\n" link-command)
          (safe-system link-command))))
)



#||
;;; Make a .bundle file on OS X or a .so file on linux
;;; Provide the pathnames for the bitcode files (parts-pathnames)
;;; if the bundle-name is _image then it's the one kernel image
;;; If it is anything else then it's a user image
;;;
(defun make-bundle (parts-pathnames &optional (bundle-name +image-pathname+)
		    &aux (bundle-type (if (eq bundle-name '_image) 'kernel 'user)))
  "Use (link-system _last-file_) to create the files for a bundle - then go to src/lisp/brcl and make-bundle.sh _image"
  (let* ((wrapper-pathname (make-bundle-wrapper parts bundle-name))
	 (wrapper-and-parts-pathnames (cons wrapper-pathname parts-pathnames))
	 (bundle-pathname (make-pathname :name (string-downcase (string bundle-name)) :defaults *image-directory*)))
    (let ((all-object-files (mapc #'(lambda (pn) (execute-clang pn)) wrapper-and-parts-pathnames)))
      (execute-link bundle-pathname all-object-files))))
||#


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

#||
(defun link-system (pathname-destination &key lisp-bitcode-files (target-backend (default-target-backend)) test) ;; &optional (bundle-pathname +image-pathname+))
  "Use (link-system _last-file_) to create the files for a bundle - then go to src/lisp/brcl and make-bundle.sh _image"
  (let* ((core:*target-backend* target-backend)
         (pathname-destination (target-backend-pathname pathname-destination :target-backend target-backend))
         (wrapper-fasl-pathname (make-bundle-wrapper lisp-bitcode-files pathname-destination))
	 (wrapper-and-parts-fasl-pathnames (cons wrapper-fasl-pathname lisp-bitcode-files)))
    (let ((object-files (mapcar #'(lambda (pn) (generate-object-file pn :test test)) wrapper-and-parts-fasl-pathnames)))
      (execute-link pathname-destination object-files :test test))))

(export '(make-bundle link-system))
||#




(defun create-module-pass-manager-for-lto (&key output-pathname debug-ir)
  (let* ((pass-manager-builder (llvm-sys:make-pass-manager-builder))
         (pass-manager (llvm-sys:make-pass-manager)))
;;    (llvm-sys:populate-module-pass-manager pass-manager-builder pass-manager)
    (llvm-sys:populate-ltopass-manager pass-manager-builder pass-manager nil)
;;    (llvm-sys:add-global-boot-functions-size-pass pass-manager)   ;; I do this outside of a module pass
#|    (when debug-ir
      (let ((
        (llvm-sys:pass-manager-add pass-manager (llvm-sys:create-debug-irpass nil nil )))
|#
    pass-manager))


(defun link-bitcode-modules (part-pathnames &key additional-bitcode-pathnames
                                              (output-pathname +image-pathname+)
                                              prologue-module
                                              epilogue-module
                                              debug-ir
                             &aux conditions)
  "Link a bunch of modules together, return the linked module"
  (format t "part-pathnames ~a~%" part-pathnames)
  (with-compiler-env (conditions)
    (multiple-value-bind (module function-pass-manager)
        (cmp:create-llvm-module-for-compile-file (pathname-name output-pathname))
      (with-compilation-unit ()
        (with-module (:module module
                              :function-pass-manager function-pass-manager)
          (let* ((*compile-file-pathname* output-pathname)
                 (*compile-file-truename* (translate-logical-pathname *compile-file-pathname*))
                 (*gv-source-path-name* (jit-make-global-string-ptr (namestring output-pathname) "source-pathname"))
                 (*gv-source-file-info-handle* (llvm-sys:make-global-variable *the-module*
                                                                              +i32+ ; type
                                                                              nil ; constant
                                                                              'llvm-sys:internal-linkage
                                                                              (jit-constant-i32 -1)
                                                                              "source-file-info-handle"))
                 (bcnum 0))
            (let ((linker (llvm-sys:make-linker *the-module*)))
              ;; Don't enforce .bc extension for additional-bitcode-pathnames
              (if prologue-module
                  (progn
                    (bformat t "Linking prologue-form\n")
                    (remove-main-function-if-exists prologue-module)
                    (llvm-sys:link-in-module linker prologue-module)))
              (dolist (part-pn additional-bitcode-pathnames)
                (let* ((bc-file part-pn))
                  (format t "Linking ~a~%" bc-file)
                  (let* ((part-module (llvm-sys:parse-bitcode-file (namestring (truename bc-file)) *llvm-context*)))
                    (remove-main-function-if-exists part-module) ;; Remove the ClaspMain FN if it exists
                    (multiple-value-bind (failure error-msg)
                        (llvm-sys:link-in-module linker part-module)
                      (when failure
                        (error "While linking additional module: ~a  encountered error: ~a" bc-file error-msg))
                      ))))
              (dolist (part-pn part-pathnames)
                (let* ((bc-file (make-pathname :type "bc" :defaults part-pn)))
                  (format t "Linking ~a~%" bc-file)
                  (let* ((part-module (llvm-sys:parse-bitcode-file (namestring (truename bc-file)) *llvm-context*)))
                    (remove-main-function-if-exists part-module) ;; Remove the ClaspMain FN if it exists
                    (multiple-value-bind (failure error-msg)
                        (llvm-sys:link-in-module linker part-module)
                      (when failure
                        (error "While linking part module: ~a  encountered error: ~a" part-pn error-msg))))))
              (if epilogue-module
                  (progn
                    (bformat t "Linking epilogue-form\n")
                    (remove-main-function-if-exists epilogue-module)
                    (llvm-sys:link-in-module linker epilogue-module)))
              (reset-global-boot-functions-name-size *the-module*)
              (add-main-function *the-module*) ;; Here add the main function
              (llvm-sys:write-bitcode-to-file *the-module* (core:coerce-to-filename (pathname "image_test_prepass.bc")))
              (let* ((mpm (create-module-pass-manager-for-lto :output-pathname output-pathname :debug-ir debug-ir)))
                (format t "Running link time optimization module pass manager~%")
                (llvm-sys:pass-manager-run mpm *the-module*))
              *the-module*)))))))

#||
(load-system :start :cmp :interp t)
(compile-file "tiny1.lsp")
(compile-file "tiny2.lsp")
(cmp:link-system-lto #P"tiny.bundle" :lisp-bitcode-files (list #P"tiny1.bc" #P"tiny2.bc"))
||#

(defun link-system-lto (output-pathname
                        &key (intrinsics-bitcode-path +intrinsics-bitcode-pathname+)
                          lisp-bitcode-files
                          prologue-form
                          epilogue-form
                          debug-ir
                          (target-backend (default-target-backend)))
  (let* ((*target-backend* target-backend)
         (output-pathname (pathname output-pathname))
         (part-pathnames lisp-bitcode-files)
         ;;         (bundle-filename (string-downcase (pathname-name output-pathname)))
	 (bundle-bitcode-pathname (make-pathname :type "bc" :defaults output-pathname))
         (prologue-module (if prologue-form (compile-form-into-module prologue-form "prologueForm")))
         (epilogue-module (if epilogue-form (compile-form-into-module epilogue-form "epilogueForm")))
         (module (link-bitcode-modules part-pathnames
                                       :additional-bitcode-pathnames (if intrinsics-bitcode-path
                                                                         (list intrinsics-bitcode-path)
                                                                         nil)
                                       :output-pathname output-pathname
                                       :prologue-module prologue-module
                                       :epilogue-module epilogue-module
                                       :debug-ir debug-ir)))
    (ensure-directories-exist bundle-bitcode-pathname)
    (llvm-sys:write-bitcode-to-file module (core:coerce-to-filename bundle-bitcode-pathname))
    (generate-object-file bundle-bitcode-pathname)
    (execute-link output-pathname (list bundle-bitcode-pathname))))

(export '(link-system-lto))


(defun builder (kind destination &rest keywords)
  (bformat t "builder kind[%s] destination[%s] keywords: %s\n" kind destination keywords)
  (break "Check parameters"))

(export '(builder))
