
(in-package :cmp)


(defun bundle-type-as-string (bundle-type)
  (cond
    ((eq bundle-type 'kernel) "kernel")
    ((eq bundle-type 'user) "user")
    (t (error "Illegal bundle-type ~a - only kernel or user are allowed" bundle-type))))

(defun bundle-name-as-string (bundle-name)
  (string-downcase (symbol-name bundle-name)))

(defun create-bundle-module (part-pathnames &key (output-pathname +image-pathname+))
  (multiple-value-bind (module function-pass-manager)
      (cmp:create-llvm-module-for-compile-file (pathname-name output-pathname))
    (with-compilation-unit (:override nil
				      :module module
				      :function-pass-manager function-pass-manager)
      (let* ((*compile-file-pathname* output-pathname)
	     (*gv-source-path-name* (jit-make-global-string-ptr (namestring *compile-file-pathname*) "source-pathname")))
	(let ((fn (with-new-function
		      (bundle-func func-env
				   :function-name (jit-function-name output-pathname)
				   :parent-env nil
				   :linkage 'llvm-sys:external-linkage
                                   :function-type +fn-void+
                                   :argument-names nil
                                   )
		    (dolist (part-pathname part-pathnames)
		      (let* ((part-name (pathname-name part-pathname))
			     (gv-part-name (jit-make-global-string-ptr part-name "part-path-name")))
			(bformat t "Adding function to evaluate compiled top level forms for: %s\n" part-name)
			(let* ((efn (llvm-sys:function-create +fn-void+
							      'llvm-sys::external-linkage
							      (jit-function-name part-pathname)
							      *the-module*))
			       (result (car (llvm-sys:get-argument-list bundle-func)))
			       )
			  (irc-intrinsic "invokeFASLLlvmFunctionVoid" efn gv-part-name)))))))
	  (llvm-sys:dump fn)
	  *the-module*)))))


(defun make-bundle-wrapper (parts bundle-name)
  (let* ((module (create-bundle-module parts :output-pathname bundle-name))
	 (wrapper-name (pathname-name bundle-name))
	 (wrapper-pathname (fasl-pathname (make-pathname :name wrapper-name
                                          :type "bc"
                                          :defaults bundle-name)))
	 )
    (bformat t "Writing bundle module to %s\n" (namestring wrapper-pathname))
    (llvm-sys:write-bitcode-to-file module (core:coerce-to-filename wrapper-pathname))
    wrapper-pathname)
  )


(defvar *echo-system* t)
(defun safe-system (cmd)
  (if *echo-system*
      (bformat t "%s\n" cmd))
  (let ((ret (system cmd)))
    (unless (eql ret 0)
      (error "Could not execute command with system: ~s" cmd))))






;;; This function will compile a bitcode file in PART-BITCODE-PATHNAME with clang and put the output in the
;;; same directory as PART-BITCODE-PATHNAME
(defun execute-clang (part-bitcode-pathname)
  (let* ((name (probe-file (make-pathname :type "bc" :defaults part-bitcode-pathname)))
         (file (namestring (probe-file name)))
         (output-file (namestring (make-pathname :type "o" :defaults (probe-file name)))))
    #+target-os-darwin
    (progn
      (safe-system (bformat nil "clang -c -o %s %s" output-file file))
      (return-from execute-clang))
    #+(and :target-os-linux :address-model-64)
    (progn
      (safe-system (bformat nil "llc -filetype=obj -relocation-model=pic -o %s %s" output-file file))
      (return-from execute-clang))
    (error "Add support for running external clang")))



(defun execute-link (bundle-pathname all-part-pathnames)
  #+target-os-darwin
  (let ((part-files (mapcar #'(lambda (pn) (namestring (probe-file (make-pathname :type "o" :defaults pn))))
			    all-part-pathnames))
	(bundle-file (core:coerce-to-filename bundle-pathname)))
    (let ((all-names (make-array 256 :element-type 'character :adjustable t :fill-pointer 0)))
      (dolist (f part-files) (push-string all-names (bformat nil "%s " f)))
      (safe-system (bformat nil "ld -v %s -macosx_version_min 10.7 -flat_namespace -undefined warning -bundle -o %s" all-names bundle-file)))
    (return-from execute-link))
  #+target-os-linux
  (let ((part-files (mapcar #'(lambda (pn) (namestring (probe-file (make-pathname :type "o" :defaults pn))))
			    all-part-pathnames))
	(bundle-file (core:coerce-to-filename bundle-pathname)))
    (let ((all-names (make-array 256 :element-type 'character :adjustable t :fill-pointer 0)))
      (dolist (f part-files) (push-string all-names (bformat nil "%s " f)))
      (safe-system (bformat nil "clang++ -v %s -shared -o %s" all-names bundle-file)))
    (return-from execute-link))
  (error "Add support for this operating system to cmp:execute-link")
  )



;;; Make a .bundle file on OS X or a .so file on linux
;;; Provide the pathnames for the bitcode files (parts-pathnames)
;;; if the bundle-name is _image then it's the one kernel image
;;; If it is anything else then it's a user image
;;;   
(defun make-bundle (parts-pathnames &optional (bundle-name +image-pathname+)
		    &aux (bundle-type (if (eq bundle-name '_image) 'kernel 'user)))
  "Use (bundle-boot _last-file_) to create the files for a bundle - then go to src/lisp/brcl and make-bundle.sh _image"
  (let* ((wrapper-pathname (make-bundle-wrapper parts bundle-name))
	 (wrapper-and-parts-pathnames (cons wrapper-pathname parts-pathnames))
	 (bundle-pathname (make-pathname :name (string-downcase (string bundle-name)) :defaults *image-directory*)))
    (mapc #'(lambda (pn) (execute-clang pn)) wrapper-and-parts-pathnames)
    (execute-link bundle-pathname wrapper-and-parts-pathnames)))



;;;
;;; Gather a list of boot parts as pathnames
;;; Skip over keyword symbols in the boot part lists
;;;
(defun boot-part-pathnames (last-file &key first-file)
  (or first-file (error "You must provide first-file"))
  (mapcan #'(lambda (part) (and (not (keywordp part)) (list (core::get-pathname-with-type part "lsp"))))
	  (core::select-source-files last-file :first-file first-file)))
	  

(defun bundle-boot (&optional (last-file :all) (first-file :base) (bundle-pathname +image-pathname+))
  "Use (bundle-boot _last-file_) to create the files for a bundle - then go to src/lisp/brcl and make-bundle.sh _image"
  (let* ((parts-pathnames (boot-part-pathnames last-file :first-file first-file))
	 (wrapper-fasl-pathname (make-bundle-wrapper parts-pathnames bundle-pathname))
	 (wrapper-and-parts-fasl-pathnames (cons wrapper-fasl-pathname (mapcar (lambda (x) (fasl-pathname x)) parts-pathnames))))
    (mapc #'(lambda (pn) (execute-clang pn)) wrapper-and-parts-fasl-pathnames)
    (execute-link bundle-pathname wrapper-and-parts-fasl-pathnames)))

(export '(make-bundle bundle-boot))




(defun create-module-pass-manager-for-lto (&key output-pathname debug-ir)
  (let* ((pass-manager-builder (llvm-sys:make-pass-manager-builder))
         (pass-manager (llvm-sys:make-pass-manager)))
    (llvm-sys:populate-module-pass-manager pass-manager-builder pass-manager)
    (llvm-sys:populate-ltopass-manager pass-manager-builder pass-manager nil t nil)
#|    (when debug-ir
      (let ((
        (llvm-sys:pass-manager-add pass-manager (llvm-sys:create-debug-irpass nil nil )))
|#
    pass-manager))
         

(defun link-bitcode-modules (part-pathnames &key additional-bitcode-pathnames
                                              (output-pathname +image-pathname+)
                                              debug-ir)
  "Link a bunch of modules together, return the linked module"
  (format t "part-pathnames ~a~%" part-pathnames)
  (let* ((module (create-bundle-module part-pathnames :output-pathname output-pathname))
         (linker (llvm-sys:make-linker module))
         (all-bitcodes (append additional-bitcode-pathnames part-pathnames))
         )
    ;; Don't enforce .bc extension for additional-bitcode-pathnames
    (dolist (part-pn additional-bitcode-pathnames)
      (let* ((bc-file part-pn))
        (format t "Linking ~a~%" bc-file)
        (let* ((part-module (llvm-sys:parse-bitcode-file (namestring (truename bc-file)) *llvm-context*)))
          (multiple-value-bind (failure error-msg)
              (llvm-sys:link-in-module linker part-module)
            (when failure
              (error "While linking additional module: ~a  encountered error: ~a" bc-file error-msg)))
          )))
    (dolist (part-pn part-pathnames)
      (let* ((bc-file (fasl-pathname (make-pathname :type "bc" :defaults part-pn))))
        (format t "Linking ~a~%" bc-file)
        (let* ((part-module (llvm-sys:parse-bitcode-file (namestring (truename bc-file)) *llvm-context*)))
          (multiple-value-bind (failure error-msg)
              (llvm-sys:link-in-module linker part-module)
            (when failure
              (error "While linking part module: ~a  encountered error: ~a" part-pn error-msg)))
          )))
    (format t "Running module pass manager~%")
    (let* ((linked-module (llvm-sys:get-module linker))
           (mpm (create-module-pass-manager-for-lto :output-pathname output-pathname :debug-ir debug-ir)))
      (llvm-sys:write-bitcode-to-file linked-module (core:coerce-to-filename (pathname "_image_test.bc")))
      (llvm-sys:pass-manager-run mpm linked-module)
      linked-module)))



(defun bundle-boot-lto (&optional (last-part :all)
                                  &key (intrinsics-bitcode-path +intrinsics-bitcode-pathname+)
                                  (first-file :base)
                                  (output-pathname +imagelto-pathname+)
                                  debug-ir)
  (let* ((part-pathnames (boot-part-pathnames last-part :first-file first-file))
;;         (bundle-filename (string-downcase (pathname-name output-pathname)))
	 (bundle-bitcode-pathname (fasl-pathname (make-pathname :type "bc" :defaults output-pathname)))
         (module (link-bitcode-modules part-pathnames
                                       :additional-bitcode-pathnames (if intrinsics-bitcode-path
                                                                         (list intrinsics-bitcode-path)
                                                                         nil)
                                       :output-pathname output-pathname
                                       :debug-ir debug-ir))
         )
    (ensure-directories-exist bundle-bitcode-pathname)
    (llvm-sys:write-bitcode-to-file module (core:coerce-to-filename bundle-bitcode-pathname))
    (execute-clang bundle-bitcode-pathname)
    (execute-link output-pathname (list bundle-bitcode-pathname))
    ))
         
(export '(bundle-boot-lto))
