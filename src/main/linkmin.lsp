(load-system :start :min)
(let ((bitcode-files (mapcar #'(lambda (x) 
                                 (build-pathname x "bc"))
                             (select-source-files :min :first-file :init :system *init-files*))))
  (cmp:link-system-lto (target-backend-pathname +image-pathname+)
                       ; :intrinsics-bitcode-path nil
                       :lisp-bitcode-files bitcode-files
                       :prologue-form (default-prologue-form)
                       :epilogue-form +minimal-epilogue-form+))
(quit)
