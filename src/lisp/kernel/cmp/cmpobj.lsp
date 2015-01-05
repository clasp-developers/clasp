
(in-package :cmp)


(defun generate-obj-asm (module output-stream &key file-type (reloc-model 'llvm-sys:reloc-model-default))
  (let* ((triple-string (llvm-sys:get-target-triple module))
	 (normalized-triple-string (llvm-sys:triple-normalize triple-string))
	 (triple (llvm-sys:make-triple normalized-triple-string))
	 (target-options (llvm-sys:make-target-options))
	 )
    (llvm-sys:setf-no-frame-pointer-elim target-options t)
    (multiple-value-bind (target msg)
	(llvm-sys:target-registry-lookup-target "" triple)
      (unless target (error msg))
      (let* ((target-machine (llvm-sys:create-target-machine target
							     (llvm-sys:get-triple triple)
							     ""
							     ""
							     target-options
							     reloc-model
							     'llvm-sys:code-model-default
							     'llvm-sys:code-gen-opt-default ))
	     (pm (llvm-sys:make-pass-manager))
	     (tli (llvm-sys:make-target-library-info triple))
	     (data-layout-pass (llvm-sys:make-data-layout-pass))
	     (target-subtarget-info (llvm-sys:get-subtarget-impl target-machine))
	     (data-layout (llvm-sys:get-data-layout target-subtarget-info))
	     )
	(if data-layout (llvm-sys:set-data-layout module data-layout))
	(llvm-sys:pass-manager-add pm tli)
	(llvm-sys:pass-manager-add pm data-layout-pass)
	(llvm-sys:add-passes-to-emit-file-and-run-pass-manager target-machine pm output-stream file-type module)))))


(export '(disassemble-asm bitcode-to-obj-file))

(defun bitcode-to-obj-file (input-filename output-filename &key reloc-model)
  "Generate a .o file from a bitcode file"
  (or reloc-model (error "You must provide reloc-model"))
  (format t "Generating object file  ~a  -->  ~a  reloc-model: ~a~%" input-filename output-filename reloc-model)
  (let* ((module (llvm-sys:parse-bitcode-file (namestring (truename input-filename)) cmp:*llvm-context*)))
    (with-open-file (fout output-filename :direction :output)
      (generate-obj-asm module fout :file-type 'llvm-sys:code-gen-file-type-object-file :reloc-model reloc-model))))


(defun disassemble-asm (module)
  (with-output-to-string (sout)
    (generate-obj-asm module fout 'llvm-sys:code-gen-file-type-assembly-file module)))
