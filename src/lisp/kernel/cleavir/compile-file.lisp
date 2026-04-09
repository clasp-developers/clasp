(in-package #:cmp)

(defun generate-obj-asm-stream (module output-stream file-type reloc-model &key)
  (with-track-llvm-time
      (let* ((triple-string (llvm-sys:get-target-triple module))
             (normalized-triple-string (llvm-sys:triple-normalize triple-string))
             (triple (llvm-sys:make-triple normalized-triple-string))
             (target-options (llvm-sys:make-target-options)))
        (multiple-value-bind (target msg)
            (llvm-sys:target-registry-lookup-target "" triple)
          (unless target
            (error msg))
          (llvm-sys:emit-module (llvm-sys:create-target-machine target
                                                                #+(or llvm15 llvm16 llvm17 llvm18 llvm19) (llvm-sys:get-triple triple) #-(or llvm16 llvm16 llvm17 llvm18 llvm19) triple
                                                                ""
                                                                ""
                                                                target-options
                                                                reloc-model
                                                                (code-model :jit nil)
                                                                'llvm-sys:code-gen-opt-default
                                                                nil)
                                output-stream
                                nil ; dwo-stream for dwarf objects
                                file-type module)))))
