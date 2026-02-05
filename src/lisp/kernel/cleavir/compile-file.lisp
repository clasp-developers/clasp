(in-package #:cmp)

(defun generate-obj-asm-stream (module output-stream file-type reloc-model &key (output-type *default-output-type*))
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
                                                                (llvm-sys:get-triple triple)
                                                                ""
                                                                ""
                                                                target-options
                                                                reloc-model
                                                                (code-model :jit nil :output-type output-type)
                                                                'llvm-sys:code-gen-opt-default
                                                                nil)
                                output-stream
                                nil ; dwo-stream for dwarf objects
                                file-type module)))))

(defun build-extension (type)
  (cond ((or (eq type :bytecode)
             (member :bytecode *features*))
         "fasl")
        ((eq type :faso)
         "faso")
        ((eq type :fasoll)
         "fasoll")
        ((eq type :fasobc)
         "fasobc")
        (t
         (error "Unsupported build-extension type ~a" type))))
