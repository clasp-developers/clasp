(defun compile-init-file (filename &key then-load-it)
  (let* ((source-path (make-path (bformat nil "%s/CANDO/init/%s" (as-string (script-dir)) filename)))
	 (bitcode-path (replace-extension (copy-path source-path) ".bc")))
    (if (and (exists bitcode-path) (> (last-write-time bitcode-path) (last-write-time source-path)))
	(progn
	  (bformat t "Skipping compilation of %s - it's bitcode file is more recent\n" (as-string source-path))
	  (bformat t "\n\n\n!!!!!!!!! Loading the compiled file: %s\n\n\n" (as-string bitcode-path))
	  (bcload (as-string bitcode-path)))
	(progn
	  (bformat t "Compiling %s\n" (as-string source-path))
	  (compile-file (as-string source-path) :output-file (as-string bitcode-path))
	  (when then-load-it
	    (bformat t "\n\n\n!!!!!!!!! Loading newly compiled file: %s\n\n\n" (as-string bitcode-path))
	    (bcload (as-string bitcode-path)))
	  ))))
#|
(bformat t "To speed up later stages: compiling a few functions\n")
(compile 'co:codegen-lexical-var-lookup)
(compile 'co:codegen-var-lookup)
(compile 'co:codegen-symbol)
(compile 'co:codegen-atom)
(bformat t "Single function compilation done\n")
|#

(defparameter *load-it* t)

#|
(compile-init-file "init.lsp")
|#

(compile-init-file "foundation.lsp" :then-load-it *load-it*)
(compile-init-file "defmacro.lsp" :then-load-it *load-it*)
(compile-init-file "evalmacros.lsp" :then-load-it *load-it*)
(compile-init-file "util.lsp" :then-load-it *load-it*)
(compile-init-file "export.lsp" :then-load-it *load-it*)
(compile-init-file "helpfile.lsp" :then-load-it *load-it*)
(compile-init-file "module.lsp" :then-load-it *load-it*)


(compile-init-file "compiler/compile-main.lsp" :then-load-it *load-it*)
(compile-init-file "compiler/llvm-ir.lsp" :then-load-it *load-it*)
(compile-init-file "compiler/compile-var-lookups.lsp" :then-load-it *load-it*)
(compile-init-file "compiler/debuginfo.lsp" :then-load-it *load-it*)
(compile-init-file "compiler/lambda-list.lsp" :then-load-it *load-it*)
(compile-init-file "compiler/compiler.lsp" :then-load-it *load-it*)
(compile-init-file "compiler/compile-file.lsp" :then-load-it *load-it*)
