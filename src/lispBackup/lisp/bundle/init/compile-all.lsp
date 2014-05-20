(defun compile-init-file (filename)
  (let* ((source-path (make-path (bformat nil "%s/CANDO/init/%s" (as-string (script-dir)) filename)))
	 (bitcode-path (replace-extension (copy-path source-path) ".bc")))
    (if (and (exists bitcode-path) (> (last-write-time bitcode-path) (last-write-time source-path)))
	(bformat t "Skipping %s - it's bitcode file is more recent\n" (as-string source-path))
	(progn
	  (bformat t "Compiling %s\n" (as-string source-path))
	  (compile-file (as-string source-path) :output-file (as-string bitcode-path))))))


#|
(compile-init-file "init.lsp")
|#
(compile-init-file "foundation.lsp")
(compile-init-file "defmacro.lsp")
(compile-init-file "evalmacros.lsp")
(compile-init-file "util.lsp")
(compile-init-file "export.lsp")
(compile-init-file "helpfile.lsp")
(compile-init-file "module.lsp")

(compile-init-file "compiler/compile-main.lsp")
(compile-init-file "compiler/llvm-ir.lsp")
(compile-init-file "compiler/debuginfo.lsp")
(compile-init-file "compiler/lambda-list.lsp")
(compile-init-file "compiler/compiler.lsp")
(compile-init-file "compiler/compile-file.lsp")

