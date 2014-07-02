(in-package :sys)

;; Compile all interpreted functions
(eval-when (:execute :load-toplevel)
  (bformat t "In cmpinterpreted.lsp\n")
  (do-all-symbols (s) (when (and s (fboundp s) (core:interpreted-function-p (fdefinition s)))
                        (bformat t "Compiling interpreted function: %s\n" s)
                        (compile s))))
