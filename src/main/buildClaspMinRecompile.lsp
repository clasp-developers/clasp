(core:bformat t "Building clasp-min\n")
(core:clean-system nil :no-prompt t)
(core:compile-min-recompile)
(core:quit)
