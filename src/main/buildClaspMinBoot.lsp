(core:bformat t "Building clasp-min\n")
(core:clean-system nil :no-prompt t :stage "min")
(core:compile-min)
(core:quit)
