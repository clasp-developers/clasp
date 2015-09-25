(format t "Building clasp full version~%")
(core:clean-system nil :no-prompt t :stage "full")
(core:compile-full)
(core:quit)
