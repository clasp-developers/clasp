(format t "Building clasp full version~%")
(core:clean-system nil :no-prompt t :target-backend "full-boehm")
(core:compile-full)
(core:quit)
