(format t "Building clasp full version~%")
(core:clean-boot nil :no-prompt t :target-backend "full-boehm")
(core:compile-full)
(core:quit)
