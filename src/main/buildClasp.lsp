(format t "Building brcl full version~%")
(core:clean-boot nil :no-prompt t)
(core:compile-full)
(core:quit)
