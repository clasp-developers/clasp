(core:bformat t "Building a few files\n")
(core:clean-boot nil :no-prompt t)
(time (core:compile-boot :start :tiny :recompile t))
(core:quit)
