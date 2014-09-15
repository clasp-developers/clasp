(core:bformat t "Building a few files\n")
(core:clean-system nil :no-prompt t)
(time (core:compile-system :start :tiny :recompile t))
(core:quit)
