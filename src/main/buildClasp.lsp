(format t "Building clasp full version~%")
(core:clean-system nil :no-prompt t :target-backend (default-target-backend "full"))
(core:compile-full)
(bformat t "Compiling asdf\n")
(compile-file "sys:kernel;asdf;build;asdf.lisp" 
	      :output-file (compile-file-pathname "sys:modules;asdf;asdf.lisp" 
						  :target-backend (default-target-backend)))
(core:quit)
