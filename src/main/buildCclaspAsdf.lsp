(format t "Building asdf full version~%")
(bformat t "Compiling asdf\n")
(compile-file "sys:kernel;asdf;build;asdf.lisp" 
              :output-file (compile-file-pathname "sys:modules;asdf;asdf.lisp" 
                                                  :target-backend (default-target-backend))))
(core:quit)
