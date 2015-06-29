(bformat t "Compiling serve-event - required by Slime\n")
(compile-file "sys:serve-event;serve-event.lisp"
              :output-file (compile-file-pathname "sys:modules;serve-event;serve-event.lisp" 
                                                  :target-backend (default-target-backend)))
(bformat t "Compiling asdf\n")
(compile-file "sys:kernel;asdf;build;asdf.lisp" 
              :output-file (compile-file-pathname "sys:modules;asdf;asdf.lisp" 
                                                  :target-backend (default-target-backend)))
(core:quit)
