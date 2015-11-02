(progn
  (bformat t "Compiling serve-event - required by Slime\n")
  (core:compile-kernel-file #P"sys:modules;serve-event;serve-event" :force-recompile t)
  (cmp:link-system-lto (core:build-pathname #P"sys:modules;serve-event;serve-event" :fasl)
                       :lisp-bitcode-files (list (core:build-pathname #P"sys:modules;serve-event;serve-event" :bc)))
  (bformat t "Compiling asdf\n")
  (core:compile-kernel-file #P"sys:modules;asdf;build;asdf" :force-recompile t)
  (cmp:link-system-lto (core:build-pathname #P"sys:modules;asdf;asdf" :fasl)
                       :lisp-bitcode-files (list (core:build-pathname #P"sys:modules;asdf;build;asdf" :bc)))
  (core:quit))

