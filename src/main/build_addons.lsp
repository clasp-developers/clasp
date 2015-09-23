(progn
  (bformat t "Compiling serve-event - required by Slime\n")
  (core:compile-kernel-file #P"modules/serve-event/serve-event")
  (cmp:link-system-lto (core:build-pathname #P"modules/serve-event/serve-event" "fasl")
                       :lisp-bitcode-files (list (core:build-pathname #P"modules/serve-event/serve-event" "bc")))
  (bformat t "Compiling asdf\n")
  (core:compile-kernel-file #P"modules/asdf/build/asdf")
  (cmp:link-system-lto (core:build-pathname #P"modules/asdf/asdf" "fasl")
                       :lisp-bitcode-files (list (core:build-pathname #P"modules/asdf/build/asdf" "bc")))
  (core:quit))

