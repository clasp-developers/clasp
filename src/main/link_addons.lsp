(progn
  (bformat t "Linking serve-event\n")
  (cmp:llvm-link (core:build-pathname #P"modules/serve-event/serve-event" "fasl")
                       :lisp-bitcode-files (list (core:build-pathname #P"modules/serve-event/serve-event" "bc")))
  (bformat t "Compiling asdf\n")
  (cmp:llvm-link (core:build-pathname #P"modules/asdf/asdf" "fasl")
                       :lisp-bitcode-files (list (core:build-pathname #P"modules/asdf/build/asdf" "bc")))
  (core:quit))

