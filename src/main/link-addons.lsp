(progn
  (bformat t "Linking serve-event - required by Slime\n")
  (cmp:link-system-lto (core:build-pathname #P"modules/serve-event/serve-event" "fasl")
                       :lisp-bitcode-files (list (core:build-pathname #P"modules/serve-event/serve-event" "bc")))
  (bformat t "Linking asdf\n")
  (cmp:link-system-lto (core:build-pathname #P"modules/asdf/asdf" "fasl")
                       :lisp-bitcode-files (list (core:build-pathname #P"modules/asdf/build/asdf" "bc")))
  (core:quit))

