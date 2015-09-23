(progn
  (bformat t "Compiling serve-event - required by Slime\n")
  (core:compile-kernel-file #P"serve-event/serve-event")
  (core:link-kernel-file #P"serve-event/serve-event")
  (bformat t "Compiling asdf\n")
  (core:compile-kernel-file #P"kernel/asdf/build/asdf")
  (core:link-kernel-file #P"kernel/asdf/build/asdf")
  (core:quit))

