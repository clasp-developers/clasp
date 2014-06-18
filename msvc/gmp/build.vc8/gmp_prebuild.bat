echo configuring for a %2 bit build
if not exist %1 (echo creating directory %1 && md %1)
if not exist %1\mp_bases.h  (echo creating %1\mp_bases.h && ..\gen-bases header %2 0 >%1\mp_bases.h)
if not exist %1\mp_bases.c  (echo creating %1\mp_bases.c && ..\gen-bases table %2 0 >%1\mp_bases.c)
if not exist %1\fac_ui.h    (echo creating %1\fac_ui.h && ..\gen-fac_ui %2 0 >%1\fac_ui.h)
if not exist %1\fib_table.h (echo creating %1\fib_table.h && ..\gen-fib header %2 0 >%1\fib_table.h)
if not exist %1\fib_table.c (echo creating %1\fib_table.c && ..\gen-fib table %2 0 >%1\fib_table.c)
if not exist %1\perfsqr.h   (echo creating %1\perfsqr.h && ..\gen-psqr %2 0 >%1\perfsqr.h)
