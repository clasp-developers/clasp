hello.exe: hello.c hello-lisp.a
	$(CC) `ecl-config --cflags` -o $@ hello.c hello-lisp.a \
	      `ecl-config --ldflags` -lecl

hello-lisp.a: hello-lisp.lisp
	ecl -norc \
	    -eval '(require :asdf)' \
	    -eval '(push "./" asdf:*central-registry*)' \
	    -eval '(asdf:make-build :hello-lisp :type :static-library :move-here "./$@")' \
	    -eval '(quit)'

clean:
	-rm -f hello-lisp.a hello.exe
