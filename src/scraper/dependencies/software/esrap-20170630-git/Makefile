.PHONY: doc web wc clean all test

all:
	echo "Targets: clean, wc, doc, test, web"

clean:
	rm -f *.fasl *~
	make -C doc clean
	make -C web clean

wc:
	wc -l *.lisp

doc:
	make -C doc

web: doc
	make -C web

gh-pages: web
	rm -rf web-tmp
	mv web web-tmp
	git checkout gh-pages
	cp web-tmp/index.html .
	git commit -a -c master
	mv web-tmp web
	git checkout -f master
