.PHONY: clean html include doc

doc: html

clean:
	rm -rf include
	rm -f  *.pdf *.html *.info
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr

include:
	sbcl                                                                                                    \
	--noinform --disable-ldb --lose-on-corruption                                                           \
	--no-userinit --disable-debugger                                                                        \
	--eval '(require :asdf)'                                                                                \
	--eval '(let ((asdf:*central-registry* (cons #p"../" asdf:*central-registry*))) (require :esrap))'      \
	--load docstrings.lisp                                                                                  \
	--eval '(sb-texinfo:generate-includes "include/" (list :esrap) :base-package :esrap)'                   \
	--quit

esrap.html: esrap.texinfo style.css docstrings.lisp ../*.lisp ../*.asd
	make include
	makeinfo --html --no-split --css-include=style.css esrap.texinfo

html: esrap.html
