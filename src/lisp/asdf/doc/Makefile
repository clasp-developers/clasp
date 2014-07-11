#user=frideau
ifeq (${user},)
userat :=
else
userat := ${user}@
endif
website:=${userat}common-lisp.net:/project/asdf/public_html/
output = asdf.html asdf asdf.pdf asdf.info
webfiles = index.html .htaccess style.css cclan.png lisp-logo120x80.png favicon.ico ${output}
intermediate = asdf.cps asdf.log asdf.vr asdf.aux asdf.fn asdf.toc asdf.vrs \
		asdf.cp asdf.fns asdf.ky asdf.pg asdf.tp asdf.tps

all:	asdf.html asdf.info asdf.pdf manual-html

manual-html: asdf.texinfo
	makeinfo --html asdf.texinfo

asdf.html: asdf.texinfo
	makeinfo --html --no-split --no-headers -o asdf.html asdf.texinfo

asdf.info: asdf.texinfo
	makeinfo asdf.texinfo

asdf.pdf: asdf.texinfo
	texi2pdf asdf.texinfo

website: all
	rsync -av ${webfiles} ${website}

.PHONY: clean veryclean manual-html
clean:
	rm -f ${intermediate}
veryclean: clean
	rm -f asdf.html asdf.pdf asdf.info
	rm -rf asdf/ asdf.t2d/
