top_srcdir=..\..
ECLDIR=../package/
ECL=..\package\ecl.exe

all: output.ecl\ansi.log output.ecl\regressions.log

.PHONY: do-ansi do-regressions do-quicklisp cleanup clean-sources update

config.lsp: $(top_srcdir)\src\tests\config.lsp.in ..\c\cut.exe Makefile
	..\c\cut.exe "@builddir@" "./" \
	          "@top_srcdir@" "../.." \
	  < $(top_srcdir)\src\tests\config.lsp.in > config.lsp

output.ecl\ansi.log: config.lsp
	$(MAKE) do-ansi
output.ecl\regressions.log: config.lsp
	$(MAKE) do-regressions

do-ansi: ansi-tests config.lsp
	set ECLDIR=$(ECLDIR)
	$(ECL) -norc -load config.lsp -eval "(ecl-tests::run-ansi-tests)" -eval "(ext:quit)"
do-regressions: regressions config.lsp
	set ECLDIR=$(ECLDIR)
	$(ECL) -norc -load config.lsp -eval "(ecl-tests::run-regressions-tests)" -eval "(ext:quit)"
do-quicklisp: quicklisp config.lsp
	set ECLDIR=$(ECLDIR)
	$(ECL) -norc -load config.lsp -eval "(ecl-tests::run-quicklisp-tests)" -eval "(ext:quit)"

#
# Create directories
#
ansi-tests: config.lsp
	set ECLDIR=$(ECLDIR)
	$(ECL) -norc -load config.lsp -eval "(ecl-tests::ensure-ansi-tests)" -eval "(ext:quit)"
regressions: config.lsp
	set ECLDIR=$(ECLDIR)
	$(ECL) -norc -load config.lsp -eval "(ecl-tests::ensure-regressions)" -eval "(ext:quit)"
quicklisp: config.lsp
	set ECLDIR=$(ECLDIR)
	$(ECL) -norc -load config.lsp -eval "(ecl-tests::ensure-quicklisp)" -eval "(ext:quit)"

#
# Cleanup
#
clean:
	rm -rf output*
clean-sources:
	test -f config.lsp.in || rm -rf bugs
	rm -rf ansi-tests quicklisp
distclean: clean-sources clean
	rm -rf cache

update: clean-sources
	$(MAKE) ansi-tests regressions quicklisp

