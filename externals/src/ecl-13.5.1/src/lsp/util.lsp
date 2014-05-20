;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
(in-package "COMPILER")
(defvar file-list
  '( "defmacro.lsp" "evalmacros.lsp" "top.lsp"
	"module.lsp" "predlib.lsp" "setf.lsp"
	"arraylib.lsp" "assert.lsp" "defstruct.lsp"
	"describe.lsp" "iolib.lsp" "listlib.lsp"
	"mislib.lsp" "numlib.lsp" "packlib.lsp"
	"seq.lsp" "seqlib.lsp" "trace.lsp" 
	"thread.lsp" "loop.lsp"))

(load "../cmp/make-declare.lsp")

(dolist (file file-list)
	(sys::proclaim-file file "/tmp/try.lsp"))

