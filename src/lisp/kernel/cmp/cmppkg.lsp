(in-package :cmp)

(export '(+fn-void+
	  +fn-prototype+
	  +fn-prototype-argument-names+
	  jit-function-name
	  *the-module*
	  *cleavir-compile-hook*
	  *cleavir-compile-file-hook*
	  *llvm-context*
	  with-irbuilder
	  *irbuilder*
	  irc-basic-block-create
	  jit-constant-i32
	  irc-ret-void
	  ))
