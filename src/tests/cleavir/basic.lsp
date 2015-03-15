

(core:test (eq :foo (funcall (cleavir-compile nil '(lambda () :foo)))) "foo")

