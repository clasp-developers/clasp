(in-package #:koga)

(defun help (&rest initargs)
  (declare (ignore initargs))
  (write-line "USAGE: ./koga [options]

OPTIONS:")
  (closer-mop:finalize-inheritance (find-class 'configuration))
  (loop for slot in (closer-mop:class-slots (find-class 'configuration))
        for type = (closer-mop:slot-definition-type slot)
        for initarg = (first (closer-mop:slot-definition-initargs slot))
        for initform = (closer-mop:slot-definition-initform slot)
        for doc = (documentation slot t)
        if (and initarg (subtypep 'boolean type))
          do (format t "  --~:[~;no-~]~(~a~)~%~@[      ~a~%~]"
                     (eq t initform) initarg doc)
        else if initarg
          do (format t "  --~(~a~)=<value>~%~@[      ~a~%~]"
                     initarg doc))
  (write-line "
TARGETS:
If no target is given to ninja then the default is cclasp-boehmprecise.

  analyze: Run the static analyzer on the boehm variant.

  test: Run the regression tests on the boehmprecise variant.

  ansi-test: Run the ANSI on the boehmprecise variant.

  test-random-integer: Run the Paul F. Dietz random integer test.
    The following environment variables are available to control the test
    parameters:
      random_int_size: the size of the expression
      random_int_variables: the number of variables
      random_int_iterations: the number of iterations
    Example usage:
      random_int_size=5 random_int_variables=2 random_int_iterations=1000 \\
        ninja -C build test-random-integer

  install: Install the default target.

  [riabcs]clasp-boehm[precise][-d]: Build clasp to the specified stage [riabcd].
    Unless you are developing clasp the \"c\" stage is generally what you want.
    The -d suffix will enable debug mode.

  analyze-boehm[-d]: Run the static analyzer on the specified variant

  test-boehm[precise][-d]: Run the regression tests on the specified variant."))
