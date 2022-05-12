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
If no target is given to ninja then the default is cclasp-boehm if the build
does not include Cando. If the build does include Cando then the default target
is dclasp-boehm.

  analyze: Run the static analyzer on the boehm variant. Builds preciseprep as
    needed.

  test: Run the regression tests on the boehm variant.

  install: Install the default target.

  [riabcd]clasp-boehm[-d]: Build clasp to the specified stage [riabcd]. Unless
    you are developing clasp the \"c\" stage is generally what you want. The -d
    suffix will enable debug mode.

  analyze-boehm[-d]: Run the static analyzer on the specified variant. Builds
    preciseprep as needed.

  test-boehm[-d]: Run the regression tests on the specified variant."))