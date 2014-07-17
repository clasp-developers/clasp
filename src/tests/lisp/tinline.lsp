

(load-boot :cmp :interp t)
(defparameter *a* 1)
(defmacro ai () `(incf *a*))

(compile 'a '(lambda ()
              (cmp:llvm-inline (result env)
               (let ((fn (cmp:irc-intrinsic "lccGlobalFunction" (cmp:irc-load (cmp:irc-global-symbol 'TEST-FUNCTION env)))))
                 (cmp:irc-varargs-funcall fn result nil nil)
                 (bformat t "result: %s    env: %s   -> %s\n" result env fn)))))

                              
