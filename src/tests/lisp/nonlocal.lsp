#|
(tagbody
   a
   1
   (tagbody
      aa
      11
      (go b)
      12)
   b
   2)
|#

(defun verify (x y)
  (if (eql x y)
      (print "Verified")
      (print "failed")))



(verify 2 (block a
            (tagbody
             a
             1
               (go b)
             b
               (return-from a 2))))


(verify 2 (block a
            (tagbody
               a
               1
               (tagbody
                  aa
                  11
                  (go b)
                  22)
               b
               (return-from a 2))))

(defun recurse1 (depth fn)
  (if (eql depth 0 )
      (progn
        (bformat t "   About to funcall\n")
        (funcall fn))
      (progn
        (bformat t "About to recurse %d\n" depth)
        (recurse1 (- depth 1) fn))))


(verify 'done (block a
                (tagbody
                 a
                 1
                   (tagbody
                    aa
                    11
                      (recurse1 10 #'(lambda () (go b)))
                    bb
                    22)
                 b
                   (return-from a 'done))))

(print "Done")
