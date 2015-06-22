;;(declaim (notinline car))
(declaim (inline car))
(LET* ((temp-list '())
          (Y (IF temp-list
                 (PROG1 (CAR temp-list)
                   (SETQ temp-list 2))
                 99)))
     (DECLARE (IGNORABLE temp-list ))
     (print (list "Should be 99 --> " Y)))
