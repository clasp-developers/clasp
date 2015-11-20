;;(declaim (notinline car))
(declaim (inline car))
(LET* ((temp-list '())
          (Y (IF temp-list
                 (PROG1 (CAR temp-list)
                   2 )
                 99)))
  (print Y))
