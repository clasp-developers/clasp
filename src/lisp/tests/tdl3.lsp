;;(declaim (notinline car))
(declaim (inline car))
(LET* ((temp-list '())
       (Y (IF temp-list
              (LET ((p1val (CAR TEMP-LIST))) p1val)
              99)))
  (print Y))
