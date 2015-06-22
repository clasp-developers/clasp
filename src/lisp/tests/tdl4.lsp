;;(declaim (notinline car))
(declaim (inline car))
(LET* ((temp-list '())
          (Y (IF temp-list
                 (CAR (TRULY-THE CONS temp-list))
                 NIL)))
     (DECLARE (IGNORABLE temp-list ))
     (print Y))
