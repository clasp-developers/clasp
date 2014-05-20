
(def-sd-generic change 0)

(def-sd-method change ((self double-float)) (* 2.0 self))

(def-sd-method change ((self integer)) (/ self 2))

