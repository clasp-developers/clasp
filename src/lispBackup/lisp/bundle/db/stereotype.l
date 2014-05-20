

(defClass Stereotype () (_plugName _matches _representative))

(defMethod __init__ Stereotype ( self &key plugName matches representative )
  (assert (notNil plugName) "Must provide :plugName")
  (assert (notNil matches) "Must provide :matches")
  (assert (notNil representative) "Must provide :representative")
  (setq (slot self _plugName) plugName)
  (setq (slot self _matches) matches)
  (setq (slot self _representative) representative))





