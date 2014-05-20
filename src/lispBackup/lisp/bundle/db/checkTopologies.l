


(defClass MetaOutPlugsWithSameName ()(_OutPlugs )
  (method __init__ (self )
	  (setq (slot self _OutPlugs )()))

  (method hasMetaOutPlug (self metaOutPlug )
	  (foreach mop (slot self _OutPlugs ) ;removeComment
		   (ifTrue (== mop metaOutPlug )
			   (return-from hasMetaOutPlug true )))
	  false)


  (method addMetaOutPlug (self metaOutPlug )
	  (ifFalse (hasMetaOutPlug self metaOutPlug )
		   (setq (slot self _OutPlugs )(Cons metaOutPlug (slot self _OutPlugs )))))


  ;;
  ;; All MetaOutPlugs with the same name must make the same number of bonds
  ;;
  (method allMetaOutPlugsAreConsistent (self fullReport )
	  (prognLog "Starting allMetaOutPlugsAreConsistent"
		    (let* ((firstMetaOutPlug (car (slot self _OutPlugs)))
			   (restOfMetaOutPlugs (cdr (slot self _OutPlugs)))
			   (numberOfBonds (numberOfBonds firstMetaOutPlug)))
		      (for otherPlug restOfMetaOutPlugs
			   (when (not (== numberOfBonds (numberOfBonds otherPlug)))
			     (raise (LispError (% "There are MetaOutPlugs with the same name that have different bonding" (getName firstMetaOutPlug))))))
		      true)))
  )

;;
;; As MetaOutPlugs are added to the MetaOutPlugCollection
;; They are put into an ObjectDictionary indexed by the MetaOutPlug's name
;; 
(defClass MetaOutPlugCollection () (_MetaOutPlugsGroupedByName)
  (method __init__ (self )
	  (setq (slot self _MetaOutPlugsGroupedByName ) (Binder)))

  (method getMetaOutPlugsGroupedByName (self )
	  (slot self _MetaOutPlugsGroupedByName ))

  (method addMetaOutPlug (self one )
	  (let* ((name (getName one ))
		(group (if (contains (slot self _MetaOutPlugsGroupedByName) name )
			   (lookup (slot self _MetaOutPlugsGroupedByName) name)
			 (extend (slot self _MetaOutPlugsGroupedByName) name (MetaOutPlugsWithSameName)))))
	    (addMetaOutPlug group one ))))


