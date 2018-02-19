


(define-setf-expander values (&rest values &environment env)
  (let ((all-vars '())
	(all-vals '())
	(all-stores '())
	(all-storing-forms '())
	(all-get-forms '()))
    (dolist (item (reverse values))
      (multiple-value-bind (vars vals stores storing-form get-form)
	  (get-setf-expansion item env)
#|	(co:log "item = %s%N" item)
	(co:log "vars = %s%N" vars)
	(co:log "vals = %s%N" vals)
	(co:log "stores = %s%N" stores)
	(co:log "storing-form = %s%N" storing-form)
	(co:log "get-form = %s%N" get-form)
|#
	;; If a place has more than one store variable, the other ones
	;; are set to nil.
	(let ((extra (rest stores)))
	  (unless (endp extra)
	    (setf vars (append extra vars)
		  vals (append (make-list (length extra)) vals)
		  stores (list (first stores)))))
	(setf all-vars (append vars all-vars)
	      all-vals (append vals all-vals)
	      all-stores (append stores all-stores)
	      all-storing-forms (cons storing-form all-storing-forms)
	      all-get-forms (cons get-form all-get-forms))))
    (values all-vars all-vals all-stores `(values ,@all-storing-forms)
	    `(values ,@all-get-forms))))





#|
;;; Proposed extension:
; Expansion of (SETF (VALUES place1 ... placek) form)
; --> (MULTIPLE-VALUE-BIND (dummy1 ... dummyk) form
;       (SETF place1 dummy1 ... placek dummyk)
;       (VALUES dummy1 ... dummyk))
(define-setf-expander VALUES (&environment env &rest subplaces)
  (do ((temps) (vals) (stores)
       (storeforms) (accessforms)
       (placesr subplaces))
      ((atom placesr)
       (setq temps (nreverse temps)
	     vals (nreverse vals)
	     stores (nreverse stores)
	     storeforms (nreverse storeforms)
	     accessforms (nreverse accessforms))
       (values temps
            vals
            stores
            `(VALUES ,@storeforms)
            `(VALUES ,@accessforms)))
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
        (get-setf-expansion (pop placesr) env)
      (setq temps (revappend SM1 temps)
	    vals (revappend SM2 vals)
	    stores (revappend SM3 stores)
	    storeforms (cons SM4 storeforms)
	    accessforms (cons SM5 accessforms)))))
|#
