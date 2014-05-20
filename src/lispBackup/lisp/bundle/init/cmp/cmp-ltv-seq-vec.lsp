
(defun hash-table-setf-gethash (ht key val)
  (setf (gethash key ht) val))



(defparameter *ltv-vector* (make-hash-table :test #'eql))
(defun create-ltv-atom (atm idx)
  (hash-table-setf-gethash *ltv-vector* atm idx))


(defun check-if-ltv-atom-already-created (atm)
  ;; This will invoke codegen-ltv-XXXX on the atm
  ;; and return the index in the ltv array of the atom
  ;; or nil if it still needs to be created
  (gethash atm *ltv-vector*))






(defparameter *node-table* (make-hash-table :test #'eq))
(defparameter *node-index* 0)

(defun reset-nodes ()
  (setq *node-table* (make-hash-table :test #'eq))
  (setq *node-index* 100))

(defun walk-graph-atom (cur)
  (multiple-value-bind (id foundp)
      (gethash cur *node-table*)
    (unless foundp
      (let ((nidx (check-if-ltv-atom-already-created cur)))
	(if nidx
	    (hash-table-setf-gethash *node-table* cur nidx)
	    (progn
	      (create-atom cur *node-index*)
	      (hash-table-setf-gethash *node-table* cur *node-index*)
	      (incf *node-index*)))))))


(defun walk-graph-cons (cur)
  (multiple-value-bind (id foundp)
      (gethash cur *node-table*)
    (unless foundp
      (create-cons cur *node-index*)
      (hash-table-setf-gethash *node-table* cur *node-index*)
      (incf *node-index*)
      (walk-graph (car cur))
      (walk-graph (cdr cur)))))

(defun walk-graph-vector (cur)
  (multiple-value-bind (id foundp)
      (gethash cur *node-table*)
    (unless foundp
      (create-vector cur *node-index*)
      (hash-table-setf-gethash *node-table* cur *node-index*)
      (incf *node-index*)
      (dotimes (idx (length cur))
	(walk-graph (svref cur idx))))))


(defun walk-graph-array-objects (cur)
  (multiple-value-bind (id foundp)
      (gethash cur *node-table*)
    (unless foundp
      (create-array-objects cur *node-index*)
      (hash-table-setf-gethash *node-table* cur *node-index*)
      (incf *node-index*)
      (dotimes (idx (array-total-size cur))
	(walk-graph (row-major-aref cur idx))))))





(defun walk-graph (cur)
  (cond
    ((consp cur) (walk-graph-cons cur))
    ((vectorp cur) (walk-graph-vector cur))
    ((arrayp cur) (walk-graph-array-objects cur))
    ((atom cur) (walk-graph-atom cur))
    (t (error "Huh?  can't walk-graph ~a" cur))))


(defun create-cons (val nidx)
  (format t "(make-cons (node-ref ~a))~%" nidx))

(defun create-vector (val nidx)
  (format t "(make-vector (node-ref ~a) :size ~a)~%" nidx (length val)))

(defun create-array-objects (val nidx)
  (format t "(make-array (node-ref ~a) :dim ~a)~%" nidx (array-dimensions val)))

(defun create-atom (val nidx)
  (format t "(make-atom (node-ref ~a) ~a)~%" nidx val))


(defun initialize-cons (key val)
  (format t "(rplaca (node-ref ~a) (node-ref ~a))~%" val (gethash (car key) *node-table*))
  (format t "(rplacd (node-ref ~a) (node-ref ~a))~%" val (gethash (cdr key) *node-table*))
  )

(defun initialize-vector (obj nidx)
  (dotimes (i (length obj))
    (format t "(set-elt (node-ref ~a) ~a (node-ref ~a))~%" nidx i (gethash (elt obj i) *node-table*))))

(defun initialize-array-objects (obj nidx)
  (dotimes (i (array-total-size obj))
    (format t "(set-row-major-aref (node-ref ~a) ~a (node-ref ~a))~%" nidx i (gethash (row-major-aref obj i) *node-table*))))

(defun initialize-nodes ()
  (maphash #'(lambda (key val)
	       (cond
		 ((consp key) (initialize-cons key val))
		 ((vectorp key) (initialize-vector key val))
		 ((arrayp key) (initialize-array-objects key val))
		 (t nil)))
	   *node-table*))


(defun generate-code (obj)
  (reset-nodes)
  (walk-graph obj)
  (initialize-nodes))

#|
(generate-code '(a b (c d)))

(create-ltv-atom 'a 1)
(create-ltv-atom 'z 2)
(generate-code '(a b #(x y z)))

(generate-code #.(list 'list #1=''a #1# #1# #1#))

(generate-code #.(let ((a '(w y z )))
		   (rplacd (cddr a) a)
		   (print a)
		   a))


(generate-code (let ((a (copy-list '(s y z))))
		 (rplacd (cddr a) a)
		 a)
)



(defparameter *a* (make-array '(2 3 4)))
(generate-code *a*)

	
|#
