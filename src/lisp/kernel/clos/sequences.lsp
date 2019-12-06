;;;; Code for the extensible sequences protocol.
;;;; System functionality is in lsp/seq{,lib,macros}.lsp.
;;;; It calls the generic functions here in the non-vector-list case.

(in-package "CLOS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Core protocol
;;; Must be defined by extension programmers for their sequences to work.
;;;

(defgeneric sequence:elt (sequence index))

(defgeneric (setf sequence:elt) (new sequence index)
  (:argument-precedence-order sequence index new))

(defgeneric sequence:length (sequence))

(defgeneric sequence:make-sequence-like
    (sequence length &key initial-element initial-contents))

(defgeneric sequence:adjust-sequence
    (sequence length &key initial-element initial-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Iterator protocol
;;; Optional.
;;; If not defined by extension programmers, the CLOS iterator protocol is used.
;;;

(defgeneric sequence:make-sequence-iterator (sequence &key from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLOS iterator protocol
;;; Optional.
;;; If not defined by extension programmers, a default implementation in terms
;;; of the core protocol will be used. This implementation will be reasonably
;;; efficient provided the sequence has efficient random access.
;;; If the full iterator protocol (i.e. make-sequence-iterator) is customized,
;;; the CLOS iterator protocol does not need to be defined. It is not used
;;; by any sequence functions directly.
;;;

(defgeneric sequence:make-simple-sequence-iterator
    (sequence &key from-end start end))

(defgeneric sequence:iterator-step (sequence iterator from-end))
(defgeneric sequence:iterator-endp (sequence iterator limit from-end))
(defgeneric sequence:iterator-element (sequence iterator))
(defgeneric (setf sequence:iterator-element) (new sequence iterator)
  (:argument-precedence-order sequence iterator new))
(defgeneric sequence:iterator-index (sequence iterator))
(defgeneric sequence:iterator-copy (sequence iterator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definition of the iterator protocol in terms of the CLOS iterator protocol
;;;

(defmethod sequence:make-sequence-iterator
    ((sequence sequence) &key from-end start end)
  (multiple-value-bind (iterator limit from-end)
      (sequence:make-simple-sequence-iterator
       sequence :from-end from-end :start start :end end)
    (values iterator limit from-end
            #'sequence:iterator-step #'sequence:iterator-endp
            #'sequence:iterator-element #'(setf sequence:iterator-element)
            #'sequence:iterator-index #'sequence:iterator-copy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default iterator protocol
;;; Iterators and limits are indices.
;;;

(defmethod sequence:iterator-step ((sequence sequence) iterator from-end)
  (if from-end (1- iterator) (1+ iterator)))
(defmethod sequence:iterator-endp ((sequence sequence) iterator limit from-end)
  (= iterator limit))
(defmethod sequence:iterator-element ((sequence sequence) iterator)
  (sequence:elt sequence iterator))
(defmethod (setf sequence:iterator-element) (new (sequence sequence) iterator)
  (setf (sequence:elt sequence iterator) new))
(defmethod sequence:iterator-index ((sequence sequence) iterator)
  iterator)
(defmethod sequence:iterator-copy ((sequence sequence) iterator)
  iterator)

(defmethod sequence:make-simple-sequence-iterator
    ((sequence sequence) &key from-end (start 0) end)
  (let ((end (or end (length sequence))))
    (if from-end
        (values (1- end) (1- start) from-end)
        (values start end from-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Core and iterator protocol specialization for LIST and VECTOR
;;;

(defun iep-and-icp ()
  (error "Supplied both ~s and ~s to ~s"
         :initial-element :initial-contents 'sequence:make-sequence-like))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; List
;;;

(defmethod sequence:elt ((sequence list) index) (nth index sequence))
(defmethod (setf sequence:elt) (new (sequence list) index)
  (setf (nth index sequence) new))
(defmethod sequence:length ((sequence null)) 0)
(defmethod sequence:length ((sequence cons)) (core:cons-length sequence))

(defmethod sequence:make-sequence-like
    ((sequence list) length
     &key (initial-element nil iep) (initial-contents nil icp))
  (cond ((and iep icp) (iep-and-icp))
        (iep (make-list length :initial-element initial-element))
        (icp (unless (= (length initial-contents) length)
               (error "Length mismatch in ~s" 'sequence:make-sequence-like))
             (replace (make-list length) initial-contents))
        (t (make-list length))))

(defmethod sequence:adjust-sequence
    ((s list) length &key initial-element (initial-contents nil icp))
  (if (zerop length)
      nil
      (let ((olength (length s)))
        (cond
          ((= length olength) (if icp (replace s initial-contents) s))
          ((< length olength) ; shorten the list
           (rplacd (nthcdr (1- length) s) nil)
           (if icp (replace s initial-contents) s))
          ((null s) ; make a new list
           (let ((r (make-list length :initial-element initial-element)))
             (if icp (replace r initial-contents) r)))
          (t ; lengthen list
           (rplacd (nthcdr (1- olength) s)
                   (make-list (- length olength)
                              :initial-element initial-element))
           (if icp (replace s initial-contents) s))))))

(defmethod sequence:make-sequence-iterator
    ((sequence list) &key from-end start end)
  (core::make-list-iterator sequence from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector
;;;

(defmethod sequence:elt ((sequence vector) index) (aref sequence index))
(defmethod (setf sequence:elt) (new (sequence vector) index)
  (setf (aref sequence index) new))
(defmethod sequence:length ((sequence vector)) (core::vector-length sequence))

(defmethod sequence:make-sequence-like
    ((sequence vector) length
     &key (initial-element nil iep) (initial-contents nil icp))
  (cond ((and iep icp) (iep-and-icp))
        (iep (make-array length :element-type (array-element-type sequence)
                                :initial-element initial-element))
        (icp (make-array length :element-type (array-element-type sequence)
                                :initial-element initial-element))
        (t (make-array length :element-type (array-element-type sequence)))))

(defmethod sequence:adjust-sequence
    ((sequence vector) length
     &rest args &key initial-element (initial-contents nil icp))
  (declare (ignore initial-element))
  (cond ((and (array-has-fill-pointer-p sequence)
              (>= (array-total-size sequence) length))
         (setf (fill-pointer sequence) length)
         (if icp (replace sequence initial-contents) sequence))
        ((= (length sequence) length)
         (if icp (replace sequence initial-contents) sequence))
        (t (apply #'adjust-array sequence length args))))

(defmethod sequence:make-sequence-iterator
    ((sequence vector) &key from-end start end)
  (core::make-vector-iterator sequence from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived functions
;;;

(defgeneric sequence:emptyp (sequence)
  (:method ((sequence sequence)) (zerop (length sequence)))
  (:method ((sequence null)) t)
  (:method ((sequence cons)) nil))
