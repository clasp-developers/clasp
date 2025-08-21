(in-package #:clos)

;;; FIXME: Only defined in its own file because (setf atomic-expander) is
;;; defined relatively late.

(mp:define-atomic-expander standard-instance-access (instance location)
    (&rest keys)
  "The requirements of the normal STANDARD-INSTANCE-ACCESS writer
must be met, including that the slot has allocation :instance, and is
bound before the operation.
If there is a CHANGE-CLASS conflicting with this operation the
consequences are not defined."
  (apply #'mp:get-atomic-expansion
         `(core:rack-ref (core:instance-rack ,instance) ,location)
         keys))
(mp:define-atomic-expander funcallable-standard-instance-access
    (instance location) (&rest keys)
  "See STANDARD-INSTANCE-ACCESS for requirements."
  (apply #'mp:get-atomic-expansion
         `(core:rack-ref (core:instance-rack ,instance) ,location)
         keys))

(defun atomic-svuc (order class object slotd)
  (let* ((loc (slot-definition-location slotd))
         (v (ecase (slot-definition-allocation slotd)
              ((:instance)
               (core:atomic-rack-read order (core:instance-rack object) loc))
              ((:class) (core:car-atomic order loc)))))
    (if (si:sl-boundp v)
        v
        (values (slot-unbound class object (slot-definition-name slotd))))))
(defun (setf atomic-svuc) (new order class object slotd)
  (let ((loc (slot-definition-location slotd)))
    (ecase (slot-definition-allocation slotd)
      ((:instance)
       (core:atomic-rack-write order new (core:instance-rack object) loc))
      ((:class) (core:rplaca-atomic order new loc)))))
(defun cas-svuc (order cmp new class obj slotd)
  (let ((loc (slot-definition-location slotd)))
    (ecase (slot-definition-allocation slotd)
      ((:instance)
       (core:cas-rack order cmp new (core:instance-rack obj) loc))
      ((:class) (core:cas-car order cmp new loc)))))

(mp:define-atomic-expander slot-value-using-class (class object slotd)
    (&key order &allow-other-keys)
  "Same requirements as STANDARD-INSTANCE-ACCESS, except the slot can have
allocation :class or other types.
If there is a redefinition of the class layout that affects the slot, that conflicts with this operation, the consequences are not defined.
Also, methods on SLOT-VALUE-USING-CLASS, SLOT-BOUNDP-USING-CLASS, and
(SETF SLOT-VALUE-USING-CLASS) are ignored (not invoked).
In the future, the CAS behavior may be customizable with a generic function."
  (let ((gclass (gensym "CLASS")) (gobj (gensym "OBJECT"))
        (gslotd (gensym "SLOTD")) (cmp (gensym "CMP")) (new (gensym "NEW")))
    (values (list gclass gobj gslotd) (list class object slotd)
            cmp new
            `(atomic-svuc ',order ,gclass ,gobj ,gslotd)
            `(setf (atomic-svuc ',order ,gclass ,gobj ,gslotd) ,new)
            `(cas-svuc ',order ,cmp ,new ,gclass ,gobj ,gslotd))))

(mp:define-atomic-expander slot-value (object slot-name) (&rest keys)
  "See SLOT-VALUE-USING-CLASS documentation for constraints.
If no slot with the given SLOT-NAME exists, SLOT-MISSING will be called,
with operation = mp:cas, and new-value a list of OLD and NEW.
If SLOT-MISSING returns, its primary value is returned."
  (let ((gobject (gensym "OBJECT")) (gsname (gensym "SLOT-NAME"))
        (gslotd (gensym "SLOTD")) (gclass (gensym "CLASS")))
    (multiple-value-bind (vars vals cmpv newv read write cas)
        (apply #'mp:get-atomic-expansion
               `(slot-value-using-class ,gclass ,gobject ,gslotd)
               keys)
      (values (list* gobject gsname gclass gslotd vars)
              (list* object slot-name `(class-of ,gobject)
                     `(find ,gsname (class-slots ,gclass)
                            :key #'slot-definition-name)
                     vals)
              cmpv newv
              `(if ,gslotd ,read (slot-missing ,gclass ,gobject ,gsname 'slot-value))
              `(if ,gslotd ,write (slot-missing ,gclass ,gobject ,gsname 'setf ,newv))
              `(if ,gslotd ,cas (slot-missing ,gclass ,gobject ,gsname 'mp:cas (list ,cmpv ,newv)))))))

;;; Internal use only, but useful.
(mp:define-atomic-expander clos::generic-function-call-history (generic-function)
    (&rest keys &key order environment)
  (let ((gf (gensym "GENERIC-FUNCTION")) (index (gensym "INDEX")))
    (multiple-value-bind (vars vals cmp new read write cas)
        (apply #'mp:get-atomic-expansion
               `(clos:funcallable-standard-instance-access ,gf ,index)
               keys)
      (values (list* gf index vars)
              (list* generic-function
                     `(clos::%gfclass-call-history-loc (class-of ,gf))
                     vals)
              cmp new read write cas))))
