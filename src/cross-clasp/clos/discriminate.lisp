(in-package #:cross-clasp.clasp.clos)

;;;; Generate a discriminating function.
#|
Because stamps are not stable, we can't generate an efficient search at
build time - we don't know what the stamps will be at runtime. (It would
be nice to figure out how to do that, but I can't think of any easy to
maintain system right now.) So we just generate some basic CASEs. These
are a little inefficient, but if need be they could be updated into
something more optimized once the system is up.

We also use a pretty inefficient expansion that doesn't merge paths
to similar outcomes or anything.
|#

(defun generate-discrimination (outcome-tags miss-tag
                                positions args specializer-profile
                                call-history)
  (cond
    ((null positions)
     (unless (= (length call-history) 1)
       (error "Duplicate call history entries: ~s" call-history))
     `(go ,(or (cdr (assoc (cdr (first call-history)) outcome-tags))
             (error "No tag for outcome: ~s" (cdr (first call-history))))))
    ((null (elt specializer-profile (first positions)))
     (generate-discrimination
      outcome-tags miss-tag
      (rest positions) args specializer-profile call-history))
    (t (loop
         ;; A list (specializer . reduced-call-history)
         ;; where reduced-call-history is the subset of call-history with that
         ;; specializer in this position.
         with tree = nil
         for position = (first positions)
         for arg = (nth position args)
         for entry in call-history
         for (specs . outcome) = entry
         for spec = (first specs)
         for existing = (assoc spec tree)
         if existing
           do (push entry (cdr existing))
         else
           do (push (list spec entry) tree)
         do (assert (typep spec 'compiler-class))
         finally
            (return
              `(let ((stamp (instance-stamp ,arg)))
                 (cond
                   ,@(loop for (spec . entries) in tree
                           for scode = `(load-time-value
                                         (let ((stamp (core:class-stamp-for-instances ,spec)))
                                           (if (eq stamp (core:unbound))
                                               (error "Unbound stamp for ~s" ',(name spec))
                                               stamp))
                                         t)
                           collect `((= ,scode stamp)
                                     ,(generate-discrimination
                                       outcome-tags miss-tag
                                       (rest positions) args specializer-profile
                                       entries)))
                   (t (go ,miss-tag)))))))))

(defun generate-discriminator (generic gf-form call-history)
  (let* ((reqargs (required-parameters generic))
         (rest (if (restp generic) (gensym "REST") nil))
         ;; Produce an alist from outcomes to tags. The same outcome
         ;; will get the same tag.
         (outcome-tags (loop for (_ . outcome) in call-history
                             unless (assoc outcome result)
                               collect (cons outcome (gensym "OUTCOME"))
                                 into result
                             finally (return result)))
         ;; A list of positions of arguments according to the APO.
         (positions (loop for a in (apo generic)
                          collect (or (position a reqargs)
                                    (error "No position: ~s" a))))
         (miss (gensym "MISS")))
    `(lambda (,@reqargs ,@(if rest `(&rest ,rest) nil))
       (with-effective-method-parameters (,@reqargs ,rest)
         (block nil
           (tagbody
              ,(generate-discrimination outcome-tags miss
                                        positions reqargs
                                        (specializer-profile generic)
                                        call-history)
              ,@(loop for (outcome . tag) in outcome-tags
                      collect tag
                      collect `(return ,(form outcome)))
              ,miss
              (return
                ,(if rest
                     `(apply #'miss ,gf-form ,@reqargs ,rest)
                     `(miss ,gf-form ,@reqargs)))))))))
