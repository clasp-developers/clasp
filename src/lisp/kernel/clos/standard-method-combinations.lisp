(in-package #:clos)

(define-method-combination standard ()
    ((around (:around))
     (before (:before))
     (primary () :required t)
     (after (:after)))
  (flet ((call-methods (methods)
           (mapcar (lambda (method)
                     `(call-method ,method))
                   methods)))
    ;; We're a bit more hopeful about avoiding make-method and m-v-p1 than
    ;; the example in CLHS define-method-combination.
    ;; Performance impact is likely to be marginal at best, but why not try?
    (let* ((call-primary `(call-method ,(first primary) ,(rest primary)))
           (call-before (if before
                            `(progn ,@(call-methods before) ,call-primary)
                            call-primary))
           (call-after (if after
                           `(multiple-value-prog1 ,call-before
                              ,@(call-methods (reverse after)))
                           call-before))
           (call-around (if around
                            (if (and (null before) (null after))
                                `(call-method ,(first around)
                                              (,@(rest around)
                                               ,@primary))
                                `(call-method ,(first around)
                                              (,@(rest around)
                                               (make-method ,call-after))))
                            call-after)))
      call-around)))

(define-method-combination progn :identity-with-one-argument t)
(define-method-combination and :identity-with-one-argument t)
(define-method-combination max :identity-with-one-argument t)
(define-method-combination + :identity-with-one-argument t)
(define-method-combination nconc :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument nil)
(define-method-combination list :identity-with-one-argument nil)
(define-method-combination min :identity-with-one-argument t)
(define-method-combination or :identity-with-one-argument t)
