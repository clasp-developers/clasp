(in-package #:clasp-printer)

(defclass client
    (incless-native:client inravina:client invistra:client quaviver/schubfach:client)
  ())

(defvar *client* (make-instance 'client))

(inravina:define-interface :client-form *client*
                           :client-class client
                           :intrinsic t)

(invistra:define-interface :client-form *client*
                           :client-class client
                           :intrinsic t)
