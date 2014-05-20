#|#########################################################################
# Copyright (C) 1997-2011 Ufasoft                                         #
# http://ufasoft.com   mailto:support@ufasoft.com                         #
#                                                                         #
# This program is free software; you can redistribute it and/or modify it #
# under the terms of the GNU General Public License as published by the   #
# Free Software Foundation; either version 3, or (at your option) any     #
# later version.                                                          #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program; If not, see <http://www.gnu.org/licenses/>     #
########################################################################=|#

;; The list of active restarts.
(defvar *active-restarts* nil)

;; A list of pairs of conditions and restarts associated with them. We have to
;; keep the associations separate because there can be a many-to-many mapping
;; between restarts and conditions, and this mapping has dynamic extent.
(defvar *condition-restarts* nil)

(defun default-restart-test (condition)
  t)

(defstruct restart
	name
	(test #'default-restart-test))

(defmacro restart-case (rf &rest clauses)
  (err))

(defmacro with-simple-restart ((restart-name format-control &rest format-arguments)
                               &body forms)
  `(restart-case (progn ,@forms)
    (,restart-name ()
      :report (lambda (stream)
                (format stream ,format-control ,@format-arguments))
      (values nil t))))


(defun compute-restarts (&optional condition)
  (if condition
    ; return only restarts that are applicable to that condition
    (remove-if-not #'(lambda (restart) (applicable-restart-p restart condition))
                   *active-restarts*
    )
    ; return all restarts
    *active-restarts*
) )



;;; 29.4.7. Establishing Restarts

;; This conses out the wazoo, but there seems to be no good way around it short
;; of special casing things a zillion ways.  The main problem is that someone
;; could write:
;;
;; (restart-bind ((nil *fun-1*
;;                     :interactive-function *fun-2*
;;                     :report-function *fun-3*
;;                     :test-function *fun-4*
;;                 )) ...)
;;
;; and it is supposed to work.

;; RESTART-BIND, CLtL2 p. 909
(defmacro restart-bind (restart-specs &body body)
  (setq body `(PROGN ,@body))
  (unless (listp restart-specs)
    (error-of-type 'source-program-error
      (ENGLISH "~S: not a list: ~S")
      'restart-bind restart-specs
  ) )
  (if restart-specs
    `(LET ((*ACTIVE-RESTARTS*
             (LIST*
               ,@(mapcar #'(lambda (spec)
                             (unless (and (listp spec) (consp (cdr spec)) (symbolp (first spec)))
                               (error-of-type 'source-program-error
                                 (ENGLISH "~S: invalid restart specification ~S")
                                 'restart-bind spec
                             ) )
                             (apply #'(lambda (name function
                                               &key (test-function '(FUNCTION DEFAULT-RESTART-TEST))
                                                    (interactive-function '(FUNCTION DEFAULT-RESTART-INTERACTIVE))
                                                    (report-function 'NIL))
                                        (when (and (null name) (eq report-function 'NIL))
                                          ; CLtL2 p. 906: "It is an error if an unnamed restart is used
                                          ; and no report information is provided."
                                          (error-of-type 'source-program-error
                                            (ENGLISH "~S: unnamed restarts require ~S to be specified: ~S")
                                            'restart-bind ':REPORT-FUNCTION spec
                                        ) )
                                        (make-restart-form `',name
                                                           test-function
                                                           'NIL
                                                           function
                                                           report-function
                                                           interactive-function
                                      ) )
                                    spec
                           ) )
                         restart-specs
                 )
               *ACTIVE-RESTARTS*
          )) )
       ,body
     )
    body
) )

; Tests whether a given restart is applicable to a given condition
(defun applicable-restart-p (restart condition)
  (and
   (or (null condition)
       ;; A restart is applicable if it is associated to that condition
       ;; or if it is not associated to any condition.
       (let ((not-at-all t))
         (dolist (asso *condition-restarts* not-at-all)
           (when (eq (cdr asso) restart)
             (if (eq (car asso) condition)
                 (return t)
                 (setq not-at-all nil))))))
   ;; Call the restart's test function:
   (funcall (restart-test restart) condition)))



(defun find-restart (ri &optional c)
	(let ((f (etypecase ri
						 (null (%error))
						 (symbol #'restart-name)
						 (restart #'identity))))
		(dolist (r *active-restarts*)
			(when (and (eq (funcall f r) ri)
                 (or (null c) (applicable-restart-p r c)))
				(return restart)))))
