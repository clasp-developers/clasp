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

(in-package "SYS")

(defvar *debug-frame* nil)

(defvar -    nil)
(defvar +    nil)
(defvar ++   nil)
(defvar +++  nil)
(defvar *    nil)
(defvar **   nil)
(defvar ***  nil)
(defvar /    nil)
(defvar //   nil)
(defvar ///  nil)

(defun _print-in-loop (vals)
  (prin1 (car vals))
  (when (cdr vals)
    (princ " ;")
    (terpri)
    (_print-in-loop (cdr vals))))

(defun _read-eval-print-loop (istm ostm &optional (promt "") filter)
  (forever
    (catch 'repl
      (if (read-eval-print istm ostm promt filter)
        (return-from _read-eval-print-loop t)))))

(defun _read-loop ()
  (forever
    (catch 'top-level-catcher
      (if (_read-eval-print-loop *standard-input* *standard-output*)
        (return-from _read-loop)))))

(defun _help ()
  (print
":?	Help
 :bt	Backtrace
 :uw	Unwind break
"  ))

(defun _backtrace ()
  (do ((frame (_frame-up nil) (_frame-up frame)))
      ((null frame))
    (describe-frame *debug-io* frame)))

(defun _backtrace0 ()
  (do ((frame (_frame-up nil) (_frame-up frame)))
      ((null frame))
    (_describe-frame0 *debug-io* frame)))


(defun _break-filter (form)
  (case form
    (:? (_help))
		(:F (print (_frame-info)))
    (:bt (_backtrace))
    (:bt0 (_backtrace0))
    (:uw (throw 'unwind nil))))

