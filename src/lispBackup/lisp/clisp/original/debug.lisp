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

(in-package "SYSTEM")

(defun frame-up (n sp mode)
  (do ((np (frame-up-1 sp mode)))
     ((or (eq np sp) (and (integerp n) (zerop n))) sp)
    (setq sp np)
    (if (integerp n)
      (setq n (1- n)))))

(defun frame-down (n sp mode)
  (do ((np (frame-down-1 sp mode)))
     ((or (eq np sp) (and (integerp n) (zerop n))) sp)
    (setq sp np)
    (if (integerp n)
      (setq n (1- n)))))

    
(defun _read-form-eof (istream)
	(clear-input istream)
	(values t t))
    
(defun read-form (prompt &optional command-list &aux (istream *standard-input*) (ostream *standard-output*)
                                                     (interactive-p (interactive-stream-p istream)))
   (let () ;((raw (terminal-raw istream nil)))
     (when interactive-p
       (fresh-line ostream)
       (write-string prompt ostream)
       (force-output ostream))
     (let* ((eof-value "EOF")
            (form (let ((*read-suppress* nil)
                        (*key-bindings* (nreconc command-list
                                                 *key-bindings*))
                        trs-bound
                        old-trs)
                    (prog2    
                      (if (and interactive-p (not (boundp '*terminal-read-stream*)))
                      	(multiple-value-bind (line flag) (read-line istream nil)
                      	  (when (null line) (return-from read-form (_read-form-eof istream)))
                      	  (dolist (h *key-bindings*)
                      	    (when (and (consp h) (string-equal (car h) (subseq line 0 (min (length (car h))
                                                                                           (length line) ))))
											 			  (funcall (cdr h) (subseq line (length (car h))))
											 			  (return-from read-form (_read-form-eof istream))))
													(setq istream (make-concatenated-stream
																					(make-string-input-stream
																						(if flag line
																							 (ext:string-concat line (string #\Newline))))
																					istream)
																trs-bound t))
												(when (boundp '*terminal-read-stream*)
												  (setq old-trs *terminal-read-stream*)
												  (makunbound '*terminal-read-stream*)
												  (setq istream old-trs
																trs-bound t)))
                      (progv (if trs-bound '(*terminal-read-stream*)) (if trs-bound (list istream))
                        (prog1 (read istream nil eof-value nil)
                               (setq old-trs (and trs-bound (boundp '*terminal-read-stream*) *terminal-read-stream*))))
                      (when old-trs
												(let ((concs (concatenated-stream-streams old-trs)))
                          (when (and (cdr concs) (peek-char t (car concs) nil))
                          	(setq *terminal-read-stream* old-trs))))))))                                                 
      ; (terminal-raw istream raw)
       (if (eql form eof-value)
         (_read-form-eof istream)
         (progn          ;(clear-input-upto-newline istream)
                (values form nil))))))

(defun read-eval-print (prompt &optional command-list)
	(multiple-value-bind (form flag)
       (read-form prompt command-list)
		(if flag
    	form ; return T
      (progn
				(setq +++ ++
							++ +
							+ form
							- form)
				(let ((vals (multiple-value-list (eval form))))
					(setq /// //
								// /
								/ vals
								*** **
								** *
								* (car vals))
					 (fresh-line)
					 (when vals
						 (do ()
								 ((null (progn (write (pop vals)) vals)))
							 (write-string " ;")
							 (terpri))))))))



	


