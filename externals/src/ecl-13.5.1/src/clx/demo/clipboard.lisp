;;; This is a pretty direct translation of the Xlib selection test
;;; program by Tor Andersson found at
;;; <http://ghostscript.com/~tor/repos/Klipp/x11clipboard.c>, with
;;; minor enhancements:
;;;
;;; * gdk requestors apparently unconditionally request UTF8_STRING
;;;   selections without checking the TARGETS list of the selection
;;;   owner -- and apparently even never request anything else.  This
;;;   seems to be in contradiction with the freedesktop.org draft
;;;   specification at
;;;   <http://www.pps.jussieu.fr/~jch/software/UTF8_STRING/UTF8_STRING.text>
;;;   (linked from <http://freedesktop.org/Standards>), but this is
;;;   the real world and we have to live in it.  It would be nice if
;;;   someone in the freedesktop community could resolve this.
;;;
;;; * the original C code, in the XSendEvent call, has an event mask
;;;   of SelectionNotify.  SelectionNotify is not an event mask at
;;;   all, however: but the code works "by accident" because
;;;   SelectionNotify happens to have value 31, which has enough bits
;;;   flipped on that most clients select on at least one of those
;;;   events.  This bug is fixed below.
;;;
;;; * [ Update 2004-11-29, superseding to some extent the above ] in
;;;   fact, these two things are related.  ICCCM says that the event
;;;   disclaiming the ability to send in a given format should be sent
;;;   with an empty event mask ("2.2 Responsibilities of the Selection
;;;   Owner").
;;;
;;; * implemented the ICCCM-required TIMESTAMP and MULTIPLE targets
;;;
;;; As ever with these things, the divisions in intellectual property
;;; between the writer of the original C program, Tor Andersson
;;; (contactable at tor [dot] andersson [at] gmail [dot] com) and the
;;; translator (Christophe Rhodes, csr21 [at] cam [dot] ac [dot] uk)
;;; are murky, probably depend on jurisdiction, and in addition for
;;; such a small work are essentially trivial.  To set peoples' minds
;;; at ease, Tor wishes this information to be disseminated as widely
;;; as possible.

;;; Copyright (c) 2004, Christophe Rhodes
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(defpackage "CLIPBOARD"
  (:use "CL" "XLIB")
  (:export "MAIN"))

(in-package "CLIPBOARD")

;;; This is "traditional" XLIB style; I don't really know if it's the
;;; best way -- in developing this program, style of XLIB programming
;;; was secondary to achieving First Paste.
(defvar *window*)
(defvar *time*)
(defvar *display*)

(defun ownselect ()
  (format t "~&> set-selection-owner~%") (finish-output)
  (set-selection-owner *display* :primary *window* *time*)
  (unless (eq *window* (selection-owner *display* :primary))
    (write-string "failed to own primary")))

(defun deselect ()
  (format t "~&> unset-selection-owner~%") (finish-output)
  (set-selection-owner *display* :primary nil *time*)
  (unless (eq nil (selection-owner *display* :primary))
    (write-string "failed to disown primary")))

(defun ask-paste ()
  (format t "~&! deleting properties on window~%") (finish-output)
  (delete-property *window* :aeclip-target)
  (delete-property *window* :aeclip-string)
  (delete-property *window* :aeclip-utf8_string)
  (delete-property *window* :aeclip-text)
  (format t "~&> convert-selection TARGETS~%") (finish-output)
  (convert-selection :primary :targets *window* :aeclip-target)
  (format t "~&> convert-selection STRING~%")  (finish-output)
  (convert-selection :primary :string *window* :aeclip-string)
  (format t "~&> convert-selection UTF8_STRING~%") (finish-output)
  (convert-selection :primary :utf8_string *window* :aeclip-utf8_string)
  (format t "~&> convert-selection TEXT~%") (finish-output)
  (convert-selection :primary :text *window* :aeclip-text)
  nil)

(defun recv-paste (property)
  (multiple-value-bind (data name format)
      (get-property *window* property)
    (format t "~&< get-prop ~S " name)
    (case format
      (32 (format t "[~{~S~^,~}]"
                  (mapcar (lambda (x) (atom-name *display* x)) data)))
      (8 (format t "~S" (map 'string 'code-char data)))
      (t (format t "format=~S data=~S" format data)))
    (format t "~%") (finish-output)
    (delete-property *window* property)))

(defun send-copy (selection target property requestor time)
  (flet ((send (target property)
	   (case target
	     ((:string)
	      (format t "~&> sending text data~%") (finish-output)
	      (change-property requestor property
			       "Hello, World (from the CLX clipboard)!"
			       target 8
			       :transform #'char-code)
	      property)
	     (:targets
	      (format t "~&> sending targets list~%") (finish-output)
	      ;; ARGH.  Can't use :TRANSFORM as we scribble over CLX's buffer.
	      (let ((targets
		     (mapcar (lambda (x) (intern-atom *display* x))
			     '(:targets :timestamp :multiple :string))))
		(change-property requestor property targets target 32))
	      property)
	     (:timestamp
	      (format t "~&> sending timestamp~%") (finish-output)
	      (change-property requestor property (list *time*) target 32)
	      property)
	     (t
	      (format t "~&> sending none~%") (finish-output)
	      nil))))
    (case target
      ;; WARNING: this is untested.  I don't know of any clients which
      ;; use the :MULTIPLE target.
      (:multiple
       (let* ((list (get-property requestor property))
	      (plist (mapcar (lambda (x) (atom-name *display* x)) list)))
	 (loop for (ptarget pproperty) on plist by #'cddr
	    with all-succeeded = t
	    if (send ptarget pproperty)
	    collect ptarget into result
	    and collect pproperty into result
	    else
	    collect nil into result
	    and collect pproperty into result
	    and do (setf all-succeeded nil)
	    finally (unless all-succeeded
		      (let ((new-list
			     (mapcar (lambda (x) (intern-atom *display* x))
				     result)))
			(change-property requestor property new-list
					 target 32))))))
      (t (setf property (send target property))))
    (send-event requestor :selection-notify (make-event-mask)
		:selection selection :target target
		:property property :time time
		:event-window requestor :window requestor)))

(defun main ()
  (let* ((*display* (open-default-display))
         (screen (display-default-screen *display*))
         (*window*
          (create-window
           :parent (screen-root screen)
           :x 10 :y 10 :width 200 :height 200
           :event-mask (make-event-mask :button-press :property-change))))
    (map-window *window*)
    (display-finish-output *display*)
    (event-case (*display*)
      (:button-press (code time)
        (format t "~&ButtonPress~%") (finish-output)
        (case code
          (1 (setf *time* time) (ownselect))
          (2 (ask-paste))
          (3 (deselect))))
      (:client-message ()
        (format t "~&ClientMessage~%") (finish-output))
      (:selection-clear (selection)
        (format t "~&SelectionClear ~S~%" selection) (finish-output))
      (:selection-notify (selection target property)
        (format t "~&SelectionNotify ~S ~S ~S~%" selection target property)
        (finish-output)
        (unless (eq property nil)
          (recv-paste property))
        (display-finish-output *display*))
      (:selection-request (selection target property requestor time)
        (format t "~&SelectionRequest ~S ~S ~S~%" selection target property)
        (finish-output)
        (send-copy selection target property requestor time)
        (display-finish-output *display*))
      (:property-notify (atom state)
        (format t "~&PropertyNotify ~S ~S~%" atom state) (finish-output)))))
