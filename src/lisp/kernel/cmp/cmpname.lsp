;;; Copied from ecl/src/cmp/cmpname.lsp

(in-package :c)


(defun encode-number-in-name (number)
  ;; Encode a number in an alphanumeric identifier which is a valid C name.
  (cond ((zerop number) "0")
        ((minusp number) (encode-number-in-name (- number)))
        (t
         (do* ((code "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
               (base (length code))
               (output '())
               (digit 0))
              ((zerop number) (coerce (nreverse output) 'base-string))
           (multiple-value-setq (number digit) (floor number base))
           (push (char code digit) output)))))



(defun unique-init-name (file)
  "Create a unique name for this initialization function. The current algorithm
relies only on the name of the source file and the time at which it is built. This
should be enough to prevent name collisions for object files built in the same
machine."
  (let* ((path (pathname file))
         (path-hash (logxor (ash (sxhash path) 8)
                            (ash (sxhash (cddr (pathname-directory path))) 16)
                            (sxhash (pathname-name path))))
         (seconds (get-universal-time))
         (ms (+ (* seconds 1000)
                (mod (floor (* 1000 (get-internal-real-time))
                            internal-time-units-per-second)
                     1000)))
         (tag (concatenate 'base-string
                           "_clasp"
                           (encode-number-in-name path-hash)
                           "_"
                           (encode-number-in-name ms))))
    tag))

(defun compute-init-name (pathname &key kind 
                                     (prefix nil)
                                     (wrapper nil))
  "Computes initialization function name. Libraries, FASLS and
programs init function names can't be randomized to allow
initialization from the C code which wants to use it."
  (let ((filename (pathname-name (translate-logical-pathname pathname)))
        (unique-name (unique-init-name pathname)))
    (case kind
      ((:object :c)
       unique-name)
      ((:fasl :fas)
       (init-function-name "CODE" :kind :fas :prefix prefix))
      ((:static-library :lib)
       (init-function-name (if wrapper
                               (remove-prefix +static-library-prefix+ filename)
                               unique-name)
                           :kind :lib
                           :prefix prefix))
      ((:shared-library :dll)
       (init-function-name (if wrapper
                               (remove-prefix +shared-library-prefix+ filename)
                               unique-name)
                           :kind :dll
                           :prefix prefix))
      ((:program)
       (concatenate 'string (or prefix "init_") "CLASP_PROGRAM"))
      (otherwise
       (error "C::BUILDER cannot accept files of kind ~s" kind)))))

(defun init-function-name (s &key (kind :object) (prefix nil))
  (flet ((translate-char (c)
           (cond ((and (char>= c #\a) (char<= c #\z))
                  (char-upcase c))
                 ((and (char>= c #\A) (char<= c #\Z))
                  c)
                 ((or (eq c #\-) (eq c #\_))
                  #\_)
                 ((eq c #\*)
                  #\x)
                 ((eq c #\?)
                  #\a)
                 ((digit-char-p c)
                  c)
                 (t
                  #\p)))
         (disambiguation (c)
           (case kind
             ((:object :c) "")
             ((:fasl :fas) "fas_")
             ((:library :static-library :lib) "lib_")
             ((:shared-library :dll) "dll_")
             ((:program) "exe_")
             (otherwise (error "Not a valid argument to INIT-FUNCTION-NAME: kind = ~S"
                               kind)))))
    (setq s (map 'string #'translate-char (string s)))
    (concatenate 'string
                 (or prefix "init_")
                 (disambiguation kind)
                 (map 'string #'translate-char (string s)))))

