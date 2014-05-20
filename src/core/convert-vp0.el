;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.



(defun isolateMatchStrings ()
  "Convert the matched expression into a LOG expression"
  (let ( (matches (match-data)) parts (idx 1) res part )
    (setq matches (cddr matches) )
    (while matches
      (setq part (match-string idx) )
      (setq parts (cons part parts) )
      (setq matches (cddr matches) )
      (setq idx ( + idx 1 ) ) )
    (setq res (reverse parts) )
    res ) )



(defun stripReturn ( str )
  (save-match-data 
    (if (string-match "[\\]n" str)
	(replace-match "" nil nil str)
      str )) )


(defun processMatch ()
  (let ( (orig (match-string 0)) part ( parts (isolateMatchStrings) ) res fmtStr )
    (setq fmtStr (stripReturn (car parts)))
    (setq res (format "LOG(BF(%s)" fmtStr ) )
    (print (format "Starting with[%s]" res ) )
    (setq parts (cdr parts) )
    (while parts
      (setq part (car parts))
      (print (format "Appending argument[%s]" part ) )
      (setq res (format "%s %% (%s)" res (car parts) ) )
      (setq parts (cdr parts ) ) )
    (setq res (format "%s ); // vp0%s" res (substring (stripReturn orig) 3 ) ))
    res ) )


(defun looking-at-vp0 ()
    "Try different regex for different numbers of arguments"
    (or 
        (looking-at "VP0(([[:space:]]*\\(\"[^;]*\"\\)[[:space:]]*,[[:space:]]*\\([^,;]*\\),[[:space:]]*\\([^,;]*\\),[[:space:]]*\\([^,;]*\\),[[:space:]]*\\([^,;]*\\),[[:space:]]*\\([^,;]*\\)[[:space:]]*));[[:space:]]*$")  ; 5 arguments
        (looking-at "VP0(([[:space:]]*\\(\"[^;]*\"\\)[[:space:]]*,[[:space:]]*\\([^,;]*\\),[[:space:]]*\\([^,;]*\\),[[:space:]]*\\([^,;]*\\),[[:space:]]*\\([^,;]*\\)[[:space:]]*));[[:space:]]*$")   ; 4 arguments
        (looking-at "VP0(([[:space:]]*\\(\"[^;]*\"\\)[[:space:]]*,[[:space:]]*\\([^,;]*\\),[[:space:]]*\\([^,;]*\\),[[:space:]]*\\([^,;]*\\)[[:space:]]*));[[:space:]]*$")  ; 3 arguments
        (looking-at "VP0(([[:space:]]*\\(\"[^;]*\"\\)[[:space:]]*,[[:space:]]*\\([^,;]*\\),[[:space:]]*\\([^,;]*\\)[[:space:]]*));[[:space:]]*$")  ; 2 arguments
        (looking-at "VP0(([[:space:]]*\\(\"[^;]*\"\\)[[:space:]]*,[[:space:]]*\\([^,;]*\\)[[:space:]]*));[[:space:]]*$")  ; 1 argument
        (looking-at "VP0(([[:space:]]*\\(\"[^;]*\"\\)[[:space:]]*));[[:space:]]*$") ; 0 arguments
    ) )
      

(defun convert-vp0 ()
  "isolate the VP0 command"
  (interactive)
  (let* ((start (point)))
    (if (looking-at-vp0)
	(progn
	  (let ( (converted (processMatch)) )
	    (print (format "converted to(%s)" converted)) 
	    (replace-match converted)					;				    
	    ) ) ) ) )

