;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (indent-tabs-mode . nil) ; Make sure to disable smart-tabs-mode if you're using it.
  (whitespace-action . nil)
  (whitespace-style . '(face trailing empty tabs))
  (c-basic-offset . 2)
  (c-auto-align-backslashes . nil)
  (c-file-offsets
   ( innamespace . 0 )
   ( substatement-open . 0 )
   ( c . c-lineup-dont-change )
   ( inextern-lang . 0 )
   ( comment-intro . c-lineup-dont-change )
   ;; If this gets to be too much of a pain, switch to
   ;; c-lineup-dont-change and rely on clang-format
   ( arglist-cont-nonempty . c-lineup-arglist )
   ( block-close . 0 )
   ( statement-case-intro . ++ )
   ( brace-list-intro . ++ )
   ( cpp-define-intro . + ))))
