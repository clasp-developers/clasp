;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:YES -*-

;;; Define lisp character to keysym mappings

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package :xlib)

(define-keysym-set :latin-1	(keysym 0 0) (keysym 0 255))
(define-keysym-set :latin-2	(keysym 1 0) (keysym 1 255))
(define-keysym-set :latin-3	(keysym 2 0) (keysym 2 255))
(define-keysym-set :latin-4	(keysym 3 0) (keysym 3 255))
(define-keysym-set :kana	(keysym 4 0) (keysym 4 255))
(define-keysym-set :arabic	(keysym 5 0) (keysym 5 255))
(define-keysym-set :cyrillic	(keysym 6 0) (keysym 6 255))
(define-keysym-set :greek	(keysym 7 0) (keysym 7 255))
(define-keysym-set :tech	(keysym 8 0) (keysym 8 255))
(define-keysym-set :special	(keysym 9 0) (keysym 9 255))
(define-keysym-set :publish	(keysym 10 0) (keysym 10 255))
(define-keysym-set :apl		(keysym 11 0) (keysym 11 255))
(define-keysym-set :hebrew	(keysym 12 0) (keysym 12 255))
(define-keysym-set :thai        (keysym 13 0) (keysym 13 255))
(define-keysym-set :korean      (keysym 14 0) (keysym 14 255))
(define-keysym-set :latin-5     (keysym 15 0) (keysym 15 255))
(define-keysym-set :latin-6     (keysym 16 0) (keysym 16 255))
(define-keysym-set :latin-7     (keysym 17 0) (keysym 17 255))
(define-keysym-set :latin-8     (keysym 18 0) (keysym 18 255))
(define-keysym-set :latin-9     (keysym 19 0) (keysym 19 255))
(define-keysym-set :currency    (keysym 32 0) (keysym 32 255))
(define-keysym-set :|3270|      (keysym 253 0) (keysym 253 255))
(define-keysym-set :xkb         (keysym 254 0) (keysym 254 255))
(define-keysym-set :keyboard	(keysym 255 0) (keysym 255 255))

(define-keysym :character-set-switch character-set-switch-keysym)
(define-keysym :left-shift left-shift-keysym)
(define-keysym :right-shift right-shift-keysym)
(define-keysym :left-control left-control-keysym)
(define-keysym :right-control right-control-keysym)
(define-keysym :caps-lock caps-lock-keysym)
(define-keysym :shift-lock shift-lock-keysym)
(define-keysym :left-meta left-meta-keysym)
(define-keysym :right-meta right-meta-keysym)
(define-keysym :left-alt left-alt-keysym)
(define-keysym :right-alt right-alt-keysym)
(define-keysym :left-super left-super-keysym)
(define-keysym :right-super right-super-keysym)
(define-keysym :left-hyper left-hyper-keysym)
(define-keysym :right-hyper right-hyper-keysym)

(define-keysym #\space 032)
(define-keysym #\! 033)
(define-keysym #\" 034)
(define-keysym #\# 035)
(define-keysym #\$ 036)
(define-keysym #\% 037)
(define-keysym #\& 038)
(define-keysym #\' 039)
(define-keysym #\( 040)
(define-keysym #\) 041)
(define-keysym #\* 042)
(define-keysym #\+ 043)
(define-keysym #\, 044)
(define-keysym #\- 045)
(define-keysym #\. 046)
(define-keysym #\/ 047)
(define-keysym #\0 048)
(define-keysym #\1 049)
(define-keysym #\2 050)
(define-keysym #\3 051)
(define-keysym #\4 052)
(define-keysym #\5 053)
(define-keysym #\6 054)
(define-keysym #\7 055)
(define-keysym #\8 056)
(define-keysym #\9 057)
(define-keysym #\: 058)
(define-keysym #\; 059)
(define-keysym #\< 060)
(define-keysym #\= 061)
(define-keysym #\> 062)
(define-keysym #\? 063)
(define-keysym #\@ 064)
(define-keysym #\A 065 :lowercase 097)
(define-keysym #\B 066 :lowercase 098)
(define-keysym #\C 067 :lowercase 099)
(define-keysym #\D 068 :lowercase 100)
(define-keysym #\E 069 :lowercase 101)
(define-keysym #\F 070 :lowercase 102)
(define-keysym #\G 071 :lowercase 103)
(define-keysym #\H 072 :lowercase 104)
(define-keysym #\I 073 :lowercase 105)
(define-keysym #\J 074 :lowercase 106)
(define-keysym #\K 075 :lowercase 107)
(define-keysym #\L 076 :lowercase 108)
(define-keysym #\M 077 :lowercase 109)
(define-keysym #\N 078 :lowercase 110)
(define-keysym #\O 079 :lowercase 111)
(define-keysym #\P 080 :lowercase 112)
(define-keysym #\Q 081 :lowercase 113)
(define-keysym #\R 082 :lowercase 114)
(define-keysym #\S 083 :lowercase 115)
(define-keysym #\T 084 :lowercase 116)
(define-keysym #\U 085 :lowercase 117)
(define-keysym #\V 086 :lowercase 118)
(define-keysym #\W 087 :lowercase 119)
(define-keysym #\X 088 :lowercase 120)
(define-keysym #\Y 089 :lowercase 121)
(define-keysym #\Z 090 :lowercase 122)
(define-keysym #\[ 091)
(define-keysym #\\ 092)
(define-keysym #\] 093)
(define-keysym #\^ 094)
(define-keysym #\_ 095)
(define-keysym #\` 096)
(define-keysym #\a 097)
(define-keysym #\b 098)
(define-keysym #\c 099)
(define-keysym #\d 100)
(define-keysym #\e 101)
(define-keysym #\f 102)
(define-keysym #\g 103)
(define-keysym #\h 104)
(define-keysym #\i 105)
(define-keysym #\j 106)
(define-keysym #\k 107)
(define-keysym #\l 108)
(define-keysym #\m 109)
(define-keysym #\n 110)
(define-keysym #\o 111)
(define-keysym #\p 112)
(define-keysym #\q 113)
(define-keysym #\r 114)
(define-keysym #\s 115)
(define-keysym #\t 116)
(define-keysym #\u 117)
(define-keysym #\v 118)
(define-keysym #\w 119)
(define-keysym #\x 120)
(define-keysym #\y 121)
(define-keysym #\z 122)
(define-keysym #\{ 123)
(define-keysym #\| 124)
(define-keysym #\} 125)
(define-keysym #\~ 126)

(progn   ;; Semi-standard characters
  (define-keysym #\rubout (keysym 255 255))	; :tty
  (define-keysym #\tab (keysym 255 009))	; :tty
  (define-keysym #\linefeed (keysym 255 010))	; :tty
  (define-keysym #\page (keysym 009 227))	; :special
  (define-keysym #\return (keysym 255 013))	; :tty
  (define-keysym #\backspace (keysym 255 008))	; :tty
  )

;;; these keysym definitions are only correct if the underlying lisp's
;;; definition of characters between 160 and 255 match latin1 exactly.
;;; If the characters are in some way locale-dependent (as, I believe,
;;; in Allegro8) or are treated as opaque without any notions of
;;; graphicness or case (as in cmucl and openmcl) then defining these
;;; keysyms is either not useful or wrong.  -- CSR, 2006-03-14
#+sbcl
(progn
  (do ((i 160 (+ i 1)))
      ((>= i 256))
    (if (or (<= #xc0 i #xd6)
            (<= #xd8 i #xde))
        (define-keysym (code-char i) i :lowercase (+ i 32))
        (define-keysym (code-char i) i))))

#+(or lispm excl)
(progn   ;; Nonstandard characters 
  (define-keysym #\escape (keysym 255 027))	; :tty
  )

#+ti
(progn
  (define-keysym #\Inverted-exclamation-mark 161)
  (define-keysym #\american-cent-sign 162)
  (define-keysym #\british-pound-sign 163)
  (define-keysym #\Currency-sign 164)
  (define-keysym #\Japanese-yen-sign 165)
  (define-keysym #\Yen 165)
  (define-keysym #\Broken-bar 166)
  (define-keysym #\Section-symbol 167)
  (define-keysym #\Section 167)
  (define-keysym #\Diaresis 168)
  (define-keysym #\Umlaut 168)
  (define-keysym #\Copyright-sign 169)
  (define-keysym #\Copyright 169)
  (define-keysym #\Feminine-ordinal-indicator 170)
  (define-keysym #\Angle-quotation-left 171)
  (define-keysym #\Soft-hyphen 173)
  (define-keysym #\Shy 173)
  (define-keysym #\Registered-trademark 174)
  (define-keysym #\Macron 175)
  (define-keysym #\Degree-sign 176)
  (define-keysym #\Ring 176)
  (define-keysym #\Plus-minus-sign 177)
  (define-keysym #\Superscript-2 178)
  (define-keysym #\Superscript-3 179)
  (define-keysym #\Acute-accent 180)
  (define-keysym #\Greek-mu 181)
  (define-keysym #\Paragraph-symbol 182)
  (define-keysym #\Paragraph 182)
  (define-keysym #\Pilcrow-sign 182)
  (define-keysym #\Middle-dot 183)
  (define-keysym #\Cedilla 184)
  (define-keysym #\Superscript-1 185)
  (define-keysym #\Masculine-ordinal-indicator 186)
  (define-keysym #\Angle-quotation-right 187)
  (define-keysym #\Fraction-1/4 188)
  (define-keysym #\One-quarter 188)
  (define-keysym #\Fraction-1/2 189)
  (define-keysym #\One-half 189)
  (define-keysym #\Fraction-3/4 190)
  (define-keysym #\Three-quarters 190)
  (define-keysym #\Inverted-question-mark 191)
  (define-keysym #\Multiplication-sign 215)
  (define-keysym #\Eszet 223)
  (define-keysym #\Division-sign 247)
)

#+ti
(progn	;; There are no 7-bit ascii representations for the following
        ;; European characters, so use int-char to create them to ensure
        ;; nothing is lost while sending files through the mail.
  (define-keysym (int-char 192) 192 :lowercase 224)
  (define-keysym (int-char 193) 193 :lowercase 225)
  (define-keysym (int-char 194) 194 :lowercase 226)
  (define-keysym (int-char 195) 195 :lowercase 227)
  (define-keysym (int-char 196) 196 :lowercase 228)
  (define-keysym (int-char 197) 197 :lowercase 229)
  (define-keysym (int-char 198) 198 :lowercase 230)
  (define-keysym (int-char 199) 199 :lowercase 231)
  (define-keysym (int-char 200) 200 :lowercase 232)
  (define-keysym (int-char 201) 201 :lowercase 233)
  (define-keysym (int-char 202) 202 :lowercase 234)
  (define-keysym (int-char 203) 203 :lowercase 235)
  (define-keysym (int-char 204) 204 :lowercase 236)
  (define-keysym (int-char 205) 205 :lowercase 237)
  (define-keysym (int-char 206) 206 :lowercase 238)
  (define-keysym (int-char 207) 207 :lowercase 239)
  (define-keysym (int-char 208) 208 :lowercase 240)
  (define-keysym (int-char 209) 209 :lowercase 241)
  (define-keysym (int-char 210) 210 :lowercase 242)
  (define-keysym (int-char 211) 211 :lowercase 243)
  (define-keysym (int-char 212) 212 :lowercase 244)
  (define-keysym (int-char 213) 213 :lowercase 245)
  (define-keysym (int-char 214) 214 :lowercase 246)
  (define-keysym (int-char 215) 215)
  (define-keysym (int-char 216) 216 :lowercase 248)
  (define-keysym (int-char 217) 217 :lowercase 249)
  (define-keysym (int-char 218) 218 :lowercase 250)
  (define-keysym (int-char 219) 219 :lowercase 251)
  (define-keysym (int-char 220) 220 :lowercase 252)
  (define-keysym (int-char 221) 221 :lowercase 253)
  (define-keysym (int-char 222) 222 :lowercase 254)
  (define-keysym (int-char 223) 223)
  (define-keysym (int-char 224) 224)
  (define-keysym (int-char 225) 225)
  (define-keysym (int-char 226) 226)
  (define-keysym (int-char 227) 227)
  (define-keysym (int-char 228) 228)
  (define-keysym (int-char 229) 229)
  (define-keysym (int-char 230) 230)
  (define-keysym (int-char 231) 231)
  (define-keysym (int-char 232) 232)
  (define-keysym (int-char 233) 233)
  (define-keysym (int-char 234) 234)
  (define-keysym (int-char 235) 235)
  (define-keysym (int-char 236) 236)
  (define-keysym (int-char 237) 237)
  (define-keysym (int-char 238) 238)
  (define-keysym (int-char 239) 239)
  (define-keysym (int-char 240) 240)
  (define-keysym (int-char 241) 241)
  (define-keysym (int-char 242) 242)
  (define-keysym (int-char 243) 243)
  (define-keysym (int-char 244) 244)
  (define-keysym (int-char 245) 245)
  (define-keysym (int-char 246) 246)
  (define-keysym (int-char 247) 247)
  (define-keysym (int-char 248) 248)
  (define-keysym (int-char 249) 249)
  (define-keysym (int-char 250) 250)
  (define-keysym (int-char 251) 251)
  (define-keysym (int-char 252) 252)
  (define-keysym (int-char 253) 253)
  (define-keysym (int-char 254) 254)
  (define-keysym (int-char 255) 255)
  )

#+lispm  ;; Nonstandard characters
(progn 
  (define-keysym #\center-dot (keysym 183))	; :latin-1
  (define-keysym #\down-arrow (keysym 008 254))	; :technical
  (define-keysym #\alpha (keysym 007 225))	; :greek
  (define-keysym #\beta (keysym 007 226))	; :greek
  (define-keysym #\and-sign (keysym 008 222))	; :technical
  (define-keysym #\not-sign (keysym 172))	; :latin-1
  (define-keysym #\epsilon (keysym 007 229))	; :greek
  (define-keysym #\pi (keysym 007 240))		; :greek
  (define-keysym #\lambda (keysym 007 235))	; :greek
  (define-keysym #\gamma (keysym 007 227))	; :greek
  (define-keysym #\delta (keysym 007 228))	; :greek
  (define-keysym #\up-arrow (keysym 008 252))	; :technical
  (define-keysym #\plus-minus (keysym 177))	; :latin-1
  (define-keysym #\infinity (keysym 008 194))	; :technical
  (define-keysym #\partial-delta (keysym 008 239))	; :technical
  (define-keysym #\left-horseshoe (keysym 011 218))	; :apl
  (define-keysym #\right-horseshoe (keysym 011 216))	; :apl
  (define-keysym #\up-horseshoe (keysym 011 195))	; :apl
  (define-keysym #\down-horseshoe (keysym 011 214))	; :apl
  (define-keysym #\double-arrow (keysym 008 205))	; :technical
  (define-keysym #\left-arrow (keysym 008 251))	; :technical
  (define-keysym #\right-arrow (keysym 008 253))	; :technical
  (define-keysym #\not-equals (keysym 008 189))	; :technical
  (define-keysym #\less-or-equal (keysym 008 188))	; :technical
  (define-keysym #\greater-or-equal (keysym 008 190))	; :technical
  (define-keysym #\equivalence (keysym 008 207))	; :technical
  (define-keysym #\or-sign (keysym 008 223))	; :technical
  (define-keysym #\integral (keysym 008 191))	; :technical
;;  break isn't null
;;  (define-keysym #\null (keysym 255 107))	; :function
  (define-keysym #\clear-input (keysym 255 011))	; :tty
  (define-keysym #\help (keysym 255 106))	; :function
  (define-keysym #\refresh (keysym 255 097))	; :function
  (define-keysym #\abort (keysym 255 105))	; :function
  (define-keysym #\resume (keysym 255 098))	; :function
  (define-keysym #\end (keysym 255 087))	; :cursor
;;#\universal-quantifier
;;#\existential-quantifier
;;#\circle-plus
;;#\circle-cross same as #\circle-x
  )

#+genera
(progn
;;#\network
;;#\symbol-help
  (define-keysym #\lozenge (keysym 009 224))	; :special
  (define-keysym #\suspend (keysym 255 019))	; :tty
  (define-keysym #\function (keysym 255 032))	; :function
  (define-keysym #\square (keysym 010 231))	; :publishing
  (define-keysym #\circle (keysym 010 230))	; :publishing
  (define-keysym #\triangle (keysym 010 232))	; :publishing
  (define-keysym #\scroll (keysym 255 086))	; :cursor
  (define-keysym #\select (keysym 255 096))	; :function
  (define-keysym #\complete (keysym 255 104))	; :function
  )

#+ti
(progn
  (define-keysym #\terminal (keysym 255 032))	; :function
  (define-keysym #\system (keysym 255 096))	; :function
  (define-keysym #\center-arrow (keysym 255 80))
  (define-keysym #\left-arrow (keysym 255 081))	; :cursor
  (define-keysym #\up-arrow (keysym 255 082))	; :cursor
  (define-keysym #\right-arrow (keysym 255 083))	; :cursor
  (define-keysym #\down-arrow (keysym 255 084))	; :cursor
  (define-keysym #\end (keysym 255 087))	; :cursor
  (define-keysym #\undo (keysym 255 101))	; :function
  (define-keysym #\break (keysym 255 107))
  (define-keysym #\keypad-space (keysym 255 128))	; :keypad
  (define-keysym #\keypad-tab (keysym 255 137))	; :keypad
  (define-keysym #\keypad-enter (keysym 255 141))	; :keypad
  (define-keysym #\f1 (keysym 255 145))		; :keypad
  (define-keysym #\f2 (keysym 255 146))		; :keypad
  (define-keysym #\f3 (keysym 255 147))		; :keypad
  (define-keysym #\f4 (keysym 255 148))		; :keypad
  (define-keysym #\f1 (keysym 255 190))		; :keypad
  (define-keysym #\f2 (keysym 255 191))		; :keypad
  (define-keysym #\f3 (keysym 255 192))		; :keypad
  (define-keysym #\f4 (keysym 255 193))		; :keypad
  (define-keysym #\keypad-plus (keysym 255 171))	; :keypad
  (define-keysym #\keypad-comma (keysym 255 172))	; :keypad
  (define-keysym #\keypad-minus (keysym 255 173))	; :keypad
  (define-keysym #\keypad-period (keysym 255 174))	; :keypad
  (define-keysym #\keypad-0 (keysym 255 176))	; :keypad
  (define-keysym #\keypad-1 (keysym 255 177))	; :keypad
  (define-keysym #\keypad-2 (keysym 255 178))	; :keypad
  (define-keysym #\keypad-3 (keysym 255 179))	; :keypad
  (define-keysym #\keypad-4 (keysym 255 180))	; :keypad
  (define-keysym #\keypad-5 (keysym 255 181))	; :keypad
  (define-keysym #\keypad-6 (keysym 255 182))	; :keypad
  (define-keysym #\keypad-7 (keysym 255 183))	; :keypad
  (define-keysym #\keypad-8 (keysym 255 184))	; :keypad
  (define-keysym #\keypad-9 (keysym 255 185))	; :keypad
  (define-keysym #\keypad-equal (keysym 255 189))	; :keypad
  (define-keysym #\f1 (keysym 255 192))		; :function
  (define-keysym #\f2 (keysym 255 193))		; :function
  (define-keysym #\f3 (keysym 255 194))		; :function
  (define-keysym #\f4 (keysym 255 195))		; :function
  (define-keysym #\network (keysym 255 214))
  (define-keysym #\status (keysym 255 215))
  (define-keysym #\clear-screen (keysym 255 217))
  (define-keysym #\left (keysym 255 218))
  (define-keysym #\middle (keysym 255 219))
  (define-keysym #\right (keysym 255 220))
  (define-keysym #\resume (keysym 255 221))
  (define-keysym #\vt (keysym 009 233))		; :special ;; same as #\delete
  )

#+ti
(progn  ;; Explorer specific characters
  (define-keysym #\Call (keysym 131))		; :latin-1
  (define-keysym #\Macro (keysym 133))		; :latin-1
  (define-keysym #\Quote (keysym 142))		; :latin-1
  (define-keysym #\Hold-output (keysym 143))	; :latin-1
  (define-keysym #\Stop-output (keysym 144))	; :latin-1
  (define-keysym #\Center (keysym 156))		; :latin-1
  (define-keysym #\no-break-space (keysym 160))	; :latin-1

  (define-keysym #\circle-plus (keysym 13))	; :latin-1
  (define-keysym #\universal-quantifier (keysym 20))	; :latin-1
  (define-keysym #\existential-quantifier (keysym 21))	; :latin-1
  (define-keysym #\circle-cross (keysym 22))	; :latin-1
  )

