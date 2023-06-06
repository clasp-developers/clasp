(in-package #:clasp-tests)
(test-type test-standard-char-p-$ #\$ standard-char)

;;; CHAR= CHAR/= CHAR< CHAR> CHAR<= CHAR>= CHAR-LESSP CHAR-GREATERP CHAR-EQUAL CHAR-NOT-LESSP CHAR-NOT-GREATERP CHAR-NOT-EQUAL 
;;; Should signal an error of type program-error if at least one character is not supplied.

(TEST-EXPECT-ERROR TEST-CHAR-0 (CHAR=) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-1 (CHAR/=) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-2 (CHAR<) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-3 (CHAR>) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-4 (CHAR<=) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-5 (CHAR>=) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-6 (CHAR-LESSP) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-7 (CHAR-GREATERP) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-8 (CHAR-EQUAL) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-9 (CHAR-NOT-LESSP) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-10 (CHAR-NOT-GREATERP) :TYPE PROGRAM-ERROR)
(TEST-EXPECT-ERROR TEST-CHAR-11 (CHAR-NOT-EQUAL) :TYPE PROGRAM-ERROR)

(TEST-EXPECT-ERROR TEST-CHAR-0a (CHAR= (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-1a (CHAR/= (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-2a (CHAR< (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-3a (CHAR> (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-4a (CHAR<= (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-5a (CHAR>= (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-6a (CHAR-LESSP (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-7a (CHAR-GREATERP (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-8a (CHAR-EQUAL (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-9a (CHAR-NOT-LESSP (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-10a (CHAR-NOT-GREATERP (make-hash-table)) :TYPE type-error)
(TEST-EXPECT-ERROR TEST-CHAR-11a (CHAR-NOT-EQUAL (make-hash-table)) :TYPE type-error)

(test TEST-CHAR-12 (name-char (char-name (code-char 13))) (#\return))

(test TEST-CHAR-12a (name-char (char-name (code-char 128))) (#\u80))

#+(or) (test-expect-error test-unicode-out-of-range #\U110000)

(test-type TEST-CHAR-13 (name-char (char-name (code-char 255))) character)

(test-type TEST-CHAR-14 (name-char (char-name (code-char 256))) character)

(test TEST-CHAR-15
      (loop for i from 0 below (min 65536 char-code-limit)
            for x = (code-char i)
            unless (or (not (characterp x))
                       (if (both-case-p x)
                           (and (graphic-char-p x)
                                (or (upper-case-p x)
                                    (lower-case-p x)))
                           (not (or (upper-case-p x)
                                    (lower-case-p x)))))
              collect (char-name x))
      (nil))

(test test-char-16
      (let ((names
              (append
               ;; Standard and semi-standard characters
               '("NEWLINE"  "SPACE" "RUBOUT" "PAGE" "TAB" "BACKSPACE" "RETURN" "LINEFEED")
               ;; A selection of additional characters from Unicode
               ;; (see cmp::*additional-clasp-character-mappings-alist*)
               '("NULL" "NUL" "BELL" "EXCLAMATION_MARK" "QUOTATION_MARK"
                 "AMPERSAND" "COMMA" "DIGIT_ZERO" "COLON"
                 "COMMERCIAL_AT" "LATIN_CAPITAL_LETTER_A"
                 "LATIN_SMALL_LETTER_X" "LEFT_CURLY_BRACKET" "TILDE" "DEL")
               ;; Characters by Unicode codepoint
               '("U80"  "U81"  "U82"  "U83"  "U84"  "U85"  "U86"  "U87"  "U88"  "U89"  "U8A"  "U8B"  "U8C"  "U8D"  "U8E"  "U8F" 
                 "U90"  "U91"  "U92"  "U93"  "U94"  "U95"  "U96"  "U97"  "U98"  "U99"  "U9A"  "U9B"  "U9C"  "U9D"  "U9E"  "U9F")))
            (result nil))
        (dolist (name names)
          (unless (name-char name)
            (push name result)))
        result)
      (nil))

(test-true test-char-17 (char/= #\a #\b #\c #\d))
(test-true test-char-18 (let ()(char/= #\a #\b #\c #\d)))

(test-true test-char-19 (eql #\Rubout #\Del))

;;; https://en.wikipedia.org/wiki/C0_and_C1_control_codes
(test-true test-char-c0
      (list #\NUL #\SOH #\STX #\ETX
            #\EOT #\ENQ #\ACK #\BEL
            #\BS #\HT #\LF #\VT
            #\FF #\CR #\SO #\SI
            #\DLE #\DC1 #\DC2 #\DC3
            #\DC4 #\NAK #\SYN #\ETB
            #\CAN #\EM #\SUB #\ESC
            #\FS #\GS #\RS #\US
            #\SP #\DEL))

(test-true test-char-standard-names
      (list #\Backspace #\Tab #\Newline #\Linefeed #\Page #\Return #\Space
            #\BACKSPACE #\TAB #\NEWLINE #\LINEFEED #\PAGE #\RETURN #\SPACE))

(TEST-true test-char-semistandard-names
      (list #\Null #\Bell #\Escape #\Rubout
            #\NULL #\BELL #\ESCAPE #\RUBOUT))
