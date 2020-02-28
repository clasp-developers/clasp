(in-package #:clasp-tests)
(test test-standard-char-p-$ (standard-char-p #\$))

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

(test TEST-CHAR-12
      (char= #\RETURN
             (name-char 
              (char-name (code-char 13)))))

(test TEST-CHAR-12a
      (char= #\U80
             (name-char 
              (char-name (code-char 128)))))

#+(or) (test-expect-error test-unicode-out-of-range #\U110000)

(test TEST-CHAR-13
      (typep  
       (name-char 
        (char-name (code-char 255)))
       'character))

(test TEST-CHAR-14
      (typep  
       (name-char 
        (char-name (code-char 256)))
       'character))

(test TEST-CHAR-15
      (null
       (loop for i from 0 below (min 65536 char-code-limit)
          for x = (code-char i)
          unless (or (not (characterp x))
                     (if (both-case-p x)
                         (and (graphic-char-p x)
                              (or (upper-case-p x)
                                (lower-case-p x)))
                         (not (or (upper-case-p x)
                                  (lower-case-p x)))))
          collect (char-name x))))

(test test-char-16
      (null
       (let ((names (list 
                     "NUL" 
                     "SOH"  "STX"  "ETX"  "EOT" 
                     "ENQ"  "ACK"  "BEL"  "Backspace" 
                     "Tab"  "Newline"  "VT" 
                     "Page"  "Return"  "SO"  "SI" 
                     "DLE"  "DC1"  "DC2"  "DC3" 
                     "DC4"  "NAK"  "SYN"  "ETB" 
                     "CAN"  "EM"  "SUB"  "ESC" 
                     "FS"  "GS"  "RS"  "US" 
                     "Space"  "EXCLAMATION_MARK"  "QUOTATION_MARK"  "NUMBER_SIGN" 
                     "DOLLAR_SIGN"  "PERCENT_SIGN"  "AMPERSAND"  "APOSTROPHE" 
                     "LEFT_PARENTHESIS"  "RIGHT_PARENTHESIS"  "ASTERISK"  "PLUS_SIGN" 
                     "COMMA"  "HYPHEN-MINUS"  "FULL_STOP"  "SOLIDUS" 
                     "DIGIT_ZERO"  "DIGIT_ONE"  "DIGIT_TWO"  "DIGIT_THREE" 
                     "DIGIT_FOUR"  "DIGIT_FIVE"  "DIGIT_SIX"  "DIGIT_SEVEN" 
                     "DIGIT_EIGHT"  "DIGIT_NINE"  "COLON"  "SEMICOLON" 
                     "LESS-THAN_SIGN"  "EQUALS_SIGN"  "GREATER-THAN_SIGN"  "QUESTION_MARK" 
                     "COMMERCIAL_AT"  "LATIN_CAPITAL_LETTER_A"  "LATIN_CAPITAL_LETTER_B"  "LATIN_CAPITAL_LETTER_C" 
                     "LATIN_CAPITAL_LETTER_D"  "LATIN_CAPITAL_LETTER_E"  "LATIN_CAPITAL_LETTER_F"  "LATIN_CAPITAL_LETTER_G" 
                     "LATIN_CAPITAL_LETTER_H"  "LATIN_CAPITAL_LETTER_I"  "LATIN_CAPITAL_LETTER_J"  "LATIN_CAPITAL_LETTER_K" 
                     "LATIN_CAPITAL_LETTER_L"  "LATIN_CAPITAL_LETTER_M"  "LATIN_CAPITAL_LETTER_N"  "LATIN_CAPITAL_LETTER_O" 
                     "LATIN_CAPITAL_LETTER_P"  "LATIN_CAPITAL_LETTER_Q"  "LATIN_CAPITAL_LETTER_R"  "LATIN_CAPITAL_LETTER_S" 
                     "LATIN_CAPITAL_LETTER_T"  "LATIN_CAPITAL_LETTER_U"  "LATIN_CAPITAL_LETTER_V"  "LATIN_CAPITAL_LETTER_W" 
                     "LATIN_CAPITAL_LETTER_X"  "LATIN_CAPITAL_LETTER_Y"  "LATIN_CAPITAL_LETTER_Z"  "LEFT_SQUARE_BRACKET" 
                     "REVERSE_SOLIDUS"  "RIGHT_SQUARE_BRACKET"  "CIRCUMFLEX_ACCENT"  "LOW_LINE" 
                     "GRAVE_ACCENT"  "LATIN_SMALL_LETTER_A"  "LATIN_SMALL_LETTER_B"  "LATIN_SMALL_LETTER_C" 
                     "LATIN_SMALL_LETTER_D"  "LATIN_SMALL_LETTER_E"  "LATIN_SMALL_LETTER_F"  "LATIN_SMALL_LETTER_G" 
                     "LATIN_SMALL_LETTER_H"  "LATIN_SMALL_LETTER_I"  "LATIN_SMALL_LETTER_J"  "LATIN_SMALL_LETTER_K" 
                     "LATIN_SMALL_LETTER_L"  "LATIN_SMALL_LETTER_M"  "LATIN_SMALL_LETTER_N"  "LATIN_SMALL_LETTER_O" 
                     "LATIN_SMALL_LETTER_P"  "LATIN_SMALL_LETTER_Q"  "LATIN_SMALL_LETTER_R"  "LATIN_SMALL_LETTER_S" 
                     "LATIN_SMALL_LETTER_T"  "LATIN_SMALL_LETTER_U"  "LATIN_SMALL_LETTER_V"  "LATIN_SMALL_LETTER_W" 
                     "LATIN_SMALL_LETTER_X"  "LATIN_SMALL_LETTER_Y"  "LATIN_SMALL_LETTER_Z"  "LEFT_CURLY_BRACKET" 
                     "VERTICAL_LINE"  "RIGHT_CURLY_BRACKET"  "TILDE"  "Rubout" 
              ;;; for the sake of a fast name-char  cover at least until #\UFF
                     "U80"  "U81"  "U82"  "U83"  "U84"  "U85"  "U86"  "U87"  "U88"  "U89"  "U8A"  "U8B"  "U8C"  "U8D"  "U8E"  "U8F" 
                     "U90"  "U91"  "U92"  "U93"  "U94"  "U95"  "U96"  "U97"  "U98"  "U99"  "U9A"  "U9B"  "U9C"  "U9D"  "U9E"  "U9F" 
                     "UA0"  "UA1"  "UA2"  "UA3"  "UA4"  "UA5"  "UA6"  "UA7"  "UA8"  "UA9"  "UAA"  "UAB"  "UAC"  "UAD"  "UAE"  "UAF" 
                     "UB0"  "UB1"  "UB2"  "UB3"  "UB4"  "UB5"  "UB6"  "UB7"  "UB8"  "UB9"  "UBA"  "UBB"  "UBC"  "UBD"  "UBE"  "UBF" 
                     "UC0"  "UC1"  "UC2"  "UC3"  "UC4"  "UC5"  "UC6"  "UC7"  "UC8"  "UC9"  "UCA"  "UCB"  "UCC"  "UCD"  "UCE"  "UCF" 
                     "UD0"  "UD1"  "UD2"  "UD3"  "UD4"  "UD5"  "UD6"  "UD7"  "UD8"  "UD9"  "UDA"  "UDB"  "UDC"  "UDD"  "UDE"  "UDF" 
                     "UE0"  "UE1"  "UE2"  "UE3"  "UE4"  "UE5"  "UE6"  "UE7"  "UE8"  "UE9"  "UEA"  "UEB"  "UEC"  "UED"  "UEE"  "UEF" 
                     "UF0"  "UF1"  "UF2"  "UF3"  "UF4"  "UF5"  "UF6"  "UF7"  "UF8"  "UF9"  "UFA"  "UFB"  "UFC"  "UFD"  "UFE" "UFF"
                     "NULL" "LINEFEED" "ESCAPE"))
             (result nil))
         (dolist (name names)
           (unless (name-char name)
             (push name result)))
         result)))

(test test-char-17 (char/= #\a #\b #\c #\d))
(test test-char-18 (let ()(char/= #\a #\b #\c #\d)))

(test test-char-19 (eql #\Rubout #\Del))

;;; https://en.wikipedia.org/wiki/C0_and_C1_control_codes
(test test-char-c0
      (list #\NUL #\SOH #\STX #\ETX
            #\EOT #\ENQ #\ACK #\BEL
            #\BS #\HT #\LF #\VT
            #\FF #\CR #\SO #\SI
            #\DLE #\DC1 #\DC2 #\DC3
            #\DC4 #\NAK #\SYN #\ETB
            #\CAN #\EM #\SUB #\ESC
            #\FS #\GS #\RS #\US
            #\SP #\DEL))

(test test-char-standard-names
      (list #\Backspace #\Tab #\Newline #\Linefeed #\Page #\Return #\Space
            #\BACKSPACE #\TAB #\NEWLINE #\LINEFEED #\PAGE #\RETURN #\SPACE))

(TEST test-char-semistandard-names
      (list #\Null #\Bell #\Escape #\Rubout
            #\NULL #\BELL #\ESCAPE #\RUBOUT))
