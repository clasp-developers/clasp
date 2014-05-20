(in-package :units)

;; (core:defmacro seconds (&optional (num 1.0))  `(units:Float ,num :seconds))
;; (core:defmacro nanoseconds (&optional (num 1.0)) `(units:Float ,num :nanoseconds))
;; (core:defmacro picoseconds (&optional (num 1.0)) `(units:Float ,num :picoseconds))
;; (core:defmacro femtoseconds (&optional (num 1.0)) `(units:Float ,num :femtoseconds))
;; (core:export 'seconds 'nanoseconds 'picoseconds 'femtoseconds)

;; (core:defmacro per-picosecond (&optional (num 1.0))  `(units:Float ,num :picoseconds -1))
;; (core:export 'per-picosecond)




;; (core:defmacro meters (&optional (num 1.0))  `(units:Float ,num :meters))
;; (core:defmacro angstroms (&optional (num 1.0)) `(units:Float ,num :angstroms))
;; (core:defmacro nanometers (&optional (num 1.0)) `(units:Float ,num :nanometers))
;; (core:export 'meters 'angstroms 'nanometers)

;; (core:defmacro Kelvin (&optional (num 1.0))  `(units:Float ,num :kelvin))
;; (core:export 'Kelvin)

;; (core:defmacro moles (&optional (num 1.0))  `(units:Float ,num :moles))
;; (core:export 'moles)

;; (core:defmacro moles*Kelvin (&optional (num 1.0))  `(units:Float ,num :moles :kelvin))
;; (core:export 'moles*Kelvin)

;; (core:defmacro Joules ( &optional (num 1.0) )  `(units:Float ,num :kilograms :meters 2 :seconds -2))
;; (core:defmacro kiloJoules ( &optional (num 1.0) )  `(units:Float (core:* 1000.0 ,num) :kilograms :meters 2 :seconds -2))
;; (core:defmacro kiloJoules-per-mole ( &optional (num 1.0) )  `(units:Float (core:* 1000.0 ,num) :kilograms :meters 2 :seconds -2 :moles -1))
;; (core:export 'Joules 'kiloJoules 'kiloJoules-per-mole)

(core:defconstant kB (* (/ joules kelvin) 1.3806488e-23))
(core:export 'kB)

(core:defconstant AVOGADRO_CONSTANT_NA (* (Unit moles -1) 6.02214179e+23))
(core:export 'AVOGADRO_CONSTANT_NA)
;; (core:defconstant R (units:div (Joules 8.3144621) (moles*Kelvin)))
;; (core:export 'R)

;; (core:defmacro kCal ()  `(Joules 4184.0))
;; (core:export 'kCal)
;; ;;
;; ;; Conversions
;; ;;
;; (core:defmacro energy-to-kCal (val)  `(units:div ,val ,(kCal)))
;; (core:export 'energy-to-kCal)


