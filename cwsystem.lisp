;;
;;  The Conway-Wechsler System
;;   http://www.mrob.com/pub/math/largenum.html#conway-wechsler
;;
(defpackage #:cwsystem
  (:use #:cl)
  (:export
    #:unit-output
    #:unit-stream
    #:unit-string
    #:radix-output
    #:radix-stream
    #:radix-string))
(in-package #:cwsystem)

(defun mkstr (&rest args)
  (with-output-to-string (stream)
    (dolist (arg args)
      (princ arg stream))))

(defun pout (&rest args)
  (dolist (x args)
    (princ x)))

(defun jout (sep list &aux first)
  (dolist (x list)
    (if first
      (pout sep)
      (setq first t))
    (pout x)))

(defmacro dovect ((x vector &optional r) &body body)
  (let ((g (gensym))
        (v (gensym)))
    `(let ((,v ,vector) ,x)
       (dotimes (,g (length ,v) ,r)
         (setq ,x (elt ,v ,g))
         (progn ,@body)))))


;;
;;  unit
;;
(defun unit-small-string (x)
  (ecase x
    (0 nil)
    (1 "mi")
    (2 "bi")
    (3 "tri")
    (4 "quadri")
    (5 "quinti")
    (6 "sexti")
    (7 "septi")
    (8 "octi")
    (9 "noni")))

(defun unit-small-out (x llion)
  (let ((x (unit-small-string x)))
    (cond ((null x) (pout (if llion "thousand" "ni")))
          (llion (pout x "llion"))
          (t (pout x)))))

(defun unit-large-1 (x)
  (ecase x
    (0 (values nil         nil  ))
    (1 (values "un"        ""   ))
    (2 (values "duo"       ""   ))
    (3 (values "tre"       t    ))
    (4 (values "quattuor"  ""   ))
    (5 (values "quin"      ""   ))
    (6 (values "se"        "sx" ))
    (7 (values "septe"     "mn" ))
    (8 (values "octo"      ""   ))
    (9 (values "nove"      "mn" ))))

(defun unit-large-10 (x)
  (ecase x
    (0 (values nil             nil  ))
    (1 (values "deci"          "n"  ))
    (2 (values "viginti"       "ms" ))
    (3 (values "triginta"      "ns" ))
    (4 (values "quadraginta"   "ns" ))
    (5 (values "quinquaginta"  "ns" ))
    (6 (values "sexaginta"     "n"  ))
    (7 (values "septuaginta"   "n"  ))
    (8 (values "octoginta"     "mx" ))
    (9 (values "nonaginta"     ""   ))))

(defun unit-large-100 (x)
  (ecase x
    (0 (values nil             nil  ))
    (1 (values "centi"         "nx" ))
    (2 (values "ducenti"       "n"  ))
    (3 (values "trecenti"      "ns" ))
    (4 (values "quadringenti"  "ns" ))
    (5 (values "quingenti"     "ns" ))
    (6 (values "sescenti"      "n"  ))
    (7 (values "septingenti"   "n"  ))
    (8 (values "octingenti"    "mx" ))
    (9 (values "nongenti"      ""   ))))

(defun unit-replace (b1 b2)
  (dovect (x b1)
    (if (position x b2 :test #'eql)
      (pout x))))

(defun unit-replace-sx (a1 b1 a2 b2)
  (pout a1)
  (cond ((not (eq b1 t))
         (unit-replace b1 b2))
        ((or (position #\s b2 :test #'eql)
             (position #\x b2 :test #'eql))
         (pout "s")))
  (pout a2))

(defun break-quotient (x)
  (multiple-value-bind (a b) (truncate x 10)
    (multiple-value-bind (c d) (truncate a 10)
      (multiple-value-bind (e f) (truncate c 10)
        (declare (ignore e))
        (values b d f)))))

(defun unit-large (x)
  (multiple-value-bind (s1 s10 s100) (break-quotient x)
    (multiple-value-bind (a1 b1) (unit-large-1 s1)
      (multiple-value-bind (a2 b2) (unit-large-10 s10)
        (multiple-value-bind (a3 b3) (unit-large-100 s100)
          (cond ((and (null a1) (null a2)) (pout a3))
                ((and (null a1) (null a3)) (pout a2))
                ((null a1) (pout a2 a3))
                ((null a2) (unit-replace-sx a1 b1 a3 b3))
                ((null a3) (unit-replace-sx a1 b1 a2 b2))
                (t (unit-replace-sx a1 b1 a2 b2)
                   (pout a3))))))))

(defun unit-name (quot)
  (cond ((< quot 10)
         (unit-small-out quot nil))
        ((< quot 1000)
         (unit-large quot))
        (t (error "Too large number ~A" quot))))

(defun remove-vowel (str)
  (let ((size (length str)))
    (if (position (char str (1- size)) "aeiou" :test #'eql)
      (subseq str 0 (1- size))
      str)))

(defun unit-name-extend (quot)
  (pout (remove-vowel
          (with-output-to-string (*standard-output*)
            (unit-name quot)))
        "illi"))

(defun unit-name-recursive (quot)
  (multiple-value-bind (large small) (truncate quot 1000)
    (unless (zerop large)
      (unit-name-recursive large))
    (unit-name-extend small)))

(defun unit-name-index (quot)
  (if (< quot 10)
    (unit-small-out quot t)
    (progn
      (unit-name-recursive quot)
      (pout "on"))))

(defun unit-output (index &optional (cardinal t))
  (unit-name-index index)
  (unless cardinal
    (pout "th")))

(defun unit-stream (index stream &optional (cardinal t))
  (let ((*standard-output* stream))
    (unit-output index cardinal)))

(defun unit-string (index &optional (cardinal t))
  (with-output-to-string (*standard-output*)
    (unit-output index cardinal)))


;;
;;  radix
;;
(defvar *cardinal*)
(defvar *radix*)

(defun radix-20 (value &optional (cardinal *cardinal*))
  (ecase value
    (0 (if cardinal "zero" "zeroth"))
    (1 (if cardinal "one" "first"))
    (2 (if cardinal "two" "second"))
    (3 (if cardinal "three" "third"))
    (4 (if cardinal "four" "fourth"))
    (5 (if cardinal "five" "fifth"))
    (6 (if cardinal "six" "sixth"))
    (7 (if cardinal "seven" "seventh"))
    (8 (if cardinal "eight" "eighth"))
    (9 (if cardinal "nine" "ninth"))
    (10 (if cardinal "ten" "tenth"))
    (11 (if cardinal "eleven" "eleventh"))
    (12 (if cardinal "twelve" "twelfth"))
    (13 (if cardinal "thirteen" "thirteenth"))
    (14 (if cardinal "fourteen" "fourteenth"))
    (15 (if cardinal "fifteen" "fifteenth"))
    (16 (if cardinal "sixteen" "sixteenth"))
    (17 (if cardinal "seventeen" "seventeenth"))
    (18 (if cardinal "eighteen" "eighteenth"))
    (19 (if cardinal "nineteen" "nineteenth"))))

(defun radix-100 (value &optional (cardinal *cardinal*))
  (ecase value
    (2 (if cardinal "twenty" "twentieth"))
    (3 (if cardinal "thirty" "thirtieth"))
    (4 (if cardinal "forty" "fortieth"))
    (5 (if cardinal "fifty" "fiftieth"))
    (6 (if cardinal "sixty" "sixtieth"))
    (7 (if cardinal "seventy" "seventieth"))
    (8 (if cardinal "eighty" "eightieth"))
    (9 (if cardinal "ninety" "ninetieth"))))

(defun radix-hundred (&optional (cardinal *cardinal*))
  (if cardinal "hundred" "hundredth"))

(defun pradix (&rest args)
  (dolist (x args)
    (push x *radix*))
  (setq *cardinal* t))

(defun pradix20 (value)
  (pradix
    (radix-20 value)))

(defun break-thousand (a)
  (multiple-value-bind (a c) (truncate a 100)
    (multiple-value-bind (a b) (truncate a 10)
      (values
        (unless (zerop a) a)
        (unless (zerop b) b)
        (unless (zerop c) c)))))

(defun pradix100 (c)
  (if (< c 20)
    (pradix20 c)
    (multiple-value-bind (a b) (truncate c 10)
      (let ((a1 (zerop a))
            (b1 (zerop b)))
        (cond ((and a1 b1))
              (a1 (pradix (radix-20 b)))
              (b1 (pradix (radix-100 a)))
              (t (pradix (mkstr (radix-100 a t) "-" (radix-20 b)))))))))

(defparameter *radix-mode1* "")
(defparameter *radix-mode2* nil)

(defun radix-text-call (value index)
  (multiple-value-bind (a b c) (break-thousand value)
    (when (or b c)
      (when (<= 0 index)
        (let ((name (unit-string index *cardinal*)))
          (if *radix*
            (pradix (mkstr name *radix-mode1*))
            (pradix name))))
      (cond ((and b c)
             (pradix100 c)
             (when *radix-mode2*
               (pradix *radix-mode2*))
             (pradix (radix-hundred))
             (pradix20 b))
            (b (pradix (radix-hundred))
               (pradix20 b))
            (c (pradix100 c))))
    (when a
      (radix-text-call a (1+ index)))))

(defun radix-output (value &optional (cardinal t))
  (let ((*cardinal* cardinal)
        (*radix* nil)
        (sign (< value 0))
        (value (abs value)))
    (radix-text-call value -1)
    (when sign
      (pradix "minus"))
    (jout " " *radix*)))

(defun radix-stream (value stream &optional (cardinal t))
  (let ((*standard-output* stream))
    (radix-output value cardinal)))

(defun radix-string (value &optional (cardinal t))
  (with-output-to-string (*standard-output*)
    (radix-output value cardinal)))

