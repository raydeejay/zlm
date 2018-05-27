;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:zlm)

(defparameter *current-alphabet* 0)

(defparameter *alphabets*
  (list (loop :for c :across "......abcdefghijklmnopqrstuvwxyz" :collecting c)
        (loop :for c :across "......ABCDEFGHIJKLMNOPQRSTUVWXYZ" :collecting c)
        (loop :for c :across (format nil ".......~C0123456789.,!?_#'\"/\-:()" #\Newline) :collecting c)))

;; 5 bit z-chars are one thing
;; the zsciii table is another

(defun decode-zscii (value)
  "Return the ZSCII char for value. This implementation is a
huge hack, but it should nevertheless work well enough.."
  (setf *current-alphabet* 0)
  (code-char value))

(defun decode-zchar (zchar)
  "Translate a value into a char (and reset the alphabet to 0), or set the current alphabet."
  (let ((result (cond
                  ((= zchar 0) #\Space)
                  ;; 1/2/3 are handled by expand-abbrevs
                  ((= zchar 4) (setf *current-alphabet* 1) (return-from decode-zchar nil))
                  ((= zchar 5) (setf *current-alphabet* 2) (return-from decode-zchar nil))
                  ;; 6 is also handled by expand-abbrevs
                  ;; there's probably a potential bug here...
                  (T (nth zchar (nth *current-alphabet* *alphabets*))))))
    (setf *current-alphabet* 0)
    result))

;; first extract all the zchars until we hit the end bit
(defun collect-zchars (ptr &optional result)
  "Extract zchars from a memory position untili the end bit is set in
the processed word."
  (let ((word (word-at ptr)))
    (let ((c1 (shr (logand word #b0111110000000000) 16 10))
          (c2 (shr (logand word #b0000001111100000) 16 5))
          (c3 (logand word #b0000000000011111)))
      (push c1 result)
      (push c2 result)
      (push c3 result))
    (if (logbitp 15 word)
        (progn (setf *current-alphabet* 0)
               (reverse result))
        (collect-zchars (+ 2 ptr) result))))

;; then translate them in sequence
(defun zchars->string (zchars)
  (coerce (remove-if #'null (mapcar #'decode-zchar zchars)) 'string))

(defun abbrev->string (table n)
  "In Versions 3 and later, Z-characters 1, 2 and 3 represent
abbreviations, sometimes also called 'synonyms' (for traditional
reasons): the next Z-character indicates which abbreviation string to
print. If z is the first Z-character (1, 2 or 3) and x the subsequent
one, then the interpreter must look up entry 32(z-1)+x in the
abbreviations table and print the string at that word address."
  (zchars->string (collect-zchars (* 2 (word-at (+ (header 'abbreviations-base)
                                                   (* 32 (1- table))
                                                   (* 2 n)))))))

;; expand abbreviations
(defun expand-abbrevs (seq)
  "Returns a copy of the sequence of zchars, with abbreviations expanded
in-place."
  (let (result)
    (do ((copy seq)) ((null copy))
      (cond ((and (= (car copy) 6) (= *current-alphabet* 2))
             (push (list (decode-zscii (+ (shl (cadr copy) 8 5) (caddr copy)))) result)
             (setf copy (cddr copy)))
            ((= (car copy) 1)
             (push (abbrev->string 1 (cadr copy)) result)
             (setf copy (cdr copy)))
            ((= (car copy) 2)
             (push (abbrev->string 2 (cadr copy)) result)
             (setf copy (cdr copy)))
            ((= (car copy) 3)
             (push (abbrev->string 3 (cadr copy)) result)
             (setf copy (cdr copy)))
            (T (let ((zc (decode-zchar (car copy))))
                 (when zc (push (list zc) result)))))
      (setf copy (cdr copy)))
    (apply #'concatenate 'string (reverse result))))

;; this should probably be the single entry point
(defun fetch-zstring (ptr)
  "Decode a zstring (sequence of zchars) from a memory position,
expanding abbreviations as necessary."
  (expand-abbrevs (collect-zchars ptr)))

(defun dump-abbrevs-table ()
  "Print the whole abbreviations table."
  (do ((table 1 (incf table))) ((> table 3))
    (do ((i 0 (incf i))) ((= i 32))
      (format t "[~2D] \"~A\"~%"
              (+ (* 32 (1- table)) i)
              (abbrev->string table i)))))
