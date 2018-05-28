;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:zlm)

(defun 1bit-typespec (byte)
  "Decode the operand types for a 2OP long opcode, and return them as
a list."
  (list (if (logbitp 6 byte)
            'variable
            'small-constant)
        (if (logbitp 5 byte)
            'variable
            'small-constant)))

(defun 2bit-typespec (byte)
  "Decode the operand types for a short opcode, and returns them as a
list."
  (case (shr byte 2 4)
    ((#b00) '(large-constant))
    ((#b01) '(small-constant))
    ((#b10) '(variable))
    ((#b11) (values))))

(defun varargs-typespec (byte)
  "Decode the operand types for a varargs or 2OP/varargs opcode, and
returns them as a list."
  (loop :for offset :from 6 :downto 0 :by 2
     :for bits := (shr byte 2 offset)
     :until (= bits #b11)
     :collect (ecase bits
                ;; ((#b11) 'absent)
                (#b00 'large-constant)
                (#b01 'small-constant)
                (#b10 'variable))))

(defun decode-operands (ptr typespec)
  (loop :with offset := 0
     :for type :in typespec
     :collect (ecase type
                ((small-constant variable) (prog1 (byte-at (+ ptr offset)) (incf offset 1)))
                (large-constant (prog1 (word-at (+ ptr offset)) (incf offset 2))))))

(defun decode-store-variable (ptr))

(defun decode-branch-offset (type ptr))

(defun decode-text (ptr))


(defun decode-long-opcode (ptr)
  "The first byte of a long instruction is of the form %0abxxxxx.
Here, %xxxxx is the 2OP opcode number, and %a and %b are abbreviated
type-indicators for the two operands:

 %0: a byte constant;
 %1: a variable number.

These correspond to the types %01 and %10, respectively.  The next two
bytes of the instruction contain the operands, first %a, then %b.
"
  (let* ((byte (byte-at ptr))
         (typespec (1bit-typespec byte))
         (opcode (logand #b00011111 byte)))
    `(,byte ,@(decode-operands (1+ ptr) typespec))))

(defun decode-short-opcode (ptr)
  "The first byte of a short instruction is of the form %10ttxxxx.
Here, %tt is the type of the operand (or %11 if absent), and %xxxx is
the 1OP (or if the operand is absent: 0OP) opcode number.  If the
operand is present, it follows the first byte."
  (let* ((byte (byte-at ptr))
         (typespec (2bit-typespec byte))
         (opcode (logand #b00001111 byte)))
    `(,byte ,@(decode-operands (1+ ptr) typespec))))

(defun decode-vargars-opcode (ptr)
  "The first byte of a variable instruction is of the form
%11axxxxx (except for the two double variable instructions, discussed
below), where %xxxxx is the (2OP or VAR) opcode number.

If %a is %0 then this instruction contains a 2OP opcode; if %1, a VAR
opcode.  It is an error for a variable instruction with a 2OP opcode
not to have two operands, except when the opcode is 2OP:$1 (i.e., the
instruction is je).  The second byte is a type byte, and contains the
type information for the operands.  It is divided into pairs of two
bits, each indicating the type of an operand, beginning with the top
bits.

The following bytes contain the operands in that order.  A %11 pair
means ‘no operand’; it is an error if a %11 bit pair occurs before a
non-%11 pair."
  (let* ((byte (byte-at ptr))
         (opcode (logand #b00011111 byte))
         (typespec (varargs-typespec (1+ ptr))))
    `(,byte ,@(decode-operands (+ 2 ptr) typespec))
    ;; (if (logbitp 5 byte)
    ;;     (progn ;; var
    ;;       (list byte 'varargs))
    ;;     (progn ;; 2op
    ;;       (if (= opcode 1)
    ;;           (list byte 'one-arg)
    ;;           (list byte 'two-args))))
    ))

;; NOTE: These are not valid opcodes in V3
(defun decode-double-vargars-opcode (ptr)
  "There are two double variable instructions, viz. %11101100 ($EC,
opcode VAR:$C) and %11111010 ($FA, opcode VAR:$1A). Their structure is
identical to that of variable instructions, except that they do not
have a type byte, but instead a type word."
  (let* ((byte (byte-at ptr))
         (opcode (logand #b00011111 byte)))
    ;; decode-types-word
    (list byte 'type-word)))

(defun decode-vargars-opcode* (ptr)
  "There are two double variable instructions, viz.

  %11101100 ($EC, opcode VAR:$0C) and
  %11111010 ($FA, opcode VAR:$1A).

Their structure is identical to that of variable instructions, except
that they do not have a type byte, but instead a type word."
  (let ((byte (byte-at ptr)))
    (if (member byte '(#xEC #xFA))
        (decode-double-vargars-opcode ptr)
        (decode-vargars-opcode ptr))))

;; the decode-* instructions return the opcode, and each of the
;; arguments, as multiple values

;; they should also return how many bytes they instruction takes?

;; they always get passed

(defun decode (ptr)
  (let ((byte (byte-at ptr)))
    ;; opcode 0 nop?
    (if (zerop byte)
        (list '0)
        (if (logbitp 7 byte)
            (if (logbitp 6 byte)
                ;; opcodes 224-255 varop
                (decode-vargars-opcode* ptr)
                ;; opcodes 128-175 1op
                ;; opcodes 176-223 0op
                (decode-short-opcode ptr))
            ;; opcodes 1-127 2op
            (decode-long-opcode ptr)))
    
    ;; read operands?
    ))


;; types are needed when decoding only because we need to know how may
;; bytes to read

;; the decode- functions should take a memory address, so they can do further reads
;; they should return how many bytes they read

;; so DECODE calls the relevant decode-* function, passing PTR, and
;; they will determine which types are to be read, and call the
;; relevant functions to read them
