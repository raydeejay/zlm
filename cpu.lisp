;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:zlm)

;;; OPCODE METADATA

;; In order to read result/branch/string values when needed, metadata
;; about each opcode must be stored somehow, since it cannot be
;; derived from the bit pattern

;; This "metadata" is valid for V3.
(defparameter *branching-opcodes*
  (let ((2ops '(1 2 3 4 5 6 7 10))
        (1ops '(128 129 130))
        (0ops '(181 182 189)))
    (append 2ops
            (mapcar (lambda (x) (+ 32 x)) 2ops)
            (mapcar (lambda (x) (+ 64 x)) 2ops)
            (mapcar (lambda (x) (+ 96 x)) 2ops)
            1ops
            (mapcar (lambda (x) (+ 16 x)) 1ops)
            0ops
            (mapcar (lambda (x) (+ 16 x)) 0ops)))
  "Opcode numbers 32 to 127: other forms of 2OP with different types.
Opcode numbers 144 to 175: other forms of 1OP with different types.
Opcode numbers 192 to 223: VAR forms of 2OP:0 to 2OP:31.")

(defun branchingp (opcode)
  (member opcode *branching-opcodes*))

(defparameter *storing-opcodes*
  (let ((2ops '(8 9 15 16 17 18 19 20 21 22 23 24))
        (1ops '(129 130 131 132 142 143)))
    (append 2ops
            (mapcar (lambda (x) (+ 32 x)) 2ops)
            (mapcar (lambda (x) (+ 64 x)) 2ops)
            (mapcar (lambda (x) (+ 96 x)) 2ops)
            1ops
            (mapcar (lambda (x) (+ 16 x)) 1ops)))
  "Opcode numbers 32 to 127: other forms of 2OP with different types.
Opcode numbers 144 to 175: other forms of 1OP with different types.
Opcode numbers 192 to 223: VAR forms of 2OP:0 to 2OP:31.")

(defun storingp (opcode)
  (member opcode *storing-opcodes*))


;;; TYPESPECS
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


;;; OPERANDS
(defun decode-operands (ptr typespec)
  (loop :with offset := 0
     :for type :in typespec
     :collect (ecase type
                ((small-constant variable) (prog1 (byte-at (+ ptr offset)) (incf offset 1)))
                (large-constant (prog1 (word-at (+ ptr offset)) (incf offset 2))))))

(defun decode-store-variable (ptr)
  (values (byte-at ptr) 1))

(defun decode-branch (ptr)
  "Some instructions require a jump (or branch) to be made to another
part of the Z-program, depending on the outcome of some test.  These
instructions are followed by one or two bytes called a branch
argument.

Bit 7 of the first byte indicates when a branch occurs, a %0 meaning
that the branch logic is ‘reversed’: branch if the instruction doesn’t
want to, don’t if it does.

If bit 6 is %1, the branch argument consists of a single byte and the
branch offset is given by its bottom 6 bits (unsigned, i.e., from 0 to
63).

If bit 6 is %0, the branch argument consists of two bytes, and the
branch offset is given by the bottom 6 bits of the first byte followed
by all bits of the second (signed, i.e., from −8192 to 8191).

The following happens when a branch is to be made.  If the branch
offset is 0 or 1, then instead of branching the instruction rfalse or
rtrue, respectively, is carried out.  Otherwise, the branch is made by
setting the PC to

  Address after branch argument + Branch offset −2.

Note that a branch argument, if present, is always the last of a
sequence of arguments."
  (let* ((byte (byte-at ptr))
         (reversed (not (logbitp 7 byte))) ;; TODO consider this here or what?
         (short-address (logbitp 6 byte))
         (byte-offset (logand #b00111111 byte))
         (offset (if short-address
                     byte-offset
                     '(make-signed (+ (shl byte-offset 8 8) (byte-at (1+ ptr)))))))
    (case offset
      (0 (values 0 1))
      (1 (values 1 1))
      (otherwise (let ((offset-bytes (if short-address 1 2)))
                   (values (+ ptr offset-bytes offset -2) offset-bytes))))))

(defun decode-text (ptr))


;;; OPCODES
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
    (values `(,byte ,@(decode-operands (1+ ptr) typespec))
            3)))

(defun decode-short-opcode (ptr)
  "The first byte of a short instruction is of the form %10ttxxxx.
Here, %tt is the type of the operand (or %11 if absent), and %xxxx is
the 1OP (or if the operand is absent: 0OP) opcode number.  If the
operand is present, it follows the first byte."
  (let* ((byte (byte-at ptr))
         (typespec (2bit-typespec byte))
         (opcode (logand #b00001111 byte)))
    (values `(,byte ,@(decode-operands (1+ ptr) typespec))
            2))) ;; or 3....

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
         (typespec (varargs-typespec (byte-at (1+ ptr)))))
    `(,byte ,@(decode-operands (+ 2 ptr) typespec))))

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


;;; DECODING

;; the decode-* instructions return the opcode, and each of the
;; arguments, as multiple values

;; they should also return how many bytes they instruction takes?

;; they always get passed

(defun instruction-length (opcode typespec)
  ;; if varargs, it takes an additional byte
  ;; it double-vargars, it takes an additional two bytes
  ;; derive the amount of space consumed by arguments from the typespec
  ;;   storing opcodes take an additional byte
  ;;   branching opcodes take an additional 1 or 2 bytes
  ;;   string opcodes take an undetermined amount of additional bytes
  )

(defun decode (ptr)
  (let ((byte (byte-at ptr)))
    ;; opcode 0 nop?
    (multiple-value-bind (instr instr-length)
        (if (zerop byte)
            (list '0)
            (if (logbitp 7 byte)
                (if (logbitp 6 byte)
                    ;; opcodes 192-255 varop
                    (decode-vargars-opcode* ptr)
                    ;; opcodes 128-175 1op
                    ;; opcodes 176-191 0op
                    (decode-short-opcode ptr))
                ;; opcodes 1-127 2op
                (decode-long-opcode ptr)))
      (when (storingp (car instr))
        (nconc instr (list (decode-store-variable (+ ptr instr-length))))
        (incf instr-length 1))
      (when (branchingp (car instr))
        (multiple-value-bind (offset bytes) (decode-branch (+ ptr instr-length))
          (nconc instr (list offset))
          (incf instr-length bytes)))
      (values instr instr-length))))

;;; TOOLS
(defun dis* (ptr)
  (multiple-value-bind (instr instr-length) (decode ptr)
    (when (storingp (car instr))
      (nconc instr (list (decode-store-variable (+ ptr instr-length))))
      (incf instr-length 1))
    (when (branchingp (car instr))
      (multiple-value-bind (offset bytes) (decode-branch (+ ptr instr-length))
        (nconc instr (list offset))
        (incf instr-length bytes)))
    (values (format nil "~{~4,'0X~^ ~}" instr)
            instr-length)))

;;; OPERATION
(defun write-capabilities ()
  )

(defun init ()
  '(init-video)
  '(init-sound)
  '(load-program)
  ;; On restart only, the current value of the ‘printer transcript
  ;; bit’ (header bit $10-$11/0) is remembered
  (setf *stack* (list (list (header 'initial-pc ))))
  ())

(defun execute ()
  )
