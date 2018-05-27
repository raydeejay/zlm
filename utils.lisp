;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:zlm)

(defun shl (x width bits)
  "Compute bitwise left shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x bits)
          (1- (ash 1 width))))

(defun shr (x width bits)
  "Compute bitwise right shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x (- bits))
          (1- (ash 1 width))))

(defun rotl (x width bits)
  "Compute bitwise left rotation of x by 'bits' bits, represented on 'width' bits"
  (logior (logand (ash x (mod bits width))
                  (1- (ash 1 width)))
          (logand (ash x (- (- width (mod bits width))))
                  (1- (ash 1 width)))))

(defun rotr (x width bits)
  "Compute bitwise right rotation of x by 'bits' bits, represented on 'width' bits"
  (logior (logand (ash x (- (mod bits width)))
                  (1- (ash 1 width)))
          (logand (ash x (- width (mod bits width)))
                  (1- (ash 1 width)))))
