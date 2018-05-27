;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:zlm)


(defun op/jz (a branch)
  (when (zerop a)
    ;;jump to branch
    ))

(defun op/je (a branch &optional b1 b2 b3)
  (when (member a (list b1 b2 b3))
    ;;jump to branch
    ))

