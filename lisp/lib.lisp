(defpackage :lib
  (:use

   :cl
   :trivia
   :iterate
   :metabang-bind
   :anaphora
   :cl-heap)

  (:export :read-problem))

(in-package :lib)

(defun read-problem (file-name)
  (with-open-file (input file-name)
    (iter
      (for line = (read-line input nil nil))
      (while line)
      (collecting line))))
