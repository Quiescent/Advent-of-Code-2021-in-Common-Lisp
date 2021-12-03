(defpackage :lib
  (:use

   :cl
   :trivia
   :iterate
   :metabang-bind
   :anaphora
   :cl-heap)

  (:shadowing-import-from :arrow-macros :-<>)
  (:shadowing-import-from :arrow-macros :<>)
  (:export :read-problem)
  (:export :read-whole-file))

(in-package :lib)

(defun read-problem (file-name)
  (uiop:read-file-lines file-name))

(defun read-whole-file (file-name)
  (uiop:read-file-string file-name))
