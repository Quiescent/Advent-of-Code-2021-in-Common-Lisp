(defpackage :day1
  (:use

   :cl-ppcre
   :cl
   :trivia
   :iterate
   :metabang-bind
   :anaphora
   :cl-heap

   :lib)

  (:shadowing-import-from :arrow-macros :->)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day1)
