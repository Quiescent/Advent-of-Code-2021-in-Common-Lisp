(defpackage :day1
  (:use

   :cl-ppcre
   :cl
   :trivia
   :iterate
   :trivia.ppcre
   :metabang-bind
   :anaphora
   :cl-heap

   :lib)

  (:shadowing-import-from :arrow-macros :->)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day1)

(defun part-1 ()
  (iter
    (for line in (read-problem "day1.in"))
    (for depth = (read-from-string line))
    (for p-depth previous depth)
    (counting (and p-depth (> depth p-depth)))))

(defun part-2 ()
  (iter
    (for line in (read-problem "day1.in"))
    (for depth = (read-from-string line))
    (for p-depth previous depth)
    (for pp-depth previous p-depth)
    (for ppp-depth previous pp-depth)
    (when ppp-depth
      (counting (> (+ depth p-depth pp-depth)
                   (+ ppp-depth pp-depth p-depth))))))
