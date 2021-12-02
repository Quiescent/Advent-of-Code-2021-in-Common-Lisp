(defpackage :day2
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

(in-package :day2)

(defun part-1 ()
  (iter
    (with x = 0)
    (with z = 0)
    (for line in (read-problem "day2.in"))
    (match line
      ((ppcre "forward ([0-9]+)"
              (read amount))
       (incf x amount))
      ((ppcre "up ([0-9]+)"
              (read amount))
       (decf z amount))
      ((ppcre "down ([0-9]+)"
              (read amount))
       (incf z amount)))
    (finally (return (* x z)))))


(defun part-2 ()
  (iter
    (with x = 0)
    (with z = 0)
    (with aim = 0)
    (for line in (read-problem "day2.in"))
    (match line
      ((ppcre "forward ([0-9]+)"
              (read amount))
       (incf x amount)
       (incf z (* amount aim)))
      ((ppcre "up ([0-9]+)"
              (read amount))
       (decf aim amount))
      ((ppcre "down ([0-9]+)"
              (read amount))
       (incf aim amount)))
    (finally (return (* x z)))))
