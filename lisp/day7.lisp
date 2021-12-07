(defpackage :day7
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
  (:shadowing-import-from :arrow-macros :-<>)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day7)

(defun spent-fuel (xs i)
  (->> (mapcar (lambda (number) (abs (- number i))) xs)
    (apply #'+)))

(defun part-1 ()
  (iter
    (with numbers = (->> (read-problem "day7.in")
                      car
                      (split ",")
                      (mapcar #'read-from-string)))
    (with minimum = (apply #'min numbers))
    (with maximum = (apply #'max numbers))
    (for i from minimum to maximum)
    (minimizing (spent-fuel numbers i))))

(defun sum-ints-to (x)
  (/ (* x (+ x 1)) 2))

(defun real-spent-fuel (xs i)
  (->> (mapcar (lambda (number) (sum-ints-to (abs (- number i)))) xs)
    (apply #'+)))

(defun part-2 ()
  (iter
    (with numbers = (->> (read-problem "day7.in")
                      car
                      (split ",")
                      (mapcar #'read-from-string)))
    (with minimum = (apply #'min numbers))
    (with maximum = (apply #'max numbers))
    (for i from minimum to maximum)
    (minimizing (real-spent-fuel numbers i))))
