(defpackage :day11
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
  (:shadowing-import-from :arrow-macros :<>)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day11)

(defvar flashes 0)
(defvar flashes-this-round (list))

(defun flash (octopuses x-in y-in)
  (when (not (member (cons x-in y-in) flashes-this-round :test #'equal))
    (iter
      (initially
       (setf (-> (aref octopuses y-in) (aref x-in)) 0)
       (incf flashes)
       (push (cons x-in y-in) flashes-this-round))
      (for y from (max 0 (1- y-in)) below (min 10 (+ 2 y-in)))
      (iter
        (for x from (max 0 (1- x-in)) below (min 10 (+ 2 x-in)))
        (when (and (= x x-in)
                   (= y y-in))
          (next-iteration))
        (for e = #1=(aref (aref octopuses y) x))
        (if (= 9 e)
            (flash octopuses x y)
            (incf #1#))))))

(defun part-1 ()
  (let ((flashes 0))
    (iter
      (with octopuses = (->> (read-problem "day11.in")
                          (map 'vector (lambda (line) (->> (split "" line)
                                                        (map 'vector #'read-from-string))))))
      (for i from 0 below 100)
      (setf flashes-this-round (list))
      (iter
        (for y from 0 below 10)
        (iter
          (for x from 0 below 10)
          (incf (aref (aref octopuses y) x))))
      (iter
        (for y from 0 below 10)
        (iter
          (for x from 0 below 10)
          (for e = (-> (aref octopuses y) (aref x)))
          (when (> e 9)
            (flash octopuses x y))))
      (iter
        (for (x . y) in flashes-this-round)
        (setf (-> (aref octopuses y) (aref x)) 0))
      (finally (return flashes)))))


(defun part-2 ()
  (let ((flashes 0))
    (iter
      (with octopuses = (->> (read-problem "day11.in")
                          (map 'vector (lambda (line) (->> (split "" line)
                                                        (map 'vector #'read-from-string))))))
      (for i from 0 below 10000)
      (setf flashes-this-round (list))
      (iter
        (for y from 0 below 10)
        (iter
          (for x from 0 below 10)
          (incf (aref (aref octopuses y) x))))
      (iter
        (for y from 0 below 10)
        (iter
          (for x from 0 below 10)
          (for e = (-> (aref octopuses y) (aref x)))
          (when (> e 9)
            (flash octopuses x y))))
      (when (= (length flashes-this-round) 100)
        (return (1+ i)))
      (iter
        (for (x . y) in flashes-this-round)
        (setf (-> (aref octopuses y) (aref x)) 0)))))
