(defpackage :day17
  (:use

   :cl-ppcre
   :cl
   :trivia
   :iterate
   :trivia.ppcre
   :metabang-bind
   :anaphora
   :queues

   :lib)

  (:shadowing-import-from :arrow-macros :->)
  (:shadowing-import-from :arrow-macros :-<>)
  (:shadowing-import-from :arrow-macros :<>)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day17)

;; target area: x=88..125, y=-157..-103
(defun part-1 ()
  (let ((min-x 88)
        (max-x 125)
        (max-y -103)
        (min-y -157))
    (iter
      (for initial-vx from 1 below 1000)
      (maximizing
       (iter
         (for initial-vy from 1 below 1000)
         (maximizing (iter
                       (with x = 0)
                       (with y = 0)
                       (with vx = initial-vx)
                       (with vy = initial-vy)
                       (incf x vx)
                       (incf y vy)
                       (setf vx (max (1- vx) 0))
                       (decf vy)
                       (maximizing y into highest)
                       (when (or (> x max-x) (< y min-y))
                         (return most-negative-fixnum))
                       (when (and (<= x max-x)
                                  (>= x min-x)
                                  (<= y max-y)
                                  (>= y min-y))
                         (return highest)))))))))

(defun part-2 ()
  (let ((min-x 88)
        (max-x 125)
        (max-y -103)
        (min-y -157))
    (iter
      (for initial-vx from 1 below 1000)
      (summing
       (iter
         (for initial-vy from -1000 below 1000)
         (counting (iter
                     (with x = 0)
                     (with y = 0)
                     (with vx = initial-vx)
                     (with vy = initial-vy)
                     (incf x vx)
                     (incf y vy)
                     (setf vx (max (1- vx) 0))
                     (decf vy)
                     (when (or (> x max-x) (< y min-y))
                       (return nil))
                     (when (and (<= x max-x)
                                (>= x min-x)
                                (<= y max-y)
                                (>= y min-y))
                       (return t)))))))))
