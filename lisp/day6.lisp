(defpackage :day6
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

(in-package :day6)

(defun part-1 (&optional input-fishes (days 80))
  (iter
    (with fishes = (or input-fishes
                       (->> (read-problem "day6.in")
                         car
                         (split ",")
                         (mapcar #'read-from-string))))
    (for i from 0 below days)
    (setf fishes
          (iter
            (for fish in fishes)
            (cond
              ((= 0 fish) (progn (collecting 6)
                                 (collecting 8)))
              (t            (collecting (1- fish))))))
    (finally (return (length fishes)))))

(defun part-2 ()
  (let ((cache (make-hash-table :test #'equal)))
    (labels ((part-2-iter (day counter)
               (or #1=(gethash (cons day counter) cache)
                   (setf #1#
                         (if (= counter 0)
                             (part-2-iter (1- day) 6)
                             (if (<= counter day)
                                 (+ (part-2-iter (- day counter)   0)
                                    (part-2-iter (- day counter 1) 8))
                                 1))))))
      (iter
        (with fishes = (->> (read-problem "day6.in")
                         car
                         (split ",")
                         (mapcar #'read-from-string)))
        (for fish in fishes)
        (summing (part-2-iter (1- 256) fish))))))
