(defpackage :day3
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

(in-package :day3)

(defun part-1 ()
  (iter
    (for line in (read-problem "day3.in"))
    (with counts = (make-hash-table :test #'equal))
    (iter
      (for char in-string (reverse line))
      (for i from 0)
      (when (null (gethash i counts))
        (setf (gethash i counts) (make-hash-table :test #'equal)))
      (case char
        (#\0 (incf (gethash 0 (gethash i counts) 0)))
        (#\1 (incf (gethash 1 (gethash i counts) 0)))))
    (finally
     (return
       (iter
         (for (pos table) in-hashtable counts)
         (for radix from 0)
         (for more-ones = (> (gethash 1 table) (gethash 0 table)))
         (if more-ones
             (summing (expt 2 radix) into a)
             (summing (expt 2 radix) into b))
         (finally (return (* a b))))))))

(defun oxygen (xs)
  (iter
    (while (> (length xs) 1))
    (with i = 0)
    (setf xs (iter
               (with counts = (make-hash-table :test #'equal))
               (for candidate in xs)
               (for char = (aref candidate i))
               (incf (gethash char counts 0))
               (finally
                (return
                  (iter
                    (with target = (if (>= (gethash #\1 counts) (gethash #\0 counts)) #\1 #\0))
                    (for candidate in xs)
                    (when (char= (aref candidate i) target)
                      (collecting candidate)))))))
    (incf i)
    (setf i (mod i (length (car xs))))
    (finally (return (car xs)))))

(defun c02 (xs)
  (iter
    (while (> (length xs) 1))
    (with i = 0)
    (setf xs (iter
               (with counts = (make-hash-table :test #'equal))
               (for candidate in xs)
               (for char = (aref candidate i))
               (incf (gethash char counts 0))
               (finally
                (return
                  (iter
                    (with target = (if (<= (gethash #\0 counts) (gethash #\1 counts)) #\0 #\1))
                    (for candidate in xs)
                    (when (char= (aref candidate i) target)
                      (collecting candidate)))))))
    (incf i)
    (setf i (mod i (length (car xs))))
    (finally (return (car xs)))))

(defun part-2 ()
  (let ((o (oxygen (read-problem "day3.in")))
        (c (c02    (read-problem "day3.in"))))
    (*
     (iter
       (for char in-string (reverse o))
       (for radix from 0)
       (for more-ones = (char= char #\1))
       (when more-ones (summing (expt 2 radix))))
     (iter
       (for char in-string (reverse c))
       (for radix from 0)
       (for more-ones = (char= char #\1))
       (when more-ones (summing (expt 2 radix)))))))
