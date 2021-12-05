(defpackage :day5
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

(in-package :day5)

(defun part-1 ()
  (iter
    (with seen = (make-hash-table :test #'equal))
    (for line in (read-problem "day5.in"))
    (format t "line: ~a~%" line)
    (match line
      ((ppcre "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)"
              (read x1) (read y1) (read x2) (read y2))
       (format t "(list x1 y1 x2 y2): ~a~%" (list x1 y1 x2 y2))
       (when (= x1 x2)
         (iter
           (for y from (min y1 y2) to (max y1 y2))
           (incf (gethash (cons x1 y) seen 0))))
       (when (= y1 y2)
         (iter
           (for x from (min x1 x2) to (max x1 x2))
           (incf (gethash (cons x y1) seen 0))))))
    (finally (return
               (iter
                 (for (key value) in-hashtable seen)
                 (counting (> value 1)))))))

(defun part-2 ()
  (iter
    (with seen = (make-hash-table :test #'equal))
    (for line in (read-problem "day5.in"))
    (match line
      ((ppcre "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)"
              (read x1) (read y1) (read x2) (read y2))
       (cond
         ((= x1 x2)
         (iter
            (for y from (min y1 y2) to (max y1 y2))
            (incf (gethash (cons x1 y) seen 0))))
         ((= y1 y2)
          (iter
            (for x from (min x1 x2) to (max x1 x2))
            (incf (gethash (cons x y1) seen 0))))
         (t (iter
              (with x = x1)
              (with y = y1)
              (with inc-y = (if (< y1 y2) 1 -1))
              (with inc-x = (if (< x1 x2) 1 -1))
              (incf (gethash (cons x y) seen 0))
              (while (and (/= x x2) (/= y y2)))
              (incf x inc-x)
              (incf y inc-y))))))
    (finally (return
               (iter
                 (for (key value) in-hashtable seen)
                 (counting (> value 1)))))))
