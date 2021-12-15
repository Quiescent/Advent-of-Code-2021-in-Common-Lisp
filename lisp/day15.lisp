(defpackage :day15
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

(in-package :day15)

(defun score (x y map)
  (+ (-> (aref map y) (aref x))))

(defun part-1 ()
  (iter
    (with map = (->> (read-problem "day15.in")
                  (map 'vector (lambda (line) (->> (split "" line)
                                           (map 'vector #'read-from-string))))))
    (with seen = (make-hash-table :test #'equal))
    (with max-x = (length (aref map 0)))
    (with max-y = (length map))
    (with to-explore = (make-instance 'cl-heap:priority-queue))
    (initially
     (cl-heap:enqueue to-explore
                      (cons (score 0 0 map) (cons 0 0))
                      (+ (score 0 0 map) max-x max-y)))
    (with best = nil)
    (for next = (cl-heap:dequeue to-explore))
    (while next)
    (for (next-score . (x . y)) = next)
    (setf (gethash (cons x y) seen) t)
    (for i from 0 below 10000000)
    (when (and best (> next-score best))
      (return best))
    (cond
      ((and (= x (1- max-x))
            (= y (1- max-y))
            (or (null best)
                (< next-score best)))
       (setf best next-score))
      (t (progn
           (when (and (< x (1- max-x)) (not (gethash (cons (1+ x) y) seen)))
             (let ((score (+ next-score (score (1+ x) y map))))
               (cl-heap:enqueue to-explore (cons score (cons (1+ x) y)) (+ score (- max-x (1+ x)) (- max-y y)))))
           (when (and (> x 0) (not (gethash (cons (1- x) y) seen)))
             (let ((score (+ next-score (score (1- x) y map))))
               (cl-heap:enqueue to-explore (cons score (cons (1- x) y)) (+ score (- max-x (1- x)) (- max-y y)))))
           (when (and (< y (1- max-y)) (not (gethash (cons x (1+ y)) seen)))
             (let ((score (+ next-score (score x (1+ y) map))))
               (cl-heap:enqueue to-explore (cons score (cons x (1+ y))) (+ score (- max-x x) (- max-y (1+ y))))))
           (when (and (> y 0) (not (gethash (cons x (1- y)) seen)))
             (let ((score (+ next-score (score x (1- y) map))))
               (cl-heap:enqueue to-explore (cons score (cons x (1- y))) (+ score (- max-x x) (- max-y (1- y)))))))))
    (finally (return (1- best)))))

;; 590

(defun incf-map (map times)
  (iter
    (for y from 0 below (length map))
    (for row = (aref map y))
    (collecting
     (iter
       (for x from 0 below (length row))
       (for a = (aref row x))
       (for new-value = (iter
                          (with result = a)
                          (for i from 0 below times)
                          (incf result)
                          (setf result (mod result 10))
                          (setf result (if (= 0 result) 1 result))
                          (finally (return result))))
       (collecting new-value :result-type vector))
     :result-type vector)))

(defun part-2 ()
  (iter
    (with map = (->> (read-problem "day15.in")
                  (map 'vector (lambda (line) (->> (split "" line)
                                           (map 'vector #'read-from-string))))))
    (with seen = (make-hash-table :test #'equal))
    (with max-x)
    (with max-y)
    (with to-explore = (make-instance 'cl-heap:priority-queue))
    (initially
     (setf map (iter
                 (for y from 0 below 5)
                 (collecting
                  (iter
                    (for x from 0 below 5)
                    (collecting (incf-map map (+ x y)))))))
     (setf map (apply #'concatenate
                      'vector
                      (iter
                        (for row in map)
                        (collecting
                         (iter
                           (for y from 0 below (length (car row)))
                           (collecting
                            (apply #'concatenate
                                   'vector
                                   (iter
                                     (for tile in row)
                                     (collecting (aref tile y))))
                            :result-type vector))))))
     (setf max-x (length (aref map 0)))
     (setf max-y (length map))
     (cl-heap:enqueue to-explore
                      (cons (score 0 0 map) (cons 0 0))
                      (+ (score 0 0 map) max-x max-y)))
    (with best = nil)
    (for next = (cl-heap:dequeue to-explore))
    (while next)
    (for (next-score . (x . y)) = next)
    (setf (gethash (cons x y) seen) t)
    (for i from 0 below 10000000)
    (when (and best (> next-score best))
      (return (1- best)))
    (cond
      ((and (= x (1- max-x))
            (= y (1- max-y))
            (or (null best)
                (< next-score best)))
       (setf best next-score))
      (t (progn
           (when (and (< x (1- max-x)) (not (gethash (cons (1+ x) y) seen)))
             (let ((score (+ next-score (score (1+ x) y map))))
               (cl-heap:enqueue to-explore (cons score (cons (1+ x) y)) (+ score (- max-x (1+ x)) (- max-y y)))))
           (when (and (> x 0) (not (gethash (cons (1- x) y) seen)))
             (let ((score (+ next-score (score (1- x) y map))))
               (cl-heap:enqueue to-explore (cons score (cons (1- x) y)) (+ score (- max-x (1- x)) (- max-y y)))))
           (when (and (< y (1- max-y)) (not (gethash (cons x (1+ y)) seen)))
             (let ((score (+ next-score (score x (1+ y) map))))
               (cl-heap:enqueue to-explore (cons score (cons x (1+ y))) (+ score (- max-x x) (- max-y (1+ y))))))
           (when (and (> y 0) (not (gethash (cons x (1- y)) seen)))
             (let ((score (+ next-score (score x (1- y) map))))
               (cl-heap:enqueue to-explore (cons score (cons x (1- y))) (+ score (- max-x x) (- max-y (1- y)))))))))
    (finally (return (1- best)))))
