(defpackage :day25
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

(in-package :day25)

(defun parse-problem ()
  (->> (read-problem "day25.in")
    (map 'vector #'identity)))

(defun step-right (map next-map x-dim y-dim)
  (iter
    (with moved = nil)
    (with moved-to = (make-hash-table :test #'equal))
    (for y from 0 below y-dim)
    (iter
      (for x from 0 below x-dim)
      (for square = (-> (aref map y) (aref x)))
      (when (and (not (gethash (cons x y) moved-to))
                 (not (char= square #\>)))
        (setf (-> (aref next-map y) (aref x)) square)
        (next-iteration))
      (for next-x = (mod (1+ x) x-dim))
      (for square-right = (-> (aref map y) (aref next-x)))
      (when (and (char= square-right #\.)
                 (char= square #\>))
        (setf moved t)
        (setf (gethash (cons next-x y) moved-to) t)
        (setf (-> (aref next-map y) (aref next-x)) #\>)
        (setf (-> (aref next-map y) (aref x))      #\.)))
    (finally (return moved))))

(defun step-down (map next-map x-dim y-dim)
  (iter
    (with moved = nil)
    (with moved-to = (make-hash-table :test #'equal))
    (for y from 0 below y-dim)
    (iter
      (for x from 0 below x-dim)
      (for square = (-> (aref map y) (aref x)))
      (when (and (not (gethash (cons x y) moved-to))
                 (not (char= square #\v)))
        (setf (-> (aref next-map y) (aref x)) square)
        (next-iteration))
      (for next-y = (mod (1+ y) y-dim))
      (for square-down = (-> (aref map next-y) (aref x)))
      (when (and (char= square-down #\.)
                 (char= square #\v))
        (setf (gethash (cons x next-y) moved-to) t)
        (setf moved t)
        (setf (-> (aref next-map next-y) (aref x)) #\v)
        (setf (-> (aref next-map y)      (aref x)) #\.)))
    (finally (return moved))))

(defun part-1 ()
  (iter
    (with map = (parse-problem))
    (with next-map = (parse-problem))
    (with y-dim = (length map))
    (with x-dim = (length (aref map 0)))
    (for step-right = (step-right map next-map x-dim y-dim))
    (for step-down = (step-down next-map map x-dim y-dim))
    (counting t)
    (while (or step-right step-down))))
