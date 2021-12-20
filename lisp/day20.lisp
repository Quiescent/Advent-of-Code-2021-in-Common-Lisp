(defpackage :day20
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

(in-package :day20)

(defun parse-problem ()
  (let ((input (read-problem "day20.in")))
    (cons (car input)
          (->> (cddr input)
            (map 'vector #'identity)))))

(defun vector-to-map (vector)
  (iter
    (with map = (make-hash-table :test #'equal))
    (for y from 0 below (length vector))
    (iter
      (for x from 0 below (length (aref vector 0)))
      (for char = (-> (aref vector y) (aref x)))
      (when (char= char #\#)
        (setf (gethash (cons x y) map) t)))
    (finally (return map))))

(defun string-from-position (map position)
  (bind (((x . y) position))
    (iter outer
      (for yi from (1- y) to (1+ y))
      (iter
        (for xi from (1- x) to (1+ x))
        (in outer (collecting (if (gethash (cons xi yi) map) #\1 #\0)
                              :result-type 'string))))))

(defun part-1 ()
  (bind (((translations . vector-map) (parse-problem))
         (map (vector-to-map vector-map)))
    (iter
      (with current-map = map)
      (for i from 0 below 2)
      (for (min-x min-y max-x max-y) = (map-bounds current-map))
      (for next-map = (make-hash-table :test #'equal))
      (iter
        (for y from (- min-y 50) to (+ max-y 50))
        (iter
          (for x from (- min-x 50) to (+ max-x 50))
          (for binary-string = (string-from-position current-map (cons x y)))
          (for index = (binary-string-to-number binary-string))
          (when (char= (aref translations index) #\#)
            (setf (gethash (cons x y) next-map) t))))
      (setf current-map next-map)
      (print-map current-map)
      (format t "~%")
      (finally
       (return
         (iter
           (for (key value) in-hashtable current-map)
           (counting t)))))))

;; 5619

;; 5685 wrong
;; 5214 wrong

(defun part-2 ()
  (bind (((translations . vector-map) (parse-problem))
         (map (vector-to-map vector-map)))
    (iter
      (with current-map = map)
      (for i from 0 below 50)
      (for (min-x min-y max-x max-y) = (map-bounds current-map))
      (for next-map = (make-hash-table :test #'equal))
      (iter
        (for y from (- min-y 9) to (+ max-y 9))
        (iter
          (for x from (- min-x 9) to (+ max-x 9))
          (for binary-string = (string-from-position current-map (cons x y)))
          (for index = (binary-string-to-number binary-string))
          (when (char= (aref translations index) #\#)
            (setf (gethash (cons x y) next-map) t))))
      (setf current-map next-map)
      ;;(print-map current-map)
      (format t "~%")
      (finally
       (progn
         (print-map current-map)
        (return
          (iter
            (for (key value) in-hashtable current-map)
            (counting t))))))))
;; 248993 wrong!
