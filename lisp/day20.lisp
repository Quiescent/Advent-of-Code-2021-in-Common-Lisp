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

(defun index-from-position (map position map-bounds outside-is-lit)
  (bind (((x . y) position)
         ((min-x min-y max-x max-y) map-bounds))
    (iter outer
      (with i = 8)
      (for yi from (1- y) to (1+ y))
      (iter
        (for xi from (1- x) to (1+ x))
        (in outer (if (or (< xi min-x)
                          (> xi max-x)
                          (< yi min-y)
                          (> yi max-y))
                      (when outside-is-lit (summing (expt 2 i)))
                      (when (gethash (cons xi yi) map) (summing (expt 2 i)))))
        (decf i)))))

(defun part-1 ()
  (bind (((translations . vector-map) (parse-problem))
         (map (vector-to-map vector-map)))
    (iter
      (with current-map = map)
      (for i from 0 below 2)
      (for map-bounds = (map-bounds current-map))
      (for (min-x min-y max-x max-y) = map-bounds)
      (for next-map = (make-hash-table :test #'equal))
      (iter
        (for y from (- min-y 1) to (+ max-y 1))
        (iter
          (for x from (- min-x 1) to (+ max-x 1))
          (for binary-string = (index-from-position current-map (cons x y) map-bounds (oddp i)))
          (for index = binary-string)
          (when (char= (aref translations index) #\#)
            (setf (gethash (cons x y) next-map) t))))
      (setf current-map next-map)
      (print-map current-map)
      (format t "i: ~a~%" i)
      (finally
       (return
         (iter
           (for (key value) in-hashtable current-map)
           (counting t)))))))

;; 5619 correct!

;; 5685 wrong!
;; 5214 wrong!

(defun part-2 ()
  (bind (((translations . vector-map) (parse-problem))
         (map (vector-to-map vector-map)))
    (iter
      (with current-map = map)
      (for i from 0 below 50)
      (for map-bounds = (map-bounds current-map))
      (for (min-x min-y max-x max-y) = map-bounds)
      (for next-map = (make-hash-table :test #'equal))
      (iter
        (for y from (- min-y 1) to (+ max-y 1))
        (iter
          (for x from (- min-x 1) to (+ max-x 1))
          (for binary-string = (index-from-position current-map (cons x y) map-bounds (oddp i)))
          (for index = binary-string)
          (when (char= (aref translations index) #\#)
            (setf (gethash (cons x y) next-map) t))))
      (setf current-map next-map)
      (format t "i: ~a~%" i)
      (finally
       (return
         (iter
           (for (key value) in-hashtable current-map)
           (counting t)))))))
;; 248993 wrong!
;; 20122 correct!

;; Improving performance...
#+nil
(time (iter (for i from 0 below 10) (part-2)))

;; Before optimizing:
;;
;; Evaluation took:
;;   22.888 seconds of real time
;;   22.996608 seconds of total run time (22.752487 user, 0.244121 system)
;;   [ Run times consist of 0.639 seconds GC time, and 22.358 seconds non-GC time. ]
;;   100.48% CPU
;;   68,532,513,330 processor cycles
;;   5,531,417,872 bytes consed

;; After optimizing:
;;
;;  - Halved the time by optimizing binary-string-to-number.
;;  - Chopped off 4 seconds by avoiding allocating the binary string & converting straight to a number.
;;  (good enough for now.)
;;
;; Evaluation took:
;;   10.312 seconds of real time
;;   10.374868 seconds of total run time (10.244743 user, 0.130125 system)
;;   [ Run times consist of 0.301 seconds GC time, and 10.074 seconds non-GC time. ]
;;   100.61% CPU
;;   30,877,769,460 processor cycles
;;   2,695,546,608 bytes consed
