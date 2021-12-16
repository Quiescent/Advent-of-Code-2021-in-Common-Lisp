(defpackage :lib
  (:use

   :cl
   :trivia
   :iterate
   :metabang-bind
   :anaphora
   :cl-heap)

  (:shadowing-import-from :arrow-macros :-<>)
  (:shadowing-import-from :arrow-macros :<>)
  (:export :read-problem)
  (:export :read-whole-file)
  (:export :xgcd)
  (:export :md5))

(in-package :lib)

(defun read-problem (file-name)
  (uiop:read-file-lines file-name))

(defun read-whole-file (file-name)
  (uiop:read-file-string file-name))

(defun vector-sub (xs ys)
  (map 'vector (lambda (x y) (- x y)) xs ys))

(defun vector-sub-into (xs ys zs)
  (map-into zs (lambda (x y) (- x y)) xs ys))

(defun scaler-mult (x ys)
  (map 'vector (lambda (y) (* x y)) ys))

(defun scaler-mult-into (x ys zs)
  (map-into zs (lambda (y) (* y x)) ys))

(defun xgcd (m n)
  (iter
    (with vs = (vector m 1 0))
    (with ws = (vector n 0 1))
    (with xs = (vector 0 0 0))
    (while (not (zerop (aref ws 0))))
    (scaler-mult-into (floor (aref vs 0) (aref ws 0)) ws xs)
    (vector-sub-into vs xs xs)
    (for tmp = vs)
    (setf vs ws)
    (setf ws xs)
    (setf xs tmp)
    (finally (return (values (aref vs 0)
                             (aref vs 1)
                             (aref vs 2))))))

(defun md5 (str)
  (byte-array-to-hex-string
   (digest-sequence :md5 (flexi-streams:string-to-octets str))))

(defun hex-string-to-number (str)
  (read-from-string (format nil "#x~a" str)))

(defun binary-string-to-number (str)
  (read-from-string (format nil "#b~a" str)))

(defun number-string-to-number (str)
  (read-from-string str))

(defun hex-string-to-binary-string (str)
  (format nil "~B" (hex-string-to-number str)))

(defun hex-string-to-decimal-string (str)
  (format nil "~a" (hex-string-to-number str)))

(defun number-to-hex-string (str)
  (format nil "~x" str))

(defun number-to-binary-string (str)
  (format nil "~b" str))

(defun map-bounds (map)
  (iter
    (for ((x . y) _) in-hashtable map)
    (minimizing x into min-x)
    (maximizing x into max-x)
    (minimizing y into min-y)
    (maximizing y into max-y)
    (finally (return (list min-x min-y max-x max-y)))))

(defun print-map (map)
  (bind (((min-x min-y max-x max-y) (map-bounds map)))
    (iter
      (for y from min-y to max-y)
      (iter
        (for x from min-x to max-x)
        (if (gethash (cons x y) map)
            (format t "#")
            (format t " ")))
      (format t "~%"))))

(defun reverse-map (map &optional (test #'equal))
  (iter
    (with rev-map = (make-hash-table :test test))
    (for (key value) in-hashtable map)
    (setf (gethash value rev-map) key)
    (finally (return rev-map))))
