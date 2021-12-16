(defpackage :day16
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

(in-package :day16)

(defun hex-to-binary (hex)
  (match hex
    (#\0 "0000")
    (#\1 "0001")
    (#\2 "0010")
    (#\3 "0011")
    (#\4 "0100")
    (#\5 "0101")
    (#\6 "0110")
    (#\7 "0111")
    (#\8 "1000")
    (#\9 "1001")
    (#\A "1010")
    (#\B "1011")
    (#\C "1100")
    (#\D "1101")
    (#\E "1110")
    (#\F "1111")))

(defun binary-string ()
  (->> (read-problem "day16.in")
    car
    (map 'list #'hex-to-binary)
    (apply #'concatenate 'string)))

(defun to-decimal (str)
  (iter
    (for i from 0 below (length str))
    (for char in-string str)
    (case char
      (#\0 nil)
      (#\1 (summing (expt 2 (- (length str) i 1)))))))

(defun digits-to-number (digits)
  (iter
    (for digit in digits)
    (for i from 0)
    (summing (* digit (expt 10 (- (length digits) i 1))))))

(defun parse-literal (input start)
  (let ((version (-> (subseq input start (+ start 3)) to-decimal))
        (type    (-> (subseq input (+ start 3) (+ start 6)) to-decimal)))
    (if (/= 4 type)
        (values nil nil nil)
        (iter
          (with pointer = (+ start 6))
          (for current-5 = (subseq input pointer (+ pointer 5)))
          (incf pointer 5)
          (collecting (subseq current-5 1) into result)
          (when (char= (aref current-5 0) #\0)
            (return (values (to-decimal (apply #'concatenate 'string result))
                            pointer
                            version)))))))

(defun compute (type arg-list)
  (case type
    (0 (apply #'+   arg-list))
    (1 (apply #'*   arg-list))
    (2 (apply #'min arg-list))
    (3 (apply #'max arg-list))
    (5 (if (> (car arg-list) (cadr arg-list)) 1 0))
    (6 (if (< (car arg-list) (cadr arg-list)) 1 0))
    (7 (if (= (car arg-list) (cadr arg-list)) 1 0))))

(defun parse-operation (input start)
  (let* ((version (-> (subseq input start (+ start 3)) to-decimal))
         (type    (-> (subseq input (+ start 3) (+ start 6)) to-decimal))
         (length-type      (aref input (+ start 6)))
         (length-of-length (if (char= length-type #\0) 15 11))
         (arg-start        (+ start 7 length-of-length))
         (length           (-> (subseq input (+ start 7) arg-start) to-decimal))
         (pointer          arg-start))
    (iter
      (while (and (not (every (lambda (char) (char= char #\0))
                              (subseq input pointer)))
                  (or (and (char= length-type #\0) (< pointer (+ length arg-start)))
                      (and (char= length-type #\1) (< (length result) length)))))
      (bind (((:values arg new-pointer sub-version-sum) (parse input pointer version-sum)))
        (if (null arg)
            (return (values result pointer (+ version version-sum)))
            (progn
              (summing (or sub-version-sum 0) into version-sum)
              (setf pointer new-pointer)
              (collecting arg into result))))
      (finally (return (values (compute type result) pointer (+ version version-sum)))))))

(defun parse (input start version-sum)
  (if (and (< start (length input))
           (not (every (lambda (char) (char= char #\0))
                       (subseq input start))))
      (let ((version (-> (subseq input start (+ start 3)) to-decimal))
            (type    (-> (subseq input (+ start 3) (+ start 6)) to-decimal)))
        (bind (((:values arg new-pointer sub-version-sum)
                (if (= type 4)
                    (parse-literal input start)
                    (parse-operation input start))))
          (values arg new-pointer (or sub-version-sum 0))))
      (values nil nil version-sum)))

(defun part-1 ()
  (bind ((input-string (binary-string))
         ((:values arg new-pointer version-sum) (parse input-string 0 0)))
    version-sum))

(defun part-2 ()
  (bind ((input-string (binary-string))
         ((:values arg new-pointer version-sum) (parse input-string 0 0)))
    arg))
