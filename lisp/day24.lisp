(defpackage :day24
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

(in-package :day24)


;; inp a - Read an input value and write it to variable a.
;; add a b - Add the value of a to the value of b, then store the result in variable a.
;; mul a b - Multiply the value of a by the value of b, then store the result in variable a.
;; div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a. (Here, "truncate" means to round the value toward zero.)
;; mod a b - Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the modulo operation.)
;; eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.


(defun parse-problem ()
  (->> (read-problem "day24.in")
    (mapcar (lambda (line) (match line
                             ((ppcre "inp ([wxyz])"
                                     (read a))
                              (list 'input a))
                             ((ppcre "add ([wxyz]) ([wxyz]|-?[0-9]+)"
                                     (read a) (read b))
                              (list 'add a b))
                             ((ppcre "mul ([wxyz]) ([wxyz]|-?[0-9]+)"
                                     (read a) (read b))
                              (list 'mul a b))
                             ((ppcre "div ([wxyz]) ([wxyz]|-?[0-9]+)"
                                     (read a) (read b))
                              (list 'div a b))
                             ((ppcre "mod ([wxyz]) ([wxyz]|-?[0-9]+)"
                                     (read a) (read b))
                              (list 'mod a b))
                             ((ppcre "eql ([wxyz]) ([wxyz]|-?[0-9]+)"
                                     (read a) (read b))
                              (list 'eql a b)))))))

(defun run-program (program input-buffer)
  (bind
      ((w 0)
       (x 0)
       (y 0)
       (z 0))
    (labels ((lookup (a)
               (if (symbolp a)
                   (case a (w w) (x x) (y y) (z z))
                   a))
             (set-reg (a b)
               (case a
                 (w (setf w b))
                 (x (setf x b))
                 (y (setf y b))
                 (z (setf z b)))))
      (iter
        (for (instruction . args) in program)
        (case instruction
          (input (set-reg (car args) (pop input-buffer)))
          (add   (set-reg (car args) (+ (lookup (car args))
                                        (lookup (cadr args)))))
          (mul   (set-reg (car args) (* (lookup (car args))
                                        (lookup (cadr args)))))
          (div   (set-reg (car args) (floor (lookup (car args))
                                            (lookup (cadr args)))))
          (mod   (set-reg (car args) (mod (lookup (car args))
                                          (lookup (cadr args)))))
          (eql   (set-reg (car args) (if (= (lookup (car args))
                                            (lookup (cadr args)))
                                         1
                                         0))))
        (return (= 0 z))))))

(defun part-1-again ()
  (iter outer
    (for d1 from 1 to 9)
    (for z1 = (+ 5 d1))
    (iter
      (for d2 from 1 to 9)
      (for z2 = (+ (* z1 26) 14 d2))
      (iter
        (for d3 from 1 to 9)
        (for z3 = (+ (* z2 26) 15 d3))
        (iter
          (for d4 from 1 to 9)
          (for z4 = (+ (* z3 26) 16 d4))
          (iter
            (for d5 from 1 to 9)
            (when (= (- (mod z4 26) 16) d5)
              (for z5 = (floor z4 26))
              (iter
                (for d6 from 1 to 9)
                (when (= (- (mod z5 26) 11) d6)
                  (for z6 = (floor z5 26))
                  (iter
                    (for d7 from 1 to 9)
                    (when (= (- (mod z6 26) 6) d7)
                      (for z7 = (floor z6 26))
                      (iter
                        (for d8 from 1 to 9)
                        (for z8 = (+ (* z7 26) 13 d8))
                        (iter
                          (for d9 from 1 to 9)
                          (for z9 = (+ (* z8 26) 16 d9))
                          (iter
                            (for d10 from 1 to 9)
                            (when (= (- (mod z9 26) 10) d10)
                              (for z10 = (floor z9 26))
                              (iter
                                (for d11 from 1 to 9)
                                (when (= (- (mod z10 26) 8) d11)
                                  (for z11 = (floor z10 26))
                                  (iter
                                    (for d12 from 1 to 9)
                                    (when (= (- (mod z11 26) 11) d12)
                                      (for z12 = (floor z11 26))
                                      (iter
                                        (for d13 from 1 to 9)
                                        (for z13 = (+ (* z12 26) 11 d13))
                                        (iter
                                          (for d14 from 1 to 9)
                                          (when (and (= (- (mod z13 26) 15) d14)
                                                     (= 0 (floor z13 26)))
                                            (in outer (collecting (list d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14)))))))))))))))))))))))))

(defun part-1 ()
  (iter
    (for numbers in (part-1-again))
    (maximizing (iter
                  (for pow from 13 downto 0)
                  (for digit in numbers)
                  (summing (* digit (expt 10 pow)))))))

;; 91599994399295 too low!
;; 91599994399395 correct!

(defun part-2 ()
  (iter
    (for numbers in (part-1-again))
    (minimizing (iter
                  (for pow from 13 downto 0)
                  (for digit in numbers)
                  (summing (* digit (expt 10 pow)))))))

;; 71111591176151 correct

;; Bin

(defun section-1-z (d1 d2 d3 d4)
  (let* ((z1 (+ d1 5))
         (z2 (+ (* z1 26) 14 d2))
         (z3 (+ (* z2 26) 15 d3))
         (z4 (+ (* z3 26) 16 d4)))
    z4))

(defun part-1-section-1 ()
  (iter outer
    (for d1 from 1 to 9)
    (iter
      (for d2 from 1 to 9)
      (iter
        (for d3 from 1 to 9)
        (iter
          (for d4 from 1 to 9)
          (iter
            (for d5 from 1 to 9)
            (when (= (mod (section-1-z d1 d2 d3 d4) 26) (+ d5 16))
              (in outer (collecting (list d1 d2 d3 d4 d5))))))))))

(defun section-2-z (d1 d2 d3 d4)
  (let ((z5 (floor (section-1-z d1 d2 d3 d4) 26)))
    z5))

(defun part-1-section-2 ()
  (bind ((section-1s (part-1-section-1)))
    (iter outer
      (for first-digits in section-1s)
      (for (d1 d2 d3 d4 d5) = first-digits)
      (iter
        (for d6 from 1 to 9)
        (when (= (mod (section-2-z d1 d2 d3 d4) 26) (+ d6 11))
          (in outer (collecting (append first-digits (list d6)))))))))

(defun section-3-z (d1 d2 d3 d4)
  (let* ((z6 (floor (section-2-z d1 d2 d3 d4) 26)))
    z6))

(defun part-1-section-3 ()
  (bind ((section-2s (part-1-section-2)))
    (iter outer
      (for second-digits in section-2s)
      (for (d1 d2 d3 d4 d5 d6) = second-digits)
      (iter
        (for d7 from 1 to 9)
        (when (= (mod (section-3-z d1 d2 d3 d4) 26) (+ d7 6))
          (in outer (collecting (append second-digits (list d7)))))))))

(defun section-4-z (d1 d2 d3 d4 d5 d6 d7 d8 d9)
  (let* ((z7 (floor (section-3-z d1 d2 d3 d4) 26))
         (z8 (+ (* z7 26) 13 d8))
         (z9 (+ (* z8 26) 16 d9)))
    z9))

(defun part-1-section-4 ()
  (bind ((section-3s (part-1-section-3)))
    (iter outer
      (for third-digits in section-3s)
      (for (d1 d2 d3 d4 d5 d6 d7) = third-digits)
      (iter
        (for d8 from 1 to 9)
        (iter
          (for d9 from 1 to 9)
          (iter
            (for d10 from 1 to 10)
            (when (= (mod (section-4-z d1 d2 d3 d4 d5 d6 d7 d8 d9) 26) (+ d10 10))
              (in outer (collecting (append third-digits (list d8 d9 d10)))))))))))

(defun section-5-z (d1 d2 d3 d4 d5 d6 d7 d8 d9)
  (let* ((z10 (floor (section-4-z d1 d2 d3 d4 d5 d6 d7 d8 d9) 26)))
    z10))

(defun part-1-section-5 ()
  (bind ((section-4s (part-1-section-4)))
    (iter outer
      (for fourth-digits in section-4s)
      (for (d1 d2 d3 d4 d5 d6 d7 d8 d9 d10) = fourth-digits)
      (iter
        (for d11 from 1 to 9)
        (when (= (mod (section-5-z d1 d2 d3 d4 d5 d6 d7 d8 d9) 26) (+ d11 8))
          (in outer (collecting (append fourth-digits (list d11)))))))))

(defun section-6-z (d1 d2 d3 d4 d5 d6 d7 d8 d9)
  (let ((z11 (floor (section-5-z d1 d2 d3 d4 d5 d6 d7 d8 d9))))
    z11))

(defun part-1-section-6 ()
  (bind ((section-5s (part-1-section-5)))
    (iter outer
      (for fifth-digits in section-5s)
      (for (d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11) = fifth-digits)
      (iter
        (for d12 from 1 to 9)
        (when (= (mod (section-6-z d1 d2 d3 d4 d5 d6 d7 d8 d9) 26) (+ d12 11))
          (in outer (collecting (append fifth-digits (list d12)))))))))

(defun section-7-z (d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13)
  (let* ((z12 (floor (section-6-z d1 d2 d3 d4 d5 d6 d7 d8 d9) 26))
         (z13 (+ (* z12 26) 11 d13)))
    z13))

(defun part-1-section-7 ()
  (bind ((section-6s (part-1-section-6)))
    (iter outer
      (for sixth-digits in section-6s)
      (for (d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12) = sixth-digits)
      (iter
        (for d13 from 1 to 9)
        (iter
          (for d14 from 1 to 9)
          (format t "z: ~a~%" (floor (section-7-z d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13) 26))
          (when (and (= (mod (section-7-z d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13) 26) (+ d14 15))
                     (= 0 (floor (section-7-z d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13) 26)))
            (in outer (collecting (append sixth-digits (list d14 d13))))))))))
