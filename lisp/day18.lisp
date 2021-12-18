(defpackage :day18
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

(in-package :day18)

(defun parse-problem ()
  (->> (read-problem "day18.in")
    (mapcar (lambda (line) (->> (map 'string
                            (lambda (char) (case char
                                        (#\[ #\()
                                        (#\] #\))
                                        (#\, #\Space)
                                        (t   char)))
                            line)
                        read-from-string)))))

(defun magnitute (cell)
  (let ((left (if (consp (car cell))
                  (magnitute (car cell))
                  (car cell)))
        (right (if (consp (cadr cell))
                   (magnitute (cadr cell))
                   (cadr cell))))
    (+ (* 3 left)
       (* 2 right))))
;; (list (list 1 2) (list (list 3 4) 5))
;; eg [[1,2],[[3,4],5]] => 143

(defun add-to-right-most (xs y)
  (if (consp (cadr xs))
      (add-to-right-most (cadr xs) y)
      (incf (cadr xs) y)))
;; (let ((xs (list (list 1 2) (list (list 3 4) 5)))) (add-to-right-most xs 2) xs)

(defun add-to-left-most (xs y)
  (if (consp (car xs))
      (add-to-left-most (car xs) y)
      (incf (car xs) y)))
;; (let ((xs (list (list 1 2) (list (list 3 4) 5)))) (add-to-left-most xs 2) xs)

(defun explode (cell parent depth)
  (if (not (consp cell))
      (values nil nil nil)
      (if (= depth 4)
          (progn
            (setf (nth (position cell parent) parent) 0)
            (values (car cell) (cadr cell) t))
          (bind (((:values add-left add-right done) (explode (car cell) cell (1+ depth))))
            (cond
              (done (values add-left
                            (if (and add-right (> (length cell) 1))
                                (progn
                                  (add-to-left-most (cdr cell) add-right)
                                  nil)
                                add-right)
                            t))
              ((not done)
               (bind (((:values add-left add-right done) (explode (cadr cell) cell (1+ depth))))
                 (if (not done)
                     (values nil nil nil)
                     (values (if add-left
                                 (progn
                                   (if (consp (car cell))
                                       (add-to-right-most (car cell) add-left)
                                       (add-to-left-most  cell       add-left))
                                      nil))
                             (if (and add-right (> (length cell) 2))
                                 (progn
                                   (add-to-left-most (cddr cell) add-right)
                                   nil)
                                 add-right)
                             t)))))))))
;; [[[[[9,8],1],2],3],4]
;; (list (list (list (list (list 9 8) 1) 2) 3) 4)
;; (let ((xs (list (list (list (list (list 9 8) 1) 2) 3) 4))) (explode xs nil 0) xs)
;; [7,[6,[5,[4,[3,2]]]]]
;; (list 7 (list 6 (list 5 (list 4 (list 3 2)))))
;; (let ((xs (list 7 (list 6 (list 5 (list 4 (list 3 2))))))) (explode xs nil 0) xs)
;; [[6,[5,[4,[3,2]]]],1]
;; (list (list 6 (list 5 (list 4 (list 3 2)))) 1)
;; (let ((xs (list (list 6 (list 5 (list 4 (list 3 2)))) 1))) (explode xs nil 0) xs)

(defun split (cell parent)
  (if (not (consp cell))
      nil
      (let ((x   (car cell))
            (idx (position cell parent)))
        (if (and (numberp x) (>= x 10))
            (setf (car cell)
                  (list (floor x 2) (ceiling x 2)))
            (or (and (consp x) (split x cell))
                (split (cdr cell) parent))))))
;; [[[[0,7],4],[15,[0,13]]],[1,1]]
;; (list (list (list (list 0 7) 4) (list 15 (list 0 13))) (list 1 1))
;; (let ((xs (list (list (list (list 0 7) 4) (list 15 (list 0 13))) (list 1 1)))) (split xs nil) xs)

(defun reduce-problem (xs)
  (iter
    (bind (((:values l r exploded) (explode xs nil 0)))
      (for splitted = (when (not exploded) (split xs nil)))
      (while (or exploded splitted)))
    (finally (return xs))))
;; (let ((xs (list (list (list (list 0 7) 4) (list 15 (list 0 13))) (list 1 1)))) (reduce-problem xs) xs)

(defun part-1 ()
  (iter
    (with lines = (parse-problem))
    (with result = (reduce-problem (car lines)))
    (for line in (cdr lines))
    (reduce-problem line)
    (setf result (list result line))
    (reduce-problem result)
    (finally
     (return
       (->> (reduce-problem result)
         magnitute)))))

;; too high: 120074

(defun deep-copy (xs)
  (read-from-string (format nil "~a" xs)))

(defun part-2 ()
  (iter
    (with lines = (parse-problem))
    (for one-line in lines)
    (maximizing
     (iter
       (for other-line in lines)
       (when (not (eq one-line other-line))
         (maximizing
          (->> (list (deep-copy one-line) (deep-copy other-line))
            reduce-problem
            magnitute)))))))

;; 4846 too high
