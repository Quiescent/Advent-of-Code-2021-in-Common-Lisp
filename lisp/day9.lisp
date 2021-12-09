(defpackage :day9
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
  (:shadowing-import-from :arrow-macros :<>)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day9)

(defun part-1 ()
  (iter
    (with map = (->> (read-problem "day9.in")
                  (map 'vector (lambda (line)
                                 (->> (split "" line)
                                   (map 'vector #'read-from-string))))))
    (for i from 0 below (length map))
    (summing
     (iter
       (for j from 0 below (length (aref map 0)))
       (for point = (aref (aref map i) j))
       (for found = (-> (concatenate 'list
                                     (if (> i 0)                     (list (aref (aref map (1- i)) j)) nil)
                                     (if (> j 0)                     (list (aref (aref map i) (1- j))) nil)
                                     (if (< i (1- (length map)))          (list (aref (aref map (1+ i)) j)) nil)
                                     (if (< j (1- (length (aref map 0)))) (list (aref (aref map i) (1+ j))) nil))
                      (sort #'<)))
       (when (< point (car found))
         (summing (1+ point)))))))

(defun low-points ()
  (iter
    (with map = (->> (read-problem "day9.in")
                  (map 'vector (lambda (line)
                                 (->> (split "" line)
                                   (map 'vector #'read-from-string))))))
    (for i from 0 below (length map))
    (appending
     (iter
       (for j from 0 below (length (aref map 0)))
       (for point = (aref (aref map i) j))
       (for found = (-> (concatenate 'list
                                     (if (> i 0)                          (list (aref (aref map (1- i)) j)) nil)
                                     (if (> j 0)                          (list (aref (aref map i) (1- j))) nil)
                                     (if (< i (1- (length map)))          (list (aref (aref map (1+ i)) j)) nil)
                                     (if (< j (1- (length (aref map 0)))) (list (aref (aref map i) (1+ j))) nil))
                      (sort #'<)))
       (when (< point (car found))
         (collecting (cons i j)))))))

(defun debug-map (membership-map)
  (iter
    (for i from 0 below 5)
    (iter
      (for j from 0 below 10)
      (for point = (gethash (cons i j) membership-map))
      (if point (format t "#") (format t "_")))
    (format t "~%")))

(defun part-2 ()
  (iter
    (with membership = (make-hash-table :test #'equal))
    (with map = (->> (read-problem "day9.in")
                  (map 'vector (lambda (line)
                                 (->> (split "" line)
                                   (map 'vector #'read-from-string))))))
    (with count = 0)
    (initially
     (iter
       (for point in (low-points))
       (for label from 0)
       (setf (gethash point membership) label)))
    (while (< count (* (length map) (length (aref map 0)))))
    (for p-count = count)
    (iter
     (for i from 0 below (length map))
     (iter
       (for j from 0 below (length (aref map 0)))
       (for point = (aref (aref map i) j))
       (when (= 9 point)
         (next-iteration))
       (when (null (gethash (cons i j) membership))
         (let ((up-label    (gethash (cons (1- i) j) membership))
               (down-label  (gethash (cons (1+ i) j) membership))
               (left-label  (gethash (cons i (1- j)) membership))
               (right-label (gethash (cons i (1+ j)) membership))
               (up          (if (> i 0)                          (aref (aref map (1- i)) j) nil))
               (left        (if (> j 0)                          (aref (aref map i) (1- j)) nil))
               (down        (if (< i (1- (length map)))          (aref (aref map (1+ i)) j) nil))
               (right       (if (< j (1- (length (aref map 0)))) (aref (aref map i) (1+ j)) nil)))
           (cond
             ((and up-label
                   (<= up (car (sort (remove nil (list left down right)) #'<)))
                   (< up point))
              (progn
                (incf count)
                (setf (gethash (cons i j) membership) up-label)))
             ((and left-label
                   (<= left (car (sort (remove nil (list up down right)) #'<)))
                   (< left point))
              (progn
                (incf count)
                (setf (gethash (cons i j) membership) left-label)))
             ((and down-label
                   (<= down (car (sort (remove nil (list left up right)) #'<)))
                   (< down point))
              (progn
                (incf count)
                (setf (gethash (cons i j) membership) down-label)))
             ((and right-label
                   (<= right (car (sort (remove nil (list left down up)) #'<)))
                   (< right point))
              (progn
                (incf count)
                (setf (gethash (cons i j) membership) right-label))))))))
    (when (= p-count count)
      (return
        (iter
          (with counts = (make-hash-table :test #'eq))
          (for (key value) in-hashtable membership)
          (incf (gethash value counts 0))
          (finally
           (return
             (iter
               (for (key value) in-hashtable counts)
               (collecting value into result)
               (finally
                (return
                  (-<> (sort result #'>)
                    (subseq <> 0 3)
                    (apply #'* <>))))))))))))
