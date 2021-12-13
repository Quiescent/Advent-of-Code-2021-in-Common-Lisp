(defpackage :day13
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

(in-package :day13)

(defun part-1 ()
  (let ((parsed-dots (make-hash-table :test #'equal))
        (parsed-folds (list)))
    (iter (with (dots folds) = (->> (read-whole-file "day13.in")
                                 (split (create-scanner '(:sequence
                                                          (:char-class #\newline)
                                                          (:char-class #\newline))))
                                 (mapcar (lambda (group) (split (create-scanner '(:char-class #\newline))
                                                                group)))))
      (for dot in dots)
      (match dot
        ((ppcre "([0-9]+),([0-9]+)"
                (read x) (read y))
         (setf (gethash (cons x y) parsed-dots) t)))
      (finally
       (iter
         (for fold in folds)
         (match fold
           ((ppcre "fold along x=([0-9]+)"
                   (read column))
            (push (cons 'x column) parsed-folds))
           ((ppcre "fold along y=([0-9]+)"
                   (read row))
            (push (cons 'y row) parsed-folds)))
         (finally
          (setf parsed-folds (list (car (reverse parsed-folds))))))))
    (iter
      (for (coord value) in-hashtable parsed-dots)
      (for (x . y) = coord)
      (when (> x 655)
        (remhash coord parsed-dots)
        (setf (gethash (cons (- 655 (- x 655)) y) parsed-dots) t))
      (finally
       (return
         (iter
           (for (key value) in-hashtable parsed-dots)
           (counting t)))))))

(defun part-2 ()
  (let ((parsed-dots (make-hash-table :test #'equal))
        (parsed-folds (list)))
    (iter (with (dots folds) = (->> (read-whole-file "day13.in")
                                 (split (create-scanner '(:sequence
                                                          (:char-class #\newline)
                                                          (:char-class #\newline))))
                                 (mapcar (lambda (group) (split (create-scanner '(:char-class #\newline))
                                                                group)))))
      (for dot in dots)
      (match dot
        ((ppcre "([0-9]+),([0-9]+)"
                (read x) (read y))
         (setf (gethash (cons x y) parsed-dots) t)))
      (finally
       (iter
         (for fold in folds)
         (match fold
           ((ppcre "fold along x=([0-9]+)"
                   (read column))
            (push (cons 'x column) parsed-folds))
           ((ppcre "fold along y=([0-9]+)"
                   (read row))
            (push (cons 'y row) parsed-folds)))
         (finally
          (setf parsed-folds (reverse parsed-folds))))))
    (iter
      (for (direction . column) in parsed-folds)
      (iter
        (for (coord value) in-hashtable parsed-dots)
        (for (x . y) = coord)
        (case direction
          (x (when (> x column)
               (remhash coord parsed-dots)
               (setf (gethash (cons (- column (- x column)) y) parsed-dots) t)))
          (y (when (> y column)
               (remhash coord parsed-dots)
               (setf (gethash (cons x (- column (- y column))) parsed-dots) t)))))
      (finally
         (return
           (iter
             (with grid = (iter
                            (for i from 0 below 10)
                            (collecting (make-string 100 :initial-element #\Space) :result-type vector)))
             (for (key value) in-hashtable parsed-dots)
             (for (x . y) = key)
             (setf (-> (aref grid y) (aref x)) #\#)
             (finally
              (iter
                (for line in-vector grid)
                (format t "~a~%" line)))))))))
