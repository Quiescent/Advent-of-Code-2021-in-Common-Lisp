(defpackage :day4
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
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day4)

(defun mark-number (number board)
  (iter
    (for row in-vector board)
    (for y from 0)
    (iter
      (for n in-vector row)
      (for x from 0)
      (when (eq n number)
        (setf (aref (aref board y) x) nil)
        (return)))))

(defun score (board)
  (->> (map 'list (lambda (row) (->> (map 'list #'identity row) (remove nil) (apply #'+))) board)
    (apply #'+)))

(defun completed (board)
  (or
   (iter
     (for y from 0 below (length board))
     (thereis
      (iter
        (for x from 0 below (length (aref board 0)))
        (always (eq nil (aref (aref board y) x))))))
   (iter
     (for x from 0 below (length (aref board 0)))
     (thereis
      (iter
        (for y from 0 below (length board))
        (always (eq nil (aref (aref board y) x))))))))

(defun part-1 ()
  (bind (((numbers . boards) (->> (read-whole-file "day4.in")
                               (split (create-scanner '(:sequence
                                                        (:char-class #\newline)
                                                        (:char-class #\newline))))))
         (ns (->> (split "," numbers)
               (mapcar #'read-from-string)))
         (bs (mapcar (lambda (board) (->> (split (create-scanner '(:char-class #\newline)) board)
                                  (map 'vector (lambda (row) (->> (split " " row)
                                                          (lambda (row) (remove "" row :test #'equal))
                                                          (map 'vector #'read-from-string))))))
                     boards)))
    (iter outer
      (for number in ns)
      (iter
        (for board in bs)
        (mark-number number board)
        (when (completed board)
          (return-from outer (* number (score board))))))))

(defun part-2 ()
  (bind (((numbers . boards) (->> (read-whole-file "day4.in")
                               (split (create-scanner '(:sequence
                                                        (:char-class #\newline)
                                                        (:char-class #\newline))))))
         (ns (->> (split "," numbers)
               (mapcar #'read-from-string)))
         (bs (mapcar (lambda (board) (->> (split (create-scanner '(:char-class #\newline)) board)
                                  (map 'vector (lambda (row) (->> (split " " row)
                                                          (lambda (row) (remove "" row :test #'equal))
                                                          (map 'vector #'read-from-string))))))
                     boards)))
    (iter outer
      (for number in ns)
      (iter
        (for board in bs)
        (mark-number number board)
        (when (completed board)
          (if (= (length bs) 1)
              (return-from outer (* number (score board)))
              (setf bs (delete board bs))))))))
