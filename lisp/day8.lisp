(defpackage :day8
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

(in-package :day8)

(defun part-1 ()
  (iter
    (for line in (read-problem "day8.in"))
    (for (input output) = (->> (split (create-scanner '(:sequence
                                                    (:char-class #\Space)
                                                    (:char-class #\|)
                                                    (:char-class #\Space)))
                                      line)
                            (mapcar (lambda (part) (split " " part)))))
    (summing
     (iter
       (for digit in output)
       (counting (or (= 2 (length digit))
                     (= 4 (length digit))
                     (= 3 (length digit))
                     (= 7 (length digit))))))))

(defun decode (input output)
  (iter
    (with config = (make-hash-table :test #'eq))
    (initially
     (iter
       (with chars = '(#\a #\b #\c #\d #\e #\f #\g))
       (for char in chars)
       (setf (gethash char config) chars)))
    (for word in (concatenate 'list input output))
    (for letters = (map 'list #'identity word))
    (case (length word)
      (5 (progn
           (setf (gethash #\a config) (intersection letters (gethash #\a config)))
           (setf (gethash #\d config) (intersection letters (gethash #\d config)))
           (setf (gethash #\g config) (intersection letters (gethash #\g config)))))
      (6 (progn
           (setf (gethash #\a config) (intersection letters (gethash #\a config)))
           (setf (gethash #\b config) (intersection letters (gethash #\b config)))
           (setf (gethash #\f config) (intersection letters (gethash #\f config)))
           (setf (gethash #\g config) (intersection letters (gethash #\g config)))))
      (2 (progn
           (setf (gethash #\c config) (intersection letters (gethash #\c config)))
           (setf (gethash #\f config) (intersection letters (gethash #\f config)))))
      (4 (progn
           (setf (gethash #\b config) (intersection letters (gethash #\b config)))
           (setf (gethash #\c config) (intersection letters (gethash #\c config)))
           (setf (gethash #\d config) (intersection letters (gethash #\d config)))
           (setf (gethash #\f config) (intersection letters (gethash #\f config)))))
      (3 (progn
           (setf (gethash #\a config) (intersection letters (gethash #\a config)))
           (setf (gethash #\c config) (intersection letters (gethash #\c config)))
           (setf (gethash #\f config) (intersection letters (gethash #\f config)))))
      (7 (progn
           (setf (gethash #\a config) (intersection letters (gethash #\a config)))
           (setf (gethash #\b config) (intersection letters (gethash #\b config)))
           (setf (gethash #\c config) (intersection letters (gethash #\c config)))
           (setf (gethash #\d config) (intersection letters (gethash #\d config)))
           (setf (gethash #\e config) (intersection letters (gethash #\e config)))
           (setf (gethash #\f config) (intersection letters (gethash #\f config)))
           (setf (gethash #\g config) (intersection letters (gethash #\g config))))))
    (finally
     (progn
       (iter
         (with done = nil)
         (while (not done))
         (for found = nil)
         (for i from 0 below 1000)
         (iter
           (for (key value) in-hashtable config)
           (when (and (listp value) (= (length value) 1))
             (setf found t)
             (setf (gethash key config) (car value))
             (iter
               (for char in '(#\a #\b #\c #\d #\e #\f #\g))
               (when (listp (gethash char config))
                 (setf (gethash char config) (remove (car value) (gethash char config)))))))
         (setf done (not found)))
       (let ((rev-config (make-hash-table :test #'eq)))
         (iter
           (for (key value) in-hashtable config)
           (setf (gethash value rev-config) key))
         (return
           (iter
             (with digits = (list (cons "abcefg" 0)
                                  (cons "cf" 1)
                                  (cons "acdeg" 2)
                                  (cons "acdfg" 3)
                                  (cons "bcdf" 4)
                                  (cons "abdfg" 5)
                                  (cons "abdefg" 6)
                                  (cons "acf" 7)
                                  (cons "abcdefg" 8)
                                  (cons "abcdfg" 9)))
             (for word in output)
             (for i from (1- (length output)) downto 0)
             (summing
              (-<> (iter
                     (for char in-string word)
                     (collecting (gethash char rev-config) :result-type string))
                (sort <> #'char<)
                (assoc <> digits :test #'string-equal)
                cdr
                (* (expt 10 i)))))))))))

(defun part-2 ()
  (iter
    (for line in (read-problem "day8.in"))
    (for (input output) = (->> (split (create-scanner '(:sequence
                                                    (:char-class #\Space)
                                                    (:char-class #\|)
                                                    (:char-class #\Space)))
                                      line)
                            (mapcar (lambda (part) (split " " part)))))
    (summing (decode input output))))

;; 820 too low
;; 10233231 too high
