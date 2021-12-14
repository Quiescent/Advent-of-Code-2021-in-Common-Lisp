(defpackage :day14
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

(in-package :day14)

(defun parse-problem ()
  (let* ((lines      (read-problem "day14.in"))
         (start      (coerce (car lines) 'list))
         (insertions (->> (cddr lines)
                       (mapcar (lambda (line) (match line
                                                ((ppcre "([A-Z]+) -> ([A-Z])"
                                                        pattern insert)
                                                 (cons pattern (aref insert 0)))))))))
    (cons start (iter
                  (with mapping = (make-hash-table :test #'equal))
                  (for (pattern . char) in insertions)
                  (setf (gethash pattern mapping) char)
                  (finally (return mapping))))))

(defun part-1 ()
  (iter
    (with (polymer . insertions) = (parse-problem))
    (for i from 0 below 10)
    (iter
      (for head on polymer)
      (for p-head previous head)
      (awhen (and p-head
                  (gethash (coerce (list (car p-head) (car head)) 'string) insertions))
        (setf (cdr p-head)
              (cons it head))))
    (finally
     (return
       (iter
         (for char in (remove-duplicates polymer))
         (minimizing (count-if (lambda (other-char) (char= char other-char)) polymer) :into minimum)
         (maximizing (count-if (lambda (other-char) (char= char other-char)) polymer) :into maximum)
         (finally (return (- maximum minimum))))))))

(defun parse-problem-2 ()
  (let* ((lines      (read-problem "day14.in"))
         (start      (coerce (car lines) 'list))
         (insertions (->> (cddr lines)
                       (mapcar (lambda (line) (match line
                                                ((ppcre "([A-Z]+) -> ([A-Z])"
                                                        pattern insert)
                                                 (cons (coerce pattern 'list) (aref insert 0)))))))))
    (cons start (iter
                  (with mapping = (make-hash-table :test #'eq))
                  (for (pattern . char) in insertions)
                  (for sub-map = (gethash (car pattern) mapping))
                  (when (null sub-map)
                    (setf (gethash (car pattern) mapping)
                          (make-hash-table :test #'eq)))
                  (setf (gethash (cadr pattern)
                                 (gethash (car pattern) mapping))
                        char)
                  (finally (return mapping))))))

(defun part-2 ()
  (iter
    (with (polymer . insertions) = (parse-problem-2))
    (for i from 0 below 15)
    (format t "i: ~a~%" i)
    (iter
      (for head on polymer)
      (for p-head previous head)
      (awhen (and p-head
                  (gethash (car p-head) insertions)
                  (gethash (car head) (gethash (car p-head) insertions)))
        (setf (cdr p-head)
              (cons it head))))
    (for result = (iter
                    (for char in (remove-duplicates polymer))
                    (minimizing (count-if (lambda (other-char) (char= char other-char)) polymer) :into minimum)
                    (finding char minimizing minimum into min-char)
                    (maximizing (count-if (lambda (other-char) (char= char other-char)) polymer) :into maximum)
                    (finding char maximizing maximum into max-char)
                    (finally (progn
                               (format t "min-char: ~a~%" min-char)
                               (format t "minimum: ~a~%" minimum)
                               (format t "max-char: ~a~%" max-char)
                               (format t "maximum: ~a~%" maximum)
                               (return (- maximum minimum))))))
    (for p-result previous result)
    (format t "result: ~a~%" result)
    (when p-result
      (format t "(- result p-result): ~a~%" (- result p-result)))
    (finally
     (return
       (iter
         (for char in (remove-duplicates polymer))
         (minimizing (count-if (lambda (other-char) (char= char other-char)) polymer) :into minimum)
         (maximizing (count-if (lambda (other-char) (char= char other-char)) polymer) :into maximum)
         (finally (return (- maximum minimum))))))))

(defvar counts)

(defun dfs (depth a b insertions)
  (when (/= depth 30)
    (awhen (and (gethash b insertions)
                (gethash b (gethash a insertions)))
      (incf (gethash it counts 0))
      (dfs (1+ depth)
           a
           it
           insertions)
      (dfs (1+ depth)
           it
           b
           insertions))))

(defun part-2-alt ()
  (bind (((polymer . insertions) (parse-problem-2))
         (counts                (make-hash-table :test #'eq)))
    (format t "(length polymer): ~a~%" (length polymer))
    (iter
      (for i from 0)
      (format t "i: ~a~%" i)
      (for char in polymer)
      (incf (gethash char counts 0))
      (for p-char previous char)
      (when p-char
        (dfs 0 p-char char insertions)))
    (iter
      (for (key value) in-hashtable counts)
      (minimizing value into minimum)
      (finding key minimizing minimum into min-char)
      (maximizing value into maximum)
      (finding key maximizing maximum into max-char)
      (finally (progn
                 (format t "min-char: ~a~%" min-char)
                 (format t "max-char: ~a~%" max-char)
                 (return (- maximum minimum)))))))

(defvar cache)

(defun dfs-cached (depth a b insertions)
  (when (/= depth 40)
    (awhen (and (gethash b insertions)
                (gethash b (gethash a insertions)))
      (incf (gethash it counts 0))
      (let ((left  #1=(gethash (list depth a  it) cache))
            (right #2=(gethash (list depth it b)  cache)))
        (when (null left)
          (let ((counts (make-hash-table :test #'eq)))
            (dfs-cached (1+ depth)
                        a
                        it
                        insertions)
            (setf (gethash (list depth a it) cache) counts)))
        (when (null right)
          (let ((counts (make-hash-table :test #'eq)))
            (dfs-cached (1+ depth)
                        it
                        b
                        insertions)
            (setf (gethash (list depth it b) cache) counts)))
        (iter
          (for (key value) in-hashtable #1#)
          (incf (gethash key counts 0) value))
        (iter
          (for (key value) in-hashtable #2#)
          (incf (gethash key counts 0) value))))))

(defun part-2-cached ()
  (bind (((polymer . insertions) (parse-problem-2))
         (counts                 (make-hash-table :test #'eq))
         (cache                  (make-hash-table :test #'equal)))
    (iter
      (for i from 0)
      (format t "i: ~a~%" i)
      (for char in polymer)
      (incf (gethash char counts 0))
      (for p-char previous char)
      (when p-char
        (dfs-cached 0 p-char char insertions)))
    (iter
      (for (key value) in-hashtable counts)
      (minimizing value into minimum)
      (finding key minimizing minimum into min-char)
      (maximizing value into maximum)
      (finding key maximizing maximum into max-char)
      (finally (progn
                 (format t "min-char: ~a~%" min-char)
                 (format t "max-char: ~a~%" max-char)
                 (return (- maximum minimum)))))))
