(defpackage :day10
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

(in-package :day10)

(defun reverse-bracket (bracket)
  (ecase bracket
    (#\( #\))
    (#\[ #\])
    (#\{ #\})
    (#\< #\>)
    (#\) #\()
    (#\] #\[)
    (#\} #\{)
    (#\> #\<)))

(defun part-1 ()
  (iter
    (for line in (read-problem "day10.in"))
    (summing
     (iter
       (with stack = (list))
       (for char in-string line)
       (for expecting = (and stack (reverse-bracket (car stack))))
       (case char
         (#\( (push char stack))
         (#\[ (push char stack))
         (#\{ (push char stack))
         (#\< (push char stack))
         (#\) (if (not (char= expecting #\)))
                  (return 3)
                  (pop stack)))
         (#\] (if (not (char= expecting #\]))
                  (return 57)
                  (pop stack)))
         (#\} (if (not (char= expecting #\}))
                  (return 1197)
                  (pop stack)))
         (#\> (if (not (char= expecting #\>))
                  (return 25137)
                  (pop stack))))
       (finally (return 0))))))


(defun part-2 ()
  (iter
    (for line in (read-problem "day10.in"))
    (collecting
     (iter
       (with stack = (list))
       (for char in-string line)
       (for expecting = (and stack (reverse-bracket (car stack))))
       (case char
         (#\( (push char stack))
         (#\[ (push char stack))
         (#\{ (push char stack))
         (#\< (push char stack))
         (#\) (if (not (char= expecting #\)))
                  (return nil)
                  (pop stack)))
         (#\] (if (not (char= expecting #\]))
                  (return nil)
                  (pop stack)))
         (#\} (if (not (char= expecting #\}))
                  (return nil)
                  (pop stack)))
         (#\> (if (not (char= expecting #\>))
                  (return nil)
                  (pop stack))))
       (finally
        (return
          (iter
            (for char in stack)
            (with score = 0)
            (setf score (* score 5))
            (case char
              (#\( (incf score 1))
              (#\[ (incf score 2))
              (#\{ (incf score 3))
              (#\< (incf score 4)))
            (finally (return score))))))
     :into scores)
    (finally
     (return
       (let ((sorted (sort (remove nil scores) #'<)))
         (nth (floor (length sorted) 2) sorted))))))
