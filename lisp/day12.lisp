(defpackage :day12
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

(in-package :day12)

(defun is-small-cave (str)
  (every (lambda (char) (char= (char-downcase char) char)) str))

(defun part-1 ()
  (iter
    (with lines = (read-problem "day12.in"))
    (with map = (make-hash-table :test #'equal))
    (initially
     (iter
       (for line in lines)
       (match line
         ((ppcre "([a-zA-Z]+)-([a-zA-Z]+)"
                 start end)
          (progn
            (push end (gethash start map))
            (push start (gethash end map)))))))
    (with count = 0)
    (with to-process = (list (list "start" (list "start"))))
    (while to-process)
    (for (current-node visited) = (pop to-process))
    (for neighbours = (remove (car visited)
                              (gethash current-node map)
                              :test #'string-equal))
    (iter
      (for neighbour in neighbours)
      (if (string-equal neighbour "end")
          (incf count)
          (when (not (and (is-small-cave neighbour)
                          (member neighbour visited :test #'string-equal)))
            (push (list neighbour (cons neighbour visited))
                  to-process))))
    (finally (return count))))

(defun part-2 ()
  (iter
    (with lines = (read-problem "day12.in"))
    (with map = (make-hash-table :test #'equal))
    (initially
     (iter
       (for line in lines)
       (match line
         ((ppcre "([a-zA-Z]+)-([a-zA-Z]+)"
                 start end)
          (progn
            (push end (gethash start map))
            (push start (gethash end map)))))))
    (with count = 0)
    (with to-process = (list (list "start" (list "start") nil)))
    (while to-process)
    (for (current-node visited twice) = (pop to-process))
    (for neighbours = (remove (car visited)
                              (gethash current-node map)
                              :test #'string-equal))
    (iter
      (for neighbour in neighbours)
      (when (string-equal neighbour "start")
        (next-iteration))
      (if (string-equal neighbour "end")
          (progn
            ;; (format t "visited: ~a~%" (reverse (cons neighbour visited)))
            (incf count))
          (if (and (null twice)
                   (is-small-cave neighbour)
                   (member neighbour visited :test #'string-equal))
              (push (list neighbour (cons neighbour visited) t)
                    to-process)
              (when (not (and (is-small-cave neighbour)
                              (member neighbour visited :test #'string-equal)))
                (push (list neighbour (cons neighbour visited) twice)
                      to-process)))))
    (finally (return count))))

;; 41600
;; 41599
