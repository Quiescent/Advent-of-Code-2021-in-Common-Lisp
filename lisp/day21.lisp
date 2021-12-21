(defpackage :day21
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

(in-package :day21)

;; Player 1 starting position: 4
;; Player 2 starting position: 5

(defun part-1 ()
  (iter
    (with player-1 = 3)
    (with player-2 = 4)
    (with score-1 = 0)
    (with score-2 = 0)
    (with rolls = 0)
    (with die = 1)
    (while (and (< score-1 1000)
                (< score-2 1000)))
    (incf player-1 (+ die (+ die 1) (+ die 2)))
    (setf player-1 (mod player-1 10))
    (incf score-1 (1+ player-1))
    (format t "player-1: ~a~%" player-1)
    (format t "score-1: ~a~%" score-1)
    (incf rolls 3)

    (while (and (< score-1 1000)
                (< score-2 1000)))
    (incf player-2 (+ (+ die 3) (+ die 4) (+ die 5)))
    (setf player-2 (mod player-2 10))
    (incf score-2 (1+ player-2))
    (format t "player-2: ~a~%" player-2)
    (format t "score-2: ~a~%" score-2)
    (format t "~%")

    (incf rolls 3)
    (incf die 6)
    (finally
     (return
       (* (min score-1 score-2) (print rolls))))))

;; 711480 wrong

(defvar cache)

#+nil
(iter outer
  (for i from 1 to 3)
  (iter
    (for j from 1 to 3)
    (iter
      (for k from 1 to 3)
      (in outer (collecting (+ i j k))))))

(defvar outcomes '(3 4 5 4 5 6 5 6 7 4 5 6 5 6 7 6 7 8 5 6 7 6 7 8 7 8 9))

(defun roll (player-1 player-2 score-1 score-2)
  (or #1=(gethash (list player-1 player-2 score-1 score-2) cache)
      (setf #1#
            ;; Need to count per player
            (if (or (>= score-1 21) (>= score-2 21))
                (if (>= score-1 21) (cons 1 0) (cons 0 1))
                (iter
                  (with outcomes-1 = 0)
                  (with outcomes-2 = 0)
                  (for player-1-tot in outcomes)
                  (for new-player-1 = (mod (+ player-1 player-1-tot) 10))
                  (for new-score-1  = (+ score-1 (1+ new-player-1)))
                  (when (>= new-score-1 21)
                    (incf outcomes-1)
                    (next-iteration))
                  (iter
                    (for player-2-tot in outcomes)
                    (for new-player-2 = (mod (+ player-2 player-2-tot) 10))
                    (for new-score-2  = (+ score-2 (1+ new-player-2)))
                    (for (wins-1 . wins-2) = (roll new-player-1 new-player-2 new-score-1 new-score-2))
                    (incf outcomes-1 wins-1)
                    (incf outcomes-2 wins-2))
                  (finally (return (cons outcomes-1 outcomes-2))))))))

;; Added to debug the problem.  It turned out that I constructed the
;; wrong key for the table.
;;
;; It was:
;; (list player-1 player-2 score-1 score-1 rolls-1 rolls-2).
;;
;; Notice the second score-1... doh!
;;
;; Either way.  This solution is significantly faster than the
;; original ;)
(defun roll-prime (player-1 player-2 score-1 score-2 rolls-1 rolls-2)
  (or #1=(gethash (list player-1 player-2 score-1 score-2 rolls-1 rolls-2) cache)
      (setf #1#
            ;; Need to count per player
            (if (or (>= score-1 21) (>= score-2 21))
                (if (>= score-1 21) (cons 1 0) (cons 0 1))
                (cond
                  ((and (= rolls-1 4) (= rolls-2 3)) (roll-prime player-1
                                                                 player-2
                                                                 score-1
                                                                 (+ 1 player-2 score-2)
                                                                 0
                                                                 0))
                  ((= rolls-1 3) (roll-prime player-1
                                             player-2
                                             (+ 1 score-1 player-1)
                                             score-2
                                             4
                                             rolls-2))
                  ((< rolls-1 3)
                   (bind ((new-player-1-1 (mod (+ player-1 1) 10))
                          ((wins-1-1 . wins-2-1) (roll-prime new-player-1-1
                                                             player-2
                                                             score-1
                                                             score-2
                                                             (1+ rolls-1)
                                                             rolls-2))
                          (new-player-1-2 (mod (+ player-1 2) 10))
                          ((wins-1-2 . wins-2-2) (roll-prime new-player-1-2
                                                             player-2
                                                             score-1
                                                             score-2
                                                             (1+ rolls-1)
                                                             rolls-2))
                          (new-player-1-3 (mod (+ player-1 3) 10))
                          ((wins-1-3 . wins-2-3) (roll-prime new-player-1-3
                                                             player-2
                                                             score-1
                                                             score-2
                                                             (1+ rolls-1)
                                                             rolls-2)))
                     (cons (+ wins-1-1 wins-1-2 wins-1-3)
                           (+ wins-2-1 wins-2-2 wins-2-3))))
                  ((< rolls-2 3)
                   (bind ((new-player-2-1 (mod (+ player-2 1) 10))
                          ((wins-1-1 . wins-2-1) (roll-prime player-1
                                                             new-player-2-1
                                                             score-1
                                                             score-2
                                                             rolls-1
                                                             (1+ rolls-2)))
                          (new-player-2-2 (mod (+ player-2 2) 10))
                          ((wins-1-2 . wins-2-2) (roll-prime player-1
                                                             new-player-2-2
                                                             score-1
                                                             score-2
                                                             rolls-1
                                                             (1+ rolls-2)))
                          (new-player-2-3 (mod (+ player-2 3) 10))
                          ((wins-1-3 . wins-2-3) (roll-prime player-1
                                                             new-player-2-3
                                                             score-1
                                                             score-2
                                                             rolls-1
                                                             (1+ rolls-2))))
                     (cons (+ wins-1-1 wins-1-2 wins-1-3)
                           (+ wins-2-1 wins-2-2 wins-2-3)))))))))

(defun part-2 (&optional (a 3) (b 4))
  (bind ((cache (make-hash-table :test #'equal))
         ((wins-1 . wins-2) (roll-prime a b 0 0 0 0)))
    (max wins-1 wins-2)))

;; 147525965223926751 wrong!
