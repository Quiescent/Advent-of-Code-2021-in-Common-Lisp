(defpackage :day23
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

(in-package :day23)

(defvar coord-pool)

(defun fill-coord-pool (max-x max-y)
  (iter
    (for y from 0 to max-y)
    (push (cons y (iter
                    (for x from 0 to max-x)
                    (collecting (cons x (cons x y)))))
          coord-pool)))

(defun coord (x y)
  (->> (assoc y coord-pool)
    cdr
    (assoc x)
    cdr))

;; Amphipod is (Symbol Bool Cons)
(defun parse-problem (&optional (filename "day23.in"))
  (let ((map       (make-hash-table :test #'eq))
        (amphipods (list))
        (lines     (read-problem filename)))
    (iter
      (for y from 0)
      (for line in lines)
      (iter
        (for x from 0 below (length line))
        (case (aref line x)
          (#\# (setf (gethash (coord x y) map) 'wall))
          (#\. (setf (gethash (coord x y) map) 'space))
          (#\D (progn
                 (push (list 'D nil (coord x y)) amphipods)
                 (setf (gethash (coord x y) map)  'space)))
          (#\C (progn
                 (push (list 'C nil (coord x y)) amphipods)
                 (setf (gethash (coord x y) map)  'space)))
          (#\B (progn
                 (push (list 'B nil (coord x y)) amphipods)
                 (setf (gethash (coord x y) map)  'space)))
          (#\A (progn
                 (push (list 'A nil (coord x y)) amphipods)
                 (setf (gethash (coord x y) map)  'space)))))
      (finally (return (list map amphipods))))))

;; Amber amphipods require 1 energy per step, Bronze amphipods require
;; 10 energy, Copper amphipods require 100, and Desert ones require
;; 1000

(defun energy (type)
  (case type
    (a 1)
    (b 10)
    (c 100)
    (d 1000)))

;; never stop on the space immediately outside any room
;;
;; never move from the hallway into a room unless that room is their
;; destination room and that room contains no amphipods which do not
;; also have that room as their own destination
;;
;; Once an amphipod stops moving in the hallway, it will stay in that
;; spot until it can move into a room

(defun amphipod-is-done (amphipod amphipods depth)
  (bind (((type active (x . y)) amphipod)
         (above-others          (< 1 y depth)))
    (declare (ignorable active))
    (and (or (not above-others)
             (iter
               (for yi from y to depth)
               (for amphipod-below = (get-amphipod-at amphipods (coord x yi)))
               (always (amphipods-are-same-type amphipod amphipod-below))))
         (case type
           (A (and (= x 3) (>= y 2)))
           (B (and (= x 5) (>= y 2)))
           (C (and (= x 7) (>= y 2)))
           (D (and (= x 9) (>= y 2)))))))

(defun done (amphipods depth)
  (iter
    (for amphipod in amphipods)
    (always (amphipod-is-done amphipod amphipods depth))))

(defun get-amphipod-at (amphipods coord)
  (find coord amphipods :key #'caddr))

(defun destination-column (amphipod)
  (bind (((type . rest) amphipod))
    (declare (ignore rest))
    (case type
      (A 3)
      (B 5)
      (C 7)
      (D 9))))

(defun amphipods-are-same-type (one other)
  (eq (car one)
      (car other)))

(defun path-between-coords-is-clear (start-coord destination-coord map amphipods)
  (iter
    (with to-explore = (make-queue :simple-queue))
    (with seen       = (make-hash-table :test #'eq))
    (with from       = (make-hash-table :test #'eq))
    (initially
     (iter
       (for coord in (empty-squares-around start-coord amphipods map))
       (qpush to-explore coord)
       (setf (gethash coord from) start-coord)
       (setf (gethash coord seen) t)))
    (while (not (= 0 (qsize to-explore))))
    (for next-coord = (qpop to-explore))
    (when (eq next-coord destination-coord)
      (return
        (iter
          (with current-coord = destination-coord)
          (while (not (eq current-coord start-coord)))
          (setf current-coord (gethash current-coord from))
          (counting t))))
    (iter
      (for coord in (empty-squares-around next-coord amphipods map))
      (when (not #1=(gethash coord seen))
        (qpush to-explore coord)
        (setf #1# t)
        (setf (gethash coord from) next-coord)))))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem)))
    (path-to-room-is-clear (coord 1 1) 3 map (list (list 'A nil (coord 3 2))))))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem)))
    (path-to-room-is-clear (coord 1 1) 3 map (list (list 'A nil (coord 5 2))))))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem)))
    (path-to-room-is-clear (coord 1 1) 3 map (list (list 'A nil (coord 2 1))))))

(defun room-contains-correct-amphipods (amphipod amphipods depth)
  (bind ((destination-column (destination-column amphipod)))
    (iter
      (for yi from 2 to depth)
      (for occupant = (get-amphipod-at amphipods (coord destination-column yi)))
      (always (or (null occupant)
                  (amphipods-are-same-type amphipod occupant))))))

(defun path-to-destination-is-clear (amphipod map amphipods depth)
  (bind (((type active coord)    amphipod)
         (room-can-take-amphipod (room-contains-correct-amphipods amphipod amphipods depth))
         (has-clear-path         (path-between-coords-is-clear coord
                                                               (destination-coord amphipod amphipods depth)
                                                               map
                                                               amphipods)))
    (declare (ignore type active))
    (and room-can-take-amphipod has-clear-path)))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem)))
    (path-to-destination-is-clear (list 'A nil (coord 2 1)) map (list (list 'A nil (coord 2 1))) 3)))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem)))
    (path-to-destination-is-clear (list 'A nil (coord 2 1)) map (list (list 'A nil (coord 2 1))
                                                                      (list 'B nil (coord 3 3))) 3)))

;; Assume that the room is a valid destination
(defun destination-coord (amphipod amphipods depth)
  (bind (((type active coord) amphipod)
         (destination-column  (destination-column amphipod)))
    (declare (ignore type active coord))
    (coord destination-column
           (or (iter
                 (for yi from 2 to depth)
                 (finding yi
                          :such-that (get-amphipod-at amphipods
                                                      (coord (destination-column amphipod)
                                                             (1+ yi)))))
               depth))))

;; State is (Number . (Amphipod))

(defvar squares-around-cache)

(defun cached-spaces (coord map)
  (or #1=(gethash coord squares-around-cache)
      (setf #1#
            (bind (((x . y) coord))
              (iter
                (for coord in (list (coord (1- x) y)
                                    (coord (1+ x) y)
                                    (coord x (1- y))
                                    (coord x (1+ y))))
                (when (and (eq 'space (gethash coord map)))
                  (collecting coord)))))))

(defun empty-squares-around (coord amphipods map)
  (iter
    (for coord-around in (cached-spaces coord map))
    (when (null (find coord-around amphipods :key #'caddr))
      (collecting coord-around))))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem)))
    (empty-squares-around (coord 3 2) amphipods map)))

(defun swap-amphipod-at (amphipods coord new-amphipod)
  (cond
    ((null amphipods) nil)
    ((eq (caddar amphipods) coord)
     (cons new-amphipod
           (cdr amphipods)))
    (t (cons (car amphipods)
             (swap-amphipod-at (cdr amphipods) coord new-amphipod)))))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem)))
    (format t "~a~%" amphipods)
    (swap-amphipod-at amphipods (coord 3 2) (list 'boom t (coord 3 2)))))

(defun manhattan-distance (coord-1 coord-2)
  (bind (((x1 . y1) coord-1)
         ((x2 . y2) coord-2))
    (+ (abs (- x2 x1))
       (abs (- y2 y1)))))

(defvar corridor-coordinates)

(defun states-from (state map depth)
  (bind ((amphipods (caddr state))
         (energy    (cadr state))
         (correct   (car state)))
    (append
     ;; Pick any amphipod not in it's destination and not in the
     ;; corridor and create a state from each position it could be in
     ;; the corridor (other than directly above a room)
     (iter outer
       (for amphipod in amphipods)
       (for (type active coord) = amphipod)
       (when (and (not (= (cdr coord) 1))
                  (not (amphipod-is-done amphipod amphipods depth)))
         (iter
           (for dest-coord in corridor-coordinates)
           (awhen (path-between-coords-is-clear coord dest-coord map amphipods)
             (in outer
                 (collecting (list correct
                                   (+ energy (* (energy type) it))
                                   (swap-amphipod-at amphipods
                                                     coord
                                                     (list type active dest-coord)))))))))

     ;; Try to move any amphipod that's in the corridor to its final
     ;; position
     (iter outer
       (for amphipod in amphipods)
       (for (type active coord) = amphipod)
       (awhen (and (not (amphipod-is-done amphipod amphipods depth))
                   (path-to-destination-is-clear amphipod map amphipods depth))
         (collecting
          (list (1+ correct)
                (+ energy (* (energy type) it))
                (swap-amphipod-at amphipods
                                  coord
                                  (list type active (destination-coord amphipod amphipods depth))))))))))

#+nil
(let ((coord-pool (list))
      (squares-around-cache (make-hash-table :test #'eq)))
  (fill-coord-pool 14 14)
  (bind (((map . rest) (parse-problem))
         (amphipods `((A NIL ,(coord 9 3)) (C NIL ,(coord 7 3)) (D NIL ,(coord 5 3))
                      (A NIL ,(coord 3 3)) (D NIL ,(coord 9 2)) (B NIL ,(coord 3 2))
                      (C NIL ,(coord 5 2)) (B NIL ,(coord 4 1)))))
    (path-to-destination-is-clear (list 'c nil (coord 5 2)) map amphipods depth)))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem))
         (state (cons 0 amphipods)))
    (states-from state map)))

(defun amphipod-x (amphipod)
  (caaddr amphipod))

(defun amphipod-y (amphipod)
  (cdaddr amphipod))

(defun find-cheapest-cost (amphipods map depth)
  (iter
    (with to-explore = (make-instance 'cl-heap:priority-queue))
    (with best       = most-positive-fixnum)
    (initially
     (let ((correct
             (iter
               (for amphipod in amphipods)
               (counting (amphipod-is-done amphipod amphipods depth)))))
       (cl-heap:enqueue to-explore (list (print correct) 0 amphipods) 0)))
    (for current-state = (cl-heap:dequeue to-explore))
    (when (null current-state)
      (return best))
    (when (> (cadr current-state) best)
      (next-iteration))
    (for i from 0 below 10000)
    (format t "correctly-ordered: ~a~%" (car current-state))
    (when (and (= (car current-state) (length amphipods)))
      (format t "(car current-state): ~a~%" (car current-state))
      (format t "current-state: ~a~%" current-state)
      (setf best (min best (cadr current-state)))
      (format t "best: ~a~%" best))
    (iter
      (for next-state in (states-from current-state map depth))
      (cl-heap:enqueue to-explore next-state (- 8 (car next-state));(cadr next-state) ;(- 8 (car next-state))
                       ))))

(defun part-1 ()
  (let ((coord-pool           (list))
        (squares-around-cache (make-hash-table :test #'eq)))
    (fill-coord-pool 14 14)
    (bind (((map amphipods) (parse-problem))
           (corridor-coordinates (list (coord 1 1)
                                       (coord 2 1)
                                       (coord 4 1)
                                       (coord 6 1)
                                       (coord 8 1)
                                       (coord 10 1)
                                       (coord 11 1))))
      (find-cheapest-cost amphipods map 3))))

;; 23356 too high!
;; 19160 correct!

(defun part-2 ()
  (let ((coord-pool           (list))
        (squares-around-cache (make-hash-table :test #'eq)))
    (fill-coord-pool 14 14)
    (bind (((map amphipods) (parse-problem "day23-part-2.in"))
           (corridor-coordinates (list (coord 1 1)
                                       (coord 2 1)
                                       (coord 4 1)
                                       (coord 6 1)
                                       (coord 8 1)
                                       (coord 10 1)
                                       (coord 11 1))))
      (find-cheapest-cost amphipods map 5))))
