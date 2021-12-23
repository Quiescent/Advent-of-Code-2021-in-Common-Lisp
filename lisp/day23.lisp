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
(defun parse-problem ()
  (let ((map       (make-hash-table :test #'eq))
        (amphipods (list))
        (lines     (read-problem "day23.in")))
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

(defun amphipod-is-done (amphipod)
  (bind (((type active (x . y)) amphipod))
    (declare (ignorable active))
    (case type
      (A (and (= x 3) (>= y 2)))
      (B (and (= x 5) (>= y 2)))
      (C (and (= x 7) (>= y 2)))
      (D (and (= x 9) (>= y 2))))))

(defun done (amphipods)
  (iter
    (for amphipod in amphipods)
    (always (amphipod-is-done amphipod))))

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

(defun path-to-room-is-clear (start-coord destination-column map amphipods)
  (iter
    (with to-explore = (make-queue :simple-queue))
    (with seen = (make-hash-table :test #'eq))
    (initially
     (iter
       (for coord in (empty-squares-around start-coord amphipods map))
       (qpush to-explore coord)
       (setf (gethash coord seen) t)))
    (while (not (= 0 (qsize to-explore))))
    (for next-coord = (qpop to-explore))
    (thereis (and (= destination-column (car next-coord))
                  (>= (cdr next-coord) 2)))
    (iter
      (for coord in (empty-squares-around next-coord amphipods map))
      (when (not #1=(gethash coord seen))
        (qpush to-explore coord)
        (setf #1# t)))))

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

(defun path-to-destination-is-clear (amphipod map amphipods)
  (bind (((type active coord)        amphipod)
         (destination-column         (destination-column amphipod))
         (amphipod-at-bottom-of-room (get-amphipod-at amphipods (coord (destination-column amphipod) 3)))
         (amphipod-at-top-of-room    (get-amphipod-at amphipods (coord (destination-column amphipod) 3)))
         (room-can-take-amphipod     (and (null amphipod-at-top-of-room)
                                          (or (null amphipod-at-bottom-of-room)
                                              (amphipods-are-same-type amphipod
                                                                       amphipod-at-bottom-of-room))))
         (has-clear-path             (path-to-room-is-clear coord
                                                            destination-column
                                                            map
                                                            amphipods)))
    (declare (ignore type active))
    (and room-can-take-amphipod has-clear-path)))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem)))
    (path-to-destination-is-clear (list 'A nil (coord 2 1)) map (list (list 'A nil (coord 2 1))))))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem)))
    (path-to-destination-is-clear (list 'A nil (coord 2 1)) map (list (list 'A nil (coord 2 1))
                                                                      (list 'B nil (coord 3 3))))))

;; Assume that the room is a valid destination
(defun destination-coord (amphipod amphipods)
  (bind (((type active coord)        amphipod)
         (destination-column         (destination-column amphipod))
         (amphipod-at-bottom-of-room (get-amphipod-at amphipods (coord (destination-column amphipod) 3))))
    (declare (ignore type active coord))
    (coord destination-column (if amphipod-at-bottom-of-room 2 3))))

(defun steps-in-route-to-room (amphipod amphipods)
  (bind (((type active coord) amphipod)
         ((x . y)             coord)
         ((dest-x . dest-y)   (destination-coord amphipod amphipods)))
    (declare (ignore type active))
    (+ (abs (- x dest-x))
       (abs (- y dest-y)))))

(defun energy-to-get-to-room (amphipod amphipods)
  (* (energy (car amphipod))
     (steps-in-route-to-room amphipod amphipods)))

;; State is (Number . (Amphipod))

(defun empty-squares-around (coord amphipods map)
  (bind (((x . y) coord))
    (iter
      (for coord in (list (coord (1- x) y)
                          (coord (1+ x) y)
                          (coord x (1- y))
                          (coord x (1+ y))))
      (when (and (eq 'space (gethash coord map))
                 (null (find coord amphipods :key #'caddr)))
        (collecting coord)))))

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

(defun states-from (state map)
  (bind ((amphipods       (caddr state))
         (energy          (cadr state))
         (correct         (car state))
         (active-amphipod (find t amphipods :key #'cadr))
         (active-coord    (and active-amphipod (caddr active-amphipod))))
    (if (null active-amphipod)
        ;; Make any amphipod active
        (iter
          (for amphipod in amphipods)
          (when (amphipod-is-done amphipod)
            (next-iteration))
          (for (type active coord) = amphipod)
          ;; Once an amiphod stops being active in the corridor it
          ;; can't move again.
          (when (= 1 (cdr coord))
            (next-iteration))
          (for empty-squares = (empty-squares-around coord amphipods map))
          (when (null empty-squares)
            (next-iteration))
          (declare (ignorable active))
          (collecting (list correct
                            energy
                            (swap-amphipod-at amphipods coord (list type t coord)))))
        
        (append
         ;; Move the active amphipod in any available direction
         (if (and (not (amphipod-is-done active-amphipod))
                  (path-to-destination-is-clear active-amphipod map amphipods))
             (list (list (1+ correct)
                         (+ energy (energy-to-get-to-room active-amphipod amphipods))
                         (swap-amphipod-at amphipods
                                           active-coord
                                           (list (car active-amphipod)
                                                 nil
                                                 (destination-coord active-amphipod
                                                                    amphipods)))))
             (when (not (amphipod-is-done active-amphipod))
               (iter
                 (with (type active coord) = active-amphipod)
                 (declare (ignorable active))
                 (for square in (empty-squares-around active-coord amphipods map))
                 (when (and (/= (destination-column active-amphipod) (car square))
                            (>= (cdr square) 2)
                            (/= (cdr coord) 3))
                   (next-iteration))
                 (collecting (list (if (eq (destination-coord active-amphipod amphipods) square)
                                       (1+ correct)
                                       correct)
                                   (+ energy (energy type))
                                   (swap-amphipod-at amphipods coord (list type t square)))))))

         ;; Make the active amphipod inactive (only if it's in the corridor)
         (when (= 1 (cdr active-coord))
           (list (list correct
                       energy
                       (swap-amphipod-at amphipods
                                         (caddr active-amphipod)
                                         (list (car active-amphipod) nil (caddr active-amphipod))))))

         ;; Try to move any amphipod to their destination spot
         (iter outer
           (for amphipod in amphipods)
           (for (type active coord) = amphipod)
           (when (and (not (amphipod-is-done amphipod))
                      (path-to-destination-is-clear amphipod map amphipods))
             (collecting
              (list (1+ correct)
                    (+ energy (energy-to-get-to-room amphipod amphipods))
                    (swap-amphipod-at amphipods
                                      coord
                                      (list type active (destination-coord amphipod amphipods)))))))))))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem))
         (state (cons 0 amphipods)))
    (states-from state map)))

(defun find-cheapest-cost (amphipods map)
  (iter
    (with to-explore = (make-instance 'cl-heap:priority-queue))
    (with seen       = (make-hash-table :test #'equal))
    (with best       = nil)
    (initially
     (cl-heap:enqueue to-explore (list 0 0 amphipods) 0)
     (setf (gethash amphipods seen) t))
    (for current-state = (cl-heap:dequeue to-explore))
    ;; (iter
    ;;   (for amphipod in (caddr current-state))
    ;;   (counting (amphipod-is-done amphipod) into correct)
    ;;   (finally
    ;;    (format t "correct: ~a~%" correct)))
    (format t "other-correct: ~a~%" (car current-state))
    ;; (format t "current-state: ~a~%" current-state)
    (when (and (= (car current-state) 8)
               (done (caddr current-state)))
      (return (cadr current-state)))
    (iter
      (for next-state in (states-from current-state map))
      ;; (format t "next-state: ~a~%" next-state)
      (when #1=(gethash (caddr next-state) seen)
        (next-iteration))
      (setf #1# t)
      (cl-heap:enqueue to-explore next-state (- 8 (car next-state))))))

(defun part-1 ()
  (let ((coord-pool (list)))
    (fill-coord-pool 14 14)
    (bind (((map amphipods) (parse-problem)))
      (find-cheapest-cost amphipods map))))
