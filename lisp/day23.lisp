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

;; (declaim (optimize (speed 3) (safety 0)))

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
                 (push (cons 'D (coord x y)) amphipods)
                 (setf (gethash (coord x y) map)  'space)))
          (#\C (progn
                 (push (cons 'C (coord x y)) amphipods)
                 (setf (gethash (coord x y) map)  'space)))
          (#\B (progn
                 (push (cons 'B (coord x y)) amphipods)
                 (setf (gethash (coord x y) map)  'space)))
          (#\A (progn
                 (push (cons 'A (coord x y)) amphipods)
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
  (bind (((type . (x . y)) amphipod)
         (above-others     (< 1 y depth)))
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
  (find coord amphipods :key #'cdr))

(defun destination-column (amphipod)
  (case (car amphipod)
    (A 3)
    (B 5)
    (C 7)
    (D 9)))

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

(defun room-contains-correct-amphipods (amphipod amphipods depth)
  (bind ((destination-column (destination-column amphipod)))
    (iter
      (for yi from 2 to depth)
      (for occupant = (get-amphipod-at amphipods (coord destination-column yi)))
      (always (or (null occupant)
                  (amphipods-are-same-type amphipod occupant))))))

(defun path-to-destination-is-clear (amphipod map amphipods depth destination-coord)
  (bind ((coord                  (cdr amphipod))
         (room-can-take-amphipod (room-contains-correct-amphipods amphipod amphipods depth))
         (has-clear-path         (path-between-coords-is-clear coord
                                                               destination-coord
                                                               map
                                                               amphipods)))
    (declare (ignore type))
    (and room-can-take-amphipod has-clear-path)))

;; Assume that the room is a valid destination
(defun destination-coord (amphipod amphipods depth)
  (bind ((destination-column (destination-column amphipod)))
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

(defvar current-amphipod-locations)

(defun empty-squares-around (coord amphipods map)
  (iter
    (for coord-around in (cached-spaces coord map))
    (when (null (gethash coord-around current-amphipod-locations))
      (collecting coord-around))))

(defun swap-amphipod-at (amphipods coord new-amphipod)
  (cond
    ((null amphipods) nil)
    ((eq (cdar amphipods) coord)
     (cons new-amphipod
           (cdr amphipods)))
    (t (cons (car amphipods)
             (swap-amphipod-at (cdr amphipods) coord new-amphipod)))))

#+nil
(let ((coord-pool (list)))
  (fill-coord-pool 14 14)
  (bind (((map amphipods) (parse-problem)))
    (format t "~a~%" amphipods)
    (swap-amphipod-at amphipods (coord 3 2) (cons 'boom (coord 3 2)))))

(defvar corridor-coordinates)

(defun states-from (state map depth)
  (bind ((amphipods                  (caddr state))
         (energy                     (cadr state))
         (correct                    (car state))
         (has-root-home              (list))
         (current-amphipod-locations (make-hash-table :test #'eq)))
    (iter
      (for amphipod in amphipods)
      (setf (gethash (cdr amphipod) current-amphipod-locations) t))
    (append
     ;; Try to move any amphipod that's in the corridor to its final
     ;; position
     (iter outer
       (for amphipod in amphipods)
       (for (type . coord) = amphipod)
       (bind ((destination-coord (destination-coord amphipod amphipods depth)))
        (awhen (and (not (amphipod-is-done amphipod amphipods depth))
                    (path-to-destination-is-clear amphipod map amphipods depth destination-coord))
          (push amphipod has-root-home)
          (collecting
           (list (1+ correct)
                 (+ energy (* (energy type) it))
                 (swap-amphipod-at amphipods
                                   coord
                                   (cons type destination-coord)))))))

     ;; Pick any amphipod not in it's destination and not in the
     ;; corridor and create a state from each position it could be in
     ;; the corridor (other than directly above a room)
     (iter outer
       (for amphipod in amphipods)
       (when (member amphipod has-root-home))
       (for (type . coord) = amphipod)
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
                                                     (cons type dest-coord))))))))))))

(defun amphipod-x (amphipod)
  (cadr amphipod))

(defun amphipod-y (amphipod)
  (cddr amphipod))

(defun find-cheapest-cost (amphipods map depth)
  (iter
    (with to-explore = (make-instance 'cl-heap:priority-queue))
    (with seen       = (make-hash-table :test #'equal))
    (with best       = most-positive-fixnum)
    (initially
     (let ((correct
             (iter
               (for amphipod in amphipods)
               (counting (amphipod-is-done amphipod amphipods depth)))))
       (cl-heap:enqueue to-explore (list (print correct) 0 amphipods) 0)
       (setf (gethash amphipods seen) 0)))
    (for current-state = (cl-heap:dequeue to-explore))
    (when (null current-state)
      (return best))
    (when (> (cadr current-state) best)
      (next-iteration))
    ;; (for i from 0 below 10000)
    ;; (format t "correctly-ordered: ~a~%" (car current-state))
    (when (and (= (car current-state) (length amphipods)))
      ;; (format t "(car current-state): ~a~%" (car current-state))
      ;; (format t "current-state: ~a~%" current-state)
      (when (< (cadr current-state) best)
        (format t "best: ~a~%" (cadr current-state)))
      (setf best (min best (cadr current-state))))
    (iter
      (for next-state in (states-from current-state map depth))
      ;; (when (> #1=(gethash (caddr next-state) seen most-positive-fixnum)
      ;;          (cadr next-state))
      ;;   (setf #1# (cadr next-state)))
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

;; 48800 too high!
