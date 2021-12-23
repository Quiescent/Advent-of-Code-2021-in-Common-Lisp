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
    (iter
      (for x from 0 to max-x)
      (setf (aref coord-pool y x) (cons x y)))))

(defun coord (x y)
  (aref coord-pool y x))

#+nil
(let ((coord-pool (make-array (list 15 15))))
  (fill-coord-pool 14 14)
  (coord 2 3))

;; Amphipod is (Symbol Bool Cons)
(defun parse-problem (&optional (filename "day23.in"))
  (let ((map       (make-hash-table :test #'eq))
        (amphipods (make-hash-table :test #'eq))
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
                 (setf (gethash (coord x y) amphipods) (cons 'D (coord x y)))
                 (setf (gethash (coord x y) map)  'space)))
          (#\C (progn
                 (setf (gethash (coord x y) amphipods) (cons 'C (coord x y)))
                 (setf (gethash (coord x y) map)  'space)))
          (#\B (progn
                 (setf (gethash (coord x y) amphipods) (cons 'B (coord x y)))
                 (setf (gethash (coord x y) map)  'space)))
          (#\A (progn
                 (setf (gethash (coord x y) amphipods) (cons 'A (coord x y)))
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
  (bind (((type . status-or-coord) amphipod))
    (or (eq status-or-coord 'done)
        (bind (((x . y)      status-or-coord)
               (above-others (< 1 y depth)))
          (and (or (not above-others)
                   (iter
                     (for yi from y to depth)
                     (for amphipod-below = (get-amphipod-at amphipods (coord x yi)))
                     (when (eq 'done (cdr amphipod-below))
                       (return t))
                     (always (amphipods-are-same-type amphipod amphipod-below))))
               (case type
                 (A (and (= x 3) (>= y 2)))
                 (B (and (= x 5) (>= y 2)))
                 (C (and (= x 7) (>= y 2)))
                 (D (and (= x 9) (>= y 2)))))))))

(defun get-amphipod-at (amphipods coord)
  (gethash coord amphipods))

(defun destination-column (amphipod)
  (case (car amphipod)
    (A 3)
    (B 5)
    (C 7)
    (D 9)))

(defun amphipods-are-same-type (one other)
  (eq (car one)
      (car other)))

;; (defvar current-amphipod-locations)

(defun path-between-coords-is-clear (start-coord destination-coord map amphipods)
  (bind (((x-dest . y-dest) destination-coord)
         ((x      . y)      start-coord)
         (going-left        (< x-dest x)))
    (when (null (get-amphipod-at amphipods destination-coord)
                ;; (aref current-amphipod-locations (cdr destination-coord) (car destination-coord))
                )
      (iter
        (for at-x = (= x x-dest))
        (if (not at-x)
            (bind ((next-x        (if going-left (1- x) (1+ x)))
                   (space-to-side (and (= y 1)
                                       (null (get-amphipod-at amphipods (coord next-x y))
                                             ;;(aref current-amphipod-locations y next-x)
                                             ))))
              (if space-to-side
                  (setf x next-x)
                  (if (eq y 1)
                      (return nil)
                      (bind ((next-y      (1- y))
                             (space-above (and (> y 1)
                                               (null (get-amphipod-at amphipods (coord x next-y))
                                                ;;(aref current-amphipod-locations next-y x)
                                                ))))
                        (if space-above
                            (setf y next-y)
                            (return nil))))))
            (bind ((at-y (= y y-dest)))
              (if at-y
                  (return length)
                  (bind ((next-y      (1+ y))
                         (space-below (null (get-amphipod-at amphipods (coord x next-y))
                                       ;;(aref current-amphipod-locations next-y x)
                                       )))
                    (if space-below
                        (setf y next-y)
                        (return nil))))))
        (counting t into length)))))

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
                                                      (coord destination-column
                                                             (1+ yi)))))
               depth))))

;; State is (Number . (Amphipod))

(defun swap-amphipod-at (amphipods coord new-coord new-amphipod)
  (let ((new-table (make-hash-table :test #'eq)))
    (iter
      (for (key value) in-hashtable amphipods)
      (setf (gethash key new-table) value))
    (remhash coord new-table)
    (setf (gethash new-coord new-table) new-amphipod)
    new-table))

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
         (has-root-home              (list)))
    (append
     ;; Try to move any amphipod that's in the corridor to its final
     ;; position
     (iter outer
       (for (coord amphipod) in-hashtable amphipods)
       (for (type . status-or-coord) = amphipod)
       (bind ((destination-coord (destination-coord amphipod amphipods depth)))
        (awhen (and (not (amphipod-is-done amphipod amphipods depth))
                    (path-to-destination-is-clear amphipod map amphipods depth destination-coord))
          ;; (push amphipod has-root-home)
          (collecting
           (list (1+ correct)
                 (+ energy (* (energy type) it))
                 (swap-amphipod-at amphipods
                                   coord
                                   destination-coord
                                   (cons type 'done)))))))

     ;; Pick any amphipod not in it's destination and not in the
     ;; corridor and create a state from each position it could be in
     ;; the corridor (other than directly above a room)
     (iter outer
       (for (coord amphipod) in-hashtable amphipods)
       (when (member amphipod has-root-home))
       (for (type . status-or-coord) = amphipod)
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
                                                     dest-coord
                                                     (cons type dest-coord))))))))))))

(defun amphipod-x (amphipod)
  (cadr amphipod))

(defun amphipod-y (amphipod)
  (cddr amphipod))

(defun amphipods-key (amphipods)
  (iter outer
    (for (coord (type . coord-or-status)) in-hashtable amphipods)
    (collecting (aref (symbol-name type) 0) :result-type string)
    (if (consp coord-or-status)
        (iter
          (for char in-string (princ-to-string (+ (car coord-or-status) (* 15 (cdr coord-or-status)))))
          (in outer (collecting char :result-type string)))
        (collecting #\# :result-type string))))

(defun find-cheapest-cost (amphipods map depth)
  (iter
    (with to-explore = (make-instance 'cl-heap:priority-queue))
    (with seen       = (make-hash-table :test #'equal))
    (with best       = most-positive-fixnum)
    (initially
     (let ((correct
             (iter
               (for (coord amphipod) in-hashtable amphipods)
               (counting (amphipod-is-done amphipod amphipods depth)))))
       (cl-heap:enqueue to-explore (list correct 0 amphipods) 0)
       (setf (gethash (amphipods-key amphipods) seen) 0)))
    (for current-state = (cl-heap:dequeue to-explore))
    (when (null current-state)
      (return best))
    (when (> (cadr current-state) best)
      (next-iteration))
    (when (and (= (car current-state) (hash-table-count amphipods)))
      (when (< (cadr current-state) best)
        (format t "best: ~a~%" (cadr current-state)))
      (setf best (min best (cadr current-state))))
    (iter
      (for next-state in (states-from current-state map depth))
      (when (> #1=(gethash (amphipods-key (caddr next-state)) seen most-positive-fixnum)
               (cadr next-state))
        (setf #1# (cadr next-state))
        (cl-heap:enqueue to-explore next-state (+ (* 10000 (- 8 (car next-state)))
                                                  (cadr next-state)) ;(cadr next-state) ;(- 8 (car next-state))
                         )))))

(defun part-1 ()
  (let ((coord-pool (make-array (list 15 15))))
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
  (let ((coord-pool (make-array (list 15 15))))
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
;; 47236 too high!
