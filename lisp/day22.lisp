(defpackage :day22
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

(in-package :day22)

(defun parse-problem ()
  (->> (read-problem "day22.in")
    (mapcar (lambda (line) (match line
                        ((ppcre "on x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)"
                                (read x-start)
                                (read x-end)
                                (read y-start)
                                (read y-end)
                                (read z-start)
                                (read z-end))
                         (list 'on
                               x-start
                               x-end
                               y-start
                               y-end
                               z-start
                               z-end))
                        ((ppcre "off x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)"
                                (read x-start)
                                (read x-end)
                                (read y-start)
                                (read y-end)
                                (read z-start)
                                (read z-end))
                         (list 'off
                               x-start
                               x-end
                               y-start
                               y-end
                               z-start
                               z-end)))))))

(defun part-1 ()
  (bind ((map (make-hash-table :test #'equal)))
    (iter
      (for i from 0 below 20)
      (for (op x-start
               x-end
               y-start
               y-end
               z-start
               z-end) in (parse-problem))
      (iter
        (for x from (max -50 x-start) to (min 50 x-end))
        (iter
          (for y from (max -50 y-start) to (min 50 y-end))
          (iter
            (for z from (max -50 z-start) to (min 50 z-end))
            (setf (gethash (list x y z) map) (if (eq op 'on) t nil)))))
      (finally
       (return
         (iter
           (for (key value) in-hashtable map)
           (counting value)))))))

(defun intersecting-area (box-1 box-2)
  (bind (((x-start-1 x-end-1 y-start-1 y-end-1 z-start-1 z-end-1) box-1)
         ((x-start-2 x-end-2 y-start-2 y-end-2 z-start-2 z-end-2) box-2))
    (* (max (1+ (- (min x-end-1 x-end-2) (max x-start-1 x-start-2))) 0)
       (max (1+ (- (min y-end-1 y-end-2) (max y-start-1 y-start-2))) 0)
       (max (1+ (- (min z-end-1 z-end-2) (max z-start-1 z-start-2))) 0))))

(defun intersecting-box (box-1 box-2)
  (bind (((x-start-1 x-end-1 y-start-1 y-end-1 z-start-1 z-end-1) box-1)
         ((x-start-2 x-end-2 y-start-2 y-end-2 z-start-2 z-end-2) box-2))
    (and (> (intersecting-area box-1 box-2) 0)
         (list (max x-start-1 x-start-2)
               (min x-end-1   x-end-2)
               (max y-start-1 y-start-2)
               (min y-end-1   y-end-2)
               (max z-start-1 z-start-2)
               (min z-end-1   z-end-2)))))

(defun areas-seen (box boxes-added)
  (bind ((overlaps (->> (mapcar (lambda (other-box) (intersecting-box box other-box))
                                boxes-added)
                     (remove nil))))
    (if overlaps
        (iter
          (for tail on overlaps)
          (for overlap = (car tail))
          (for (x-start x-end y-start y-end z-start z-end) = overlap)
          (summing (* (1+ (- x-end x-start))
                      (1+ (- y-end y-start))
                      (1+ (- z-end z-start)))
                   :into overlap-area)
          (summing (areas-seen overlap (cdr tail))
                   :into overlap-area-seen)
          (finally (return (- overlap-area overlap-area-seen))))
        0)))

(defun turn-off-regions (off-instruction on-instructions)
  (->>
      (iter
        (for instruction in on-instructions)
        (bind ((intersection (intersecting-box instruction off-instruction)))
          (if intersection
              (bind (((x-start-i x-end-i y-start-i y-end-i z-start-i z-end-i) intersection)
                     ((x-start   x-end   y-start   y-end   z-start   z-end)   instruction))
                (cond
                  ;; on command inside of off command: don't collect any cubes!
                  ((and (>= x-start x-start-i)
                        (<= x-end   x-end-i)
                        (>= y-start y-start-i)
                        (<= y-end   y-end-i)
                        (>= z-start z-start-i)
                        (<= z-end   z-end-i)) nil)

                  ;; off command inside of on command: collect 6 cubes
                  ;;
                  ;; I then realised that one can treat it as though it's
                  ;; always contained and remove invalid cubes
                  ;; afterwards!!
                  (t
                   ;; top slab
                   (collecting (list x-start x-end
                                     y-start y-end
                                     z-start (1- z-start-i)))
                   ;; bottom slab
                   (collecting (list x-start      x-end
                                     y-start      y-end
                                     (1+ z-end-i) z-end))
                   ;; left projected face
                   (collecting (list x-start  (1- x-start-i)
                                     y-start-i y-end-i
                                     z-start-i z-end-i))
                   ;; right projected face
                   (collecting (list (1+ x-end-i) x-end
                                     y-start-i    y-end-i
                                     z-start-i    z-end-i))
                   ;; front sliver
                   (collecting (list x-start   x-end
                                     y-start   (1- y-start-i)
                                     z-start-i z-end-i))
                   ;; back sliver
                   (collecting (list x-start      x-end
                                     (1+ y-end-i) y-end
                                     z-start-i    z-end-i)))))
              (collecting instruction))))
    (remove-if (lambda (instruction)
                 (bind (((x-start x-end y-start y-end z-start z-end) instruction))
                   (or (< x-end x-start)
                       (< y-end y-start)
                       (< z-end z-start)))))))

(defun preprocess (instructions)
  (iter
    (with processed = (list))
    (for instruction in instructions)
    (if (eq (car instruction) 'on)
        (push (cdr instruction) processed)
        (setf processed (turn-off-regions (cdr instruction) processed)))
    (finally (return processed))))

(defun part-2 ()
  (bind ((instructions (preprocess (parse-problem)))
         (boxes-added  (list)))
    (iter
      (for (x-start
            x-end
            y-start
            y-end
            z-start
            z-end) in instructions)
      ;; For all the boxs we already have, remove the intersecting
      ;; boxes from this one by intersecting with all of the removed
      ;; boxes and then adding to the list of removed boxes.
      (bind ((current-box (list x-start
                                x-end
                                y-start
                                y-end
                                z-start
                                z-end))
             (areas-seen (areas-seen current-box boxes-added)))
        (summing (- (* (1+ (- x-end x-start))
                       (1+ (- y-end y-start))
                       (1+ (- z-end z-start)))
                    areas-seen))
        (push current-box boxes-added)))))

;; 870227697432749 too low!
;; 104672080181642 too low!
;; 1334275219162622 Correct!
