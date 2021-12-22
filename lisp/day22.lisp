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

;; max(min(a',x')-max(a,x),0)
;; * max(min(b',y')-max(b,y),0)
;; * max(min(c',z')-max(c,z),0)

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
    ;; (format t "overlaps: ~a~%" overlaps)
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

(defun part-2 ()
  (bind ((instructions (parse-problem))
         (boxes-added  (list)))
    (iter
      (for i from 0 below 10)
      (for (op x-start
               x-end
               y-start
               y-end
               z-start
               z-end) in instructions)
      ;; For all the boxs we already have, remove the intersecting
      ;; boxes from this one by intersecting with all of the removed
      ;; boxes and then adding to the list of removed boxes.
      (when (eq op 'on)
        (bind ((current-box (list x-start
                                  x-end
                                  y-start
                                  y-end
                                  z-start
                                  z-end))
               (areas-seen (areas-seen current-box boxes-added)))
          (format t "areas-seen: ~a~%" areas-seen)
          (summing (- (* (1+ (- x-end x-start))
                         (1+ (- y-end y-start))
                         (1+ (- z-end z-start)))
                      areas-seen))
          (push current-box boxes-added))))))

;; 870227697432749 too low!
