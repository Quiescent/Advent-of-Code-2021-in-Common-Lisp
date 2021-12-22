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

;; Region is ((left-min left-max sub-regions))

(defun add-region-x (op interval regions)
  (bind (((xi-start xi-end yi-start yi-end zi-start zi-end) interval))
    (declare (ignore yi-start yi-end zi-start zi-end))
    (if (and (null regions) (eq op 'on))
        (list (list xi-start xi-end (add-region-y 'on interval nil)))
        (iter
          (for region in regions)
          (for (x-start x-end sub-regions) = region)
          (with added)
          (cond
            ;; Turn on with region contained
            ((and (eq op 'on) (<= xi-start x-start) (>= xi-end x-end))
             (setf added t)
             (when (/= x-start xi-start)
               (collecting (list xi-start (1- x-start) (add-region-y op interval nil)) into result))
             (collecting (list x-start x-end (add-region-y op interval sub-regions)) into result)
             (when (/= xi-end x-end)
               (collecting (list (1+ x-end) xi-end (add-region-y op interval nil)) into result)))

            ;; Turn on with overlap contained
            ((and (eq op 'on) (>= xi-start x-start) (<= xi-end x-end))
             (setf added t)
             (when (/= x-start xi-start)
               (collecting (list x-start     (1- xi-start) sub-regions) into result))
             (collecting (list xi-start    xi-end        (add-region-y op interval sub-regions)) into result)
             (when (/= xi-end x-end)
               (collecting (list (1+ xi-end) x-end         sub-regions) into result)))

            ;; Turn on with overlap at start
            ((and (eq op 'on) (<= xi-start x-start) (>= xi-end x-start))
             (setf added t)
             (collecting (list xi-start    (1- x-start) (add-region-y 'on interval nil)) into result)
             (collecting (list x-start     xi-end       (add-region-y 'on interval sub-regions)) into result)
             (collecting (list (1+ xi-end) x-end        sub-regions) into result))

            ;; Turn on with overlap at end
            ((and (eq op 'on) (>= xi-end x-end) (<= xi-start x-end))
             (setf added t)
             (collecting (list x-start    (1- xi-start) sub-regions) into result)
             (collecting (list xi-start   x-end         (add-region-y 'on interval sub-regions)) into result)
             (collecting (list (1+ x-end) xi-end        (add-region-y 'on interval nil)) into result))

            ;; Turn off with entire unit contained (don't re-add region)
            ((and (eq op 'off) (<= xi-start x-start) (>= xi-end x-end)) nil)

            ;; Turn off with overlap contained
            ((and (eq op 'off) (>= xi-start x-start) (<= xi-end x-end))
             (when (/= x-start xi-start)
               (collecting (list x-start xi-start (add-region-y op interval sub-regions)) into result))
             (when (/= x-end xi-end)
               (collecting (list xi-end x-end (add-region-y op interval sub-regions)) into result)))

            ;; Turn off with overlap at start
            ((and (eq op 'off) (< xi-start x-start) (< xi-end x-end))
             (collecting (list xi-end x-end (add-region-y op interval sub-regions)) into result))

            ;; No overlap here
            (t (collecting region into result)))
          (finally
           (return
             (if (and (eq op 'on) (not added))
                 (cons (list xi-start xi-end (add-region-y 'on interval nil)) result)
                 result)))))))

(defun add-region-y (op interval regions)
  (bind (((xi-start xi-end yi-start yi-end zi-start zi-end) interval))
    (declare (ignore xi-start xi-end zi-start zi-end))
    (if (and (null regions) (eq op 'on))
        (list (list yi-start yi-end (add-region-z 'on interval nil)))
        (iter
          (for region in regions)
          (for (y-start y-end sub-regions) = region)
          (with added)
          (cond
            ;; Turn on with region contained
            ((and (eq op 'on) (<= yi-start y-start) (>= yi-end y-end))
             (setf added t)
             (when (/= y-start yi-start)
               (collecting (list yi-start (1- y-start) (add-region-z op interval nil)) into result))
             (collecting (list y-start y-end (add-region-z op interval sub-regions)) into result)
             (when (/= yi-end y-end)
               (collecting (list (1+ y-end) yi-end (add-region-z op interval nil)) into result)))

            ;; Turn on with overlap contained
            ((and (eq op 'on) (>= yi-start y-start) (<= yi-end y-end))
             (setf added t)
             (when (/= y-start yi-start)
               (collecting (list y-start     (1- yi-start) sub-regions) into result))
             (collecting (list yi-start    yi-end        (add-region-z op interval sub-regions)) into result)
             (when (/= yi-end y-end)
               (collecting (list (1+ yi-end) y-end         sub-regions) into result)))

            ;; Turn on with overlap at start
            ((and (eq op 'on) (<= yi-start y-start) (>= yi-end y-start))
             (setf added t)
             (collecting (list yi-start    (1- y-start) (add-region-z 'on interval nil)) into result)
             (collecting (list y-start     yi-end       (add-region-z 'on interval sub-regions)) into result)
             (collecting (list (1+ yi-end) y-end        sub-regions) into result))

            ;; Turn on with overlap at end
            ((and (eq op 'on) (>= yi-end y-end) (<= yi-start y-end))
             (setf added t)
             (collecting (list y-start    (1- yi-start) sub-regions) into result)
             (collecting (list yi-start   y-end         (add-region-z 'on interval sub-regions)) into result)
             (collecting (list (1+ y-end) yi-end        (add-region-z 'on interval nil)) into result))

            ;; Turn off with entire unit contained (don't re-add region)
            ((and (eq op 'off) (<= yi-start y-start) (>= yi-end y-end)) nil)

            ;; Turn off with overlap contained
            ((and (eq op 'off) (>= yi-start y-start) (<= yi-end y-end))
             (when (/= y-start yi-start)
               (collecting (list y-start yi-start (add-region-z op interval sub-regions)) into result))
             (when (/= y-end yi-end)
               (collecting (list yi-end y-end (add-region-z op interval sub-regions)) into result)))

            ;; Turn off with overlap at start
            ((and (eq op 'off) (< yi-start y-start) (< yi-end y-end))
             (collecting (list yi-end y-end (add-region-z op interval sub-regions)) into result))

            ;; No overlap here
            (t (collecting region into result)))
          (finally
           (return
             (if (and (eq op 'on) (not added))
                 (cons (list yi-start yi-end (add-region-z 'on interval nil)) result)
                 result)))))))

(defun add-region-z (op interval regions)
  (bind (((xi-start xi-end yi-start yi-end zi-start zi-end) interval))
    (declare (ignore xi-start xi-end yi-start yi-end))
    (if (and (null regions) (eq op 'on))
        (list (list zi-start zi-end nil))
        (iter
          (for region in regions)
          (for (z-start z-end sub-regions) = region)
          (with added)
          (cond
            ;; Turn on with region contained
            ((and (eq op 'on) (<= zi-start z-start) (>= zi-end z-end))
             (setf added t)
             (when (/= z-start zi-start)
               (collecting (list zi-start (1- z-start) nil) into result))
             (collecting (list z-start z-end nil) into result)
             (when (/= zi-end z-end)
               (collecting (list (1+ z-end) zi-end nil) into result)))

            ;; Turn on with any overlap
            ((and (eq op 'on) (or (and (>= zi-start z-start) (<= zi-end z-end))
                                  (and (<= zi-start z-start) (>= zi-end z-start))
                                  (and (>= zi-end z-end)     (<= zi-start z-end))))
             (setf added t)
             (collecting (list (min zi-start z-start)
                               (max zi-end z-end)
                               nil) into result))

            ;; Turn off with entire unit contained (don't re-add region)
            ((and (eq op 'off) (<= zi-start z-start) (>= zi-end z-end)) nil)

            ;; Turn off with overlap contained
            ((and (eq op 'off) (>= zi-start z-start) (<= zi-end z-end))
             (when (/= z-start zi-start)
               (collecting (list z-start zi-start nil) into result))
             (when (/= z-end zi-end)
               (collecting (list zi-end z-end nil) into result)))

            ;; Turn off with overlap at start
            ((and (eq op 'off) (< zi-start z-start) (< zi-end z-end))
             (collecting (list zi-end z-end nil) into result))

            ;; No overlap here
            (t (collecting region into result)))
          (finally
           (return
             (if (and (eq op 'on) (not added))
                 (cons (list zi-start zi-end nil) result)
                 result)))))))

#+nil
(->> (add-region-x 'on '(0 5 0 5 0 5) nil) (add-region-x 'on '(0 5 0 5 0 5)))

#+nil
(->> (add-region-x 'on '(0 5 0 5 0 5) nil) (add-region-x 'on '(2 7 4 9 0 5)))

#+nil
(->> (add-region-x 'on '(0 5 0 5 0 5) nil) (add-region-x 'off '(2 7 4 9 0 5)))

(defun region-size (region)
  (bind (((start end sub-regions) region))
    (if (null sub-regions)
        #1=(- end start)
        (apply #'* #1# (mapcar #'region-size sub-regions)))))

#+nil
(->> (add-region-x 'ON '(0 5 0 5 0 5) nil) region-size)

(defun part-2 ()
  (iter
    (with regions)
    (for i from 0 below 20)
    (for (op x-start
             x-end
             y-start
             y-end
             z-start
             z-end) in (parse-problem))
    (setf regions
          (add-region-x op
                        (list x-start
                              x-end
                              y-start
                              y-end
                              z-start
                              z-end)
                        regions))
    (finally
     (return
       (iter
         (for region in regions)
         (summing (region-size region)))))))
