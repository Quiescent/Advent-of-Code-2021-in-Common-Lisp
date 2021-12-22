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

;; Interval map is ((start: Number, end: Number, x: T))
;;
;; Sort it after each step to make merging easier.

(defun interval-map ()
  (list))

(defun find-intersecting (interval-map start end)
  (assert (>= end start))
  (iter
    (for cell on interval-map)
    (for p-interval previous cell)
    (for (istart iend x) = (car cell))
    (finding (list p-interval cell (cdr cell))
             :such-that (or (<= start istart end)
                            (<= start iend   end)
                            (<= start istart iend end)
                            (<= istart start end iend)))))

(defun sort-intervals (interval-map)
  (sort interval-map #'< :key #'car))

(defun insert-range-with (interval-map start end fn)
  (assert (>= end start))
  (if (null interval-map)
      (list (list start end (funcall fn nil)))
      (bind ((intersected (find-intersecting interval-map start end)))
        (if intersected
            (bind (((previous intersection next) intersected)
                   ((istart iend x)              (car intersection))
                   (new-cell                     (cons (list (min istart start)
                                                             (max iend   end)
                                                             (funcall fn x))
                                                       next)))
              (if previous
                  (setf (cdr previous) new-cell)
                  (setf interval-map   new-cell)))
            (setf (cdr interval-map)
                  (cons (list start end (funcall fn nil))
                        (cdr interval-map))))
        (sort-intervals interval-map))))

#+nil
(-> (interval-map)
  (insert-range-with 2 4   (lambda (x) (if (null x) 'x (cons x 'x))))
  (insert-range-with 6 8   (lambda (x) (if (null x) 'y (cons x 'y))))
  (insert-range-with 10 12 (lambda (x) (if (null x) 'z (cons x 'z)))))

#+nil
(-> (interval-map)
  (insert-range-with 2 4   (lambda (x) (if (null x) 'x (cons x 'x))))
  (insert-range-with 4 8   (lambda (x) (if (null x) 'y (cons x 'y))))
  (insert-range-with 10 12 (lambda (x) (if (null x) 'z (cons x 'z))))
  (insert-range-with -1 1  (lambda (x) (if (null x) 'a (cons x 'a)))))

(defun part-2 ()
  (bind ((instructions (parse-problem)))
    ))

;; 870227697432749 too low!
