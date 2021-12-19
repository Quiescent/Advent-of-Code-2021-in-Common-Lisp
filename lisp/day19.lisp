(defpackage :day19
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

(in-package :day19)

(defun parse-problem ()
  (iter
    (with scanners = (make-hash-table :test #'eq))
    (with scanner-id)
    (for line in (read-problem "day19.in"))
    (match line
      ((ppcre "--- scanner ([0-9]+) ---"
              (read id))
       (setf scanner-id id))
      ((ppcre  "(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)"
               (read x) (read y) (read z))
       (push (list x y z) (gethash scanner-id scanners))))
    (finally (return scanners))))

;; pi/2  => ((1  0  0)
;;           (0  0 -1)
;;           (0  1  0))
;;
;; 2pi/2 => ((1  0  0)
;;           (0 -1 -1)
;;           (0  0 -1))
;;
;; 3pi/2 => ((1  0  0)
;;           (0 -1  1)
;;           (0 -1 -1))
;;
;; 4pi/2 => ((1  0  0)
;;           (0  1  0)
;;           (0 -1  1))

;; Permutations of (x y z):
;; (x y z)
;; (x z y)
;; (y x z)
;; (y z x)
;; (z x y)
;; (z y x)

(defmacro gen-permutations ()
  (cons 'list
        (apply #'concatenate 'list
               (iter
                 (for (x y z) in (permutations '(x y z)))
                 (collecting
                  `((list ,x       ,y       ,z)
                    (list (- 0 ,x) ,y       ,z)
                    (list ,x       (- 0 ,y) ,z)
                    (list (- 0 ,x) (- 0 ,y) ,z)
                    (list ,x       ,y       (- 0 ,z))
                    (list (- 0 ,x) ,y       (- 0 ,z))
                    (list ,x       (- 0 ,y) (- 0 ,z))
                    (list (- 0 ,x) (- 0 ,y) (- 0 ,z))))))))

(defun other-rotations-hard-coded (beacon)
  (bind (((x y z) beacon))
    (gen-permutations)))

(defun other-scanner-rotations (scanner)
  (or #1=(gethash scanner other-scanner-rotations-cache)
      (setf #1#
            (iter
              (with all-scanners)
              (initially
               (dotimes (i (length (other-rotations-hard-coded (car scanner))))
                 (push (list) all-scanners)))
              (for beacon in scanner)
              (iter
                (for rotation in (other-rotations-hard-coded beacon))
                (for dest on all-scanners)
                (push rotation (car dest)))
              (finally (return all-scanners))))))

(defun find-x-offsets (scanner-1 scanner-2)
  (iter
    (for offset from -3000 to 3000)
    (for count =
         (iter
           (for (x-1 y-1 z-1) in scanner-1)
           (counting
            (iter
              (for (x-2 y-2 z-2) in scanner-2)
              (thereis (= (+ x-2 offset) x-1))))))
    (when (>= count 12)
      (collecting (cons count offset)))))

(defun find-y-offsets (scanner-1 scanner-2)
  (iter
    (for offset from -3000 to 3000)
    (for count =
         (iter
           (for (x-1 y-1 z-1) in scanner-1)
           (counting
            (iter
              (for (x-2 y-2 z-2) in scanner-2)
              (thereis (= (+ y-2 offset) y-1))))))
    (when (>= count 12)
      (collecting (cons count offset)))))

(defun find-z-offsets (scanner-1 scanner-2)
  (iter
    (for offset from -3000 to 3000)
    (for count =
         (iter
           (for (x-1 y-1 z-1) in scanner-1)
           (counting
            (iter
              (for (x-2 y-2 z-2) in scanner-2)
              (thereis (= (+ z-2 offset) z-1))))))
    (when (>= count 12)
      (collecting (cons count offset)))))

(defun compute-offsets (scanner-1 scanner-2)
  (list (car (sort (find-x-offsets scanner-1 scanner-2) #'> :key #'car))
        (car (sort (find-y-offsets scanner-1 scanner-2) #'> :key #'car))
        (car (sort (find-z-offsets scanner-1 scanner-2) #'> :key #'car))))

(defun make-relative-to (scanner-1 scanner-2)
  (bind (((dx dy dz) (compute-offsets scanner-1 scanner-2)))
    (if (or (null dx) (null dy) (null dz))
        nil
        (list (cdr dx) (cdr dy) (cdr dz)
              (mapcar (lambda (beacon) (bind (((x-2 y-2 z-2) beacon))
                                         (list (+ x-2 (cdr dx))
                                               (+ y-2 (cdr dy))
                                               (+ z-2 (cdr dz)))))
                      scanner-2)))))

(defvar other-scanner-rotations-cache)

(defun match-scanners (scanner-1 scanner-2)
  (iter
    (for rotation in (other-scanner-rotations scanner-2))
    (thereis (make-relative-to scanner-1 rotation))))

(defun find-matches (scanners)
  (let ((other-scanner-rotations-cache (make-hash-table :test #'eq)))
    (iter
      (with graph = (make-hash-table :test #'eq))
      (for i from 0)
      (format t "i: ~a~%" i)
      (for (key scanner) in-hashtable scanners)
      (iter
        (for (other-key other-scanner) in-hashtable scanners)
        (when (eq scanner other-scanner)
          (next-iteration))
        (awhen (match-scanners scanner other-scanner)
          (push (list other-key nil) (gethash key graph))
          (push (list key       nil) (gethash other-key graph))))
      (finally (return graph)))))

(defun all-relative-to-0 (graph scanners)
  (iter
    (with all-beacons = (gethash 0 scanners))
    (with to-explore = (make-queue :simple-queue))
    (initially
     (dolist (key (->> (mapcar #'car (gethash 0 graph))))
       (qpush to-explore (list key (gethash 0 scanners)))))
    (with seen = (list 0))

    (for (key prev-scanner) = (qpop to-explore))
    (for (dx dy dz scanner) = (match-scanners prev-scanner (gethash key scanners)))
    (setf all-beacons  (concatenate 'list all-beacons scanner))

    (format t "key: ~a~%" key)
    (format t "scanner: ~a~%" scanner)
    (format t "all-beacons: ~a~%" all-beacons)

    (iter
      (for (other-key . _) in (gethash key graph))
      (when (member other-key seen)
        (next-iteration))
      (format t "enqueueing other-key: ~a~%" other-key)
      (qpush to-explore (list other-key scanner))
      (push other-key seen))

    (while (/= 0 (qsize to-explore)))

    (finally
     (return
       (->> (remove-duplicates all-beacons :test #'equal)
         length)))




    ;; (for (key scanner) in-hashtable scanners)
    ;; (when (eq key 0)
    ;;   (collecting scanner into result)
    ;;   (next-iteration))
    ;; (for path =
    ;;      (iter
    ;;        ;; (with this-scanner = scanner)
    ;;        (with from = (make-hash-table :test #'eq))
    ;;        (with stack = (list key))
    ;;        (with seen = ())
    ;;        (for current = (pop stack))
    ;;        (while (/= current 0))
    ;;        (for next-links = (gethash current graph))
    ;;        (iter
    ;;          (for (next-key . _) in next-links)
    ;;          (when (member next-key seen)
    ;;            (next-iteration))
    ;;          (push next-key seen)
    ;;          (push next-key stack)
    ;;          (setf (gethash next-key from) current))
    ;;        (finally
    ;;         (return
    ;;           (iter
    ;;             (for current initially 0 then (gethash current from))
    ;;             (while (/= current key))
    ;;             (collecting current :at start))))))
    ;; (iter
    ;;   (with current = key)
    ;;   (for step in path)
    ;;   (for edges = (gethash current graph))
    ;;   (for (other-key . _) = (find step edges :key #'car))
    ;;   (for (_key (_diff rotated)) = (match-scanners ))
    ;;   (iter
    ;;     (for beacon in scanner)
    ;;     (decf (car beacon) dx)
    ;;     (decf (cadr beacon) dy)
    ;;     (decf (caddr beacon) dz))
    ;;   (setf current step))
    ;; (format t "scanner: ~a~%" scanner)
    ;; (collecting scanner into result)
    ;; (finally
    ;;  (return
    ;;    (-<> (apply #'concatenate 'list result)
    ;;      (remove-duplicates <> :test #'equal)
    ;;      length)))
    ))

(defun part-1 ()
  (let* ((scanners (parse-problem))
         (graph (find-matches scanners)))
    (all-relative-to-0 graph scanners)))


;; Bin

(defun per-slot-diff (beacon-1 beacon-2)
  (+ (abs (- (car beacon-1) (car beacon-2)))
     (abs (- (cadr beacon-1) (cadr beacon-2)))
     (abs (- (caddr beacon-1) (caddr beacon-2)))))

;; (defun compute-offsets (scanner-1 scanner-2)
;;   (iter
;;     (for dx from -500 to 500)
;;     (for (count . offsets) =
;;          (iter
;;            (for dy from -500 to 500)
;;            (for (count . offsets) =
;;                 (iter
;;                   (for dz from -500 to 500)
;;                   (for count = (iter
;;                                  (for (x-1 y-1 z-1) in scanner-1)
;;                                  (summing
;;                                   (iter
;;                                     (for (x-2 y-2 z-2) in scanner-2)
;;                                     (counting (and (= (+ x-2 dx) x-1)
;;                                                    (= (+ y-2 dy) y-1)
;;                                                    (= (+ z-2 dz) z-1)))))))
;;                   (finding (cons count (list dx dy dz)) maximizing count)))
;;            (finding (cons count offsets) maximizing count)))
;;     (finding (cons count offsets) maximizing count)))

(defun permutations (xs)
  (if (= (length xs) 1)
      (list xs)
      (iter outer
        (for x in xs)
        (for rest = (remove x xs))
        (iter
          (for other in (permutations rest))
          (in outer (collecting (cons x other)))))))

(defun vector-matrix-mult (vector matrix)
  (bind (((x y z) vector)
         (((a b c)
           (d e f)
           (g h i)) matrix))
    (list (+ (* x a) (* x d) (* x g))
          (+ (* y b) (* y e) (* y h))
          (+ (* z c) (* z f) (* z i)))))

(defun other-rotations (beacon)
  (bind ((xss       (permutations beacon))

         (rot-90-x  (x-rotation-matrix (/ pi 2)))
         (rot-180-x (x-rotation-matrix (* 2 (/ pi 2))))
         (rot-270-x (x-rotation-matrix (* 3 (/ pi 2))))

         (rot-90-y  (y-rotation-matrix (/ pi 2)))
         (rot-180-y (y-rotation-matrix (* 2 (/ pi 2))))
         (rot-270-y (y-rotation-matrix (* 3 (/ pi 2))))

         (rot-90-z  (z-rotation-matrix (/ pi 2)))
         (rot-180-z (z-rotation-matrix (* 2 (/ pi 2))))
         (rot-270-z (z-rotation-matrix (* 3 (/ pi 2)))))
    (iter
      (for xs in xss)
      (collecting xs)
      (collecting (-> (vector-matrix-mult xs rot-90-x)
                    (vector-matrix-mult rot-90-y)
                    (vector-matrix-mult rot-90-z)))
      (collecting (-> (vector-matrix-mult xs rot-180-x)
                    (vector-matrix-mult rot-180-y)
                    (vector-matrix-mult rot-180-z)))
      (collecting (-> (vector-matrix-mult xs rot-270-x)
                    (vector-matrix-mult rot-270-y)
                    (vector-matrix-mult rot-270-z))))))

(defun x-rotation-matrix (radians)
  (list (list 1 0                     0)
        (list 0 (round (cos radians)) (round (- 0 (sin radians))))
        (list 0 (round (sin radians)) (round (cos radians)))))

(defun y-rotation-matrix (radians)
  (list (list (round (cos radians))       0 (round (sin radians)))
        (list 0                           1 0)
        (list (- 0 (round (sin radians))) 0 (round (cos radians)))))

(defun z-rotation-matrix (radians)
  (list (list (round (cos radians)) (round (- 0 (sin radians))) 0)
        (list (round (sin radians)) (round (cos radians))       0)
        (list 0                     0                           1)))

(defun count-hits (scanner-1 scanner-2)
  (count-if (lambda (beacon) (find beacon scanner-2 :test #'equal)) scanner-1))
