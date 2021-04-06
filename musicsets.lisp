;;;; musicsets.lisp

(in-package #:musicsets)

(defun transpose (set n &optional (modulus 12))
  (if (= n modulus)
      set
      (mapcar (lambda (x) (mod (+ x n) modulus)) set)))

(defun invert (set &optional (modulus 12))
  (mapcar (lambda (x) (mod (- modulus x) modulus)) set))

(defun remove-duplicate-sets (list)
  (remove-duplicates list :test #'equal))

(defun remove-equivalent-pitches (set &optional (divisor 12))
  "Remove equivalent (if divisor = 12, then (0,12), (1,13), etc.) pitches from a set, leaving the most reduced form."
  (remove-duplicates (mapcar (lambda (x) (mod x divisor)) set)))

(defun immediate-subsets (set)
  (let ((s (circular set))
        (l (length set)))
    (loop for i below l collect (subseq s i (1- (+ i l))))))

(defun permutations (set)
  (let ((s (circular set))
        (l (length set)))
    (loop for i below l collect (subseq s i (+ i l)))))

(defun circular (list)
  (let ((l (copy-list list)))
    (setf (cdr (last l)) l)))

(defun normal (set)
  (let* ((p (permutations (remove-equivalent-pitches set)))
         (min-width (apply #'min (mapcar (lambda (set) (mod (- (first (last set)) (first set)) 12)) p)))
         (first-pass (remove-if (lambda (set) (> (mod (- (first (last set)) (first set)) 12) min-width)) p)))
    (if (= (length first-pass) 1)
        (first first-pass)
        (let* ((min-width (apply #'min (mapcar (lambda (set) (mod (- (first (last (butlast set))) (first set)) 12)) first-pass)))
               (second-pass (remove-if (lambda (set) (> (mod (- (first (last (butlast set))) (first set)) 12) min-width)) first-pass)))
          (if (= (length second-pass) 1)
              (first second-pass)
              (alexandria:extremum second-pass #'< :key #'first))))))

(defun n-chords (set n)
  "Return list of chord sets. Trichord: n = 3, tetrachord: n = 4, etc. Return NIL if n > l."
  (let ((l (length set)))
    (cond ((= n l) (permutations set))
          ((> l n) (loop for i below (- l n)
                         for s = (immediate-subsets set) then (alexandria:mappend #'immediate-subsets s)
                         finally (return (remove-duplicate-sets s)))))))

(defun all-transpositions (set &optional (modulus 12))
  (loop for i below modulus collect (transpose set i modulus)))

(defun prime (set &optional (modulus 12))
  (let* ((normal (normal set))
         (normal-transposed (transpose normal (- modulus (first normal))))
         (inversion (invert set modulus))
         (inversion-normal (normal inversion))
         (inversion-normal-transposed (transpose inversion-normal (- modulus (first inversion-normal)))))
    (values normal
            normal-transposed
            inversion
            inversion-normal
            inversion-normal-transposed)))