(defpackage :aoc
  (:use :cl :alexandria :iterate)
  (:import-from :serapeum #:~> #:~>>))
(in-package :aoc)

(declaim (optimize (speed 3)))

(defparameter *input*
  (uiop:read-file-lines "input"))

(defun part1 ()
  (~>> *input*
       (mapcar
	(lambda (l)
	  (~>> l
	       (coerce _ 'list)
	       (serapeum:filter-map
		(lambda (c)
		  (digit-char-p c)))
	       ((lambda (i)
		  (+ (* 10 (first i))
		     (car (last i))))))))
       (apply #'+)))

(defparameter *table*
  (serapeum:dict "one" 1
		 "two" 2
		 "three" 3
		 "four" 4
		 "five" 5
		 "six" 6
		 "seven" 7
		 "eight" 8
		 "nine" 9))

;; this function is absolutely horrendous. This is because I was too
;; frustrated from not being able to figure out how to get cl-ppcre to
;; properly report capture groups with a positive lookahead to write
;; actually good code for this fallback solution.
(defun collect-matches (s)
  (let ((numbers (hash-table-keys *table*)))
    (iter top
      (for c in-string s)
      (for i upfrom 0)
      (let ((digit (digit-char-p c)))
	(if digit (collect digit)))
      (let ((potentials (iter
			  (for num in numbers)
			  (for c1 = (aref num 0))
			  (if (char-equal c1 c)
			      (collect num)))))
	(if potentials
	    (iter
	      (for j upfrom (1+ i))
	      (for k upfrom 1)
	      (for sc = (if (< j (length s))
			    (aref s j)
			    (leave)))
	      (setf potentials
		    (remove-if-not
		     (lambda (pot)
		       (char-equal sc (aref pot k)))
		     potentials))
	      (if (null potentials)
		  (leave)
		  (if (and (= 1 (length potentials))
			   (= (1+ k) (length (car potentials))))
		      (progn (in top
				 (collect (gethash (car potentials)
						   *table*)))
			     (leave))))))))))

(defun part2 ()
  (~>> *input*
       (mapcar
	(lambda (l)
	  (~>> l
	       (collect-matches)
	       ((lambda (i)
		  (+ (* 10 (first i))
		     (car (last i))))))))
       (apply #'+)))
