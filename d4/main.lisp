(defpackage :aoc
  (:use :cl :alexandria :iterate :ut)
  (:import-from :serapeum #:~> #:~>>)
  (:local-nicknames (#:spm #:serapeum)))
(in-package :aoc)

(declaim (optimize (debug 3)))

(defparameter *input*
  (spm:lines "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

(defparameter *input*
  (uiop:read-file-lines "input"))

;; part 1
(~>> *input*
     (mapcar (lambda (line)
	       (let* ((lists (~>> line
				  (subseq _ 10)
				  (spm:split-sequence #\|)))
		      (matches (~>> (mapcar (lambda (nums)
					      (mapcar #'parse-integer (spm:words nums)))
					    lists)
				    (apply #'intersection))))
		 (if matches (expt 2 (1- (length matches))) 0))))
     (apply #'+)
     print)

;; part 2
(let ((copies (make-hash-table)))
  (~>> *input*
       (mapcar (lambda (line)
		 (let* ((card-num (parse-integer (subseq line 5 8)))
			(lists (~>> line
				    (subseq _ 10)
				    (spm:split-sequence #\|)))
			(matches (~>> (mapcar (lambda (nums)
						(mapcar #'parse-integer (spm:words nums)))
					      lists)
				      (apply #'intersection))))
		   (if matches
		       (iter
			 (for i from (1+ card-num) to (+ card-num (length matches)))
			 (format t "inc ~A~%" i)
			 (incf (gethash i copies 1) (gethash card-num copies 1))))
		   (gethash card-num copies 1))))
       (apply #'+)
       print))
