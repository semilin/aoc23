(defpackage :aoc
  (:use :cl :alexandria :iterate :ut)
  (:import-from :serapeum #:~> #:~>>)
  (:local-nicknames (#:spm #:serapeum)))
(in-package :aoc)

(defparameter *input*
  (the list (uiop:read-file-lines "input")))

;; part 1 
(let ((races (mapcar #'list
		     (~>> *input* first spm:words (subseq _ 1) (mapcar #'parse-integer))
		     (~>> *input* second spm:words (subseq _ 1) (mapcar #'parse-integer)))))
  (print (apply #'*
		(iter top
		  (for (time distance) in races)
		  (collect (iter
			     (for rate from 1 to time)
			     (for d = (* rate (- time rate)))
			     (counting (>= d distance))))))))

;; part 2
(defun part2 ()
  (declare (optimize (speed 3)))
  (let ((time (~>> *input* first spm:words (subseq _ 1) (spm:string-join _ "") parse-integer))
	(distance (~>> *input* second spm:words (subseq _ 1) (spm:string-join _ "") parse-integer)))
    (print (iter
	     (for rate from 1 to time)
	     (for d = (* rate (- time rate)))
	     (counting (>= d distance))))))
