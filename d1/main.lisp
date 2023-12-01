(defpackage :aoc
  (:use :cl :alexandria :iterate)
  (:import-from :serapeum #:~> #:~>>))
(in-package :aoc)

(defparameter *input*
  (uiop:read-file-lines "input"))

(defun main ()
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

(main)

