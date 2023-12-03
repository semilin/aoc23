(defpackage :aoc
  (:use :cl :alexandria :iterate)
  (:import-from :serapeum #:~> #:~>>))
(in-package :aoc)

;;; sample input
;; (defparameter *input*
;;   (str:lines "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
;; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
;; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
;; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
;; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(defparameter *input*
  (uiop:read-file-lines "input"))

(defun parse-game (str)
  (let* ((words (serapeum:words str))
	 (game-id (parse-integer (second words))))
    (list game-id
	  (~> words
	      (subseq 2)
	      (serapeum:batches 2)
	      (mapcar (lambda (p)
			(list (parse-integer (first p))
			      (second p)))
		      _)))))

;; part 1
(let ((max-counts (serapeum:dict "red" 12 "green" 13 "blue" 14)))
  (~>> *input*
       (mapcar (lambda (line)
		 (let* ((game (parse-game line)))
		   (~> (iterate (for (number color) in (cadr game))
			 (if (> number (gethash color max-counts))
			     (leave t)))
		       (if _
			   0
			   (car game))))))
       (apply #'+)
       (print)))

;; part 2
(~>> *input*
     (mapcar (lambda (line)
	       (let* ((game (parse-game line))
		      (greatest (serapeum:dict "red" 0 "green" 0 "blue" 0)))
		 (iterate
		   (for (number color) in (cadr game))
		   (let ((color-count (gethash color greatest)))
		     (if (> number color-count)
			 (setf (gethash color greatest) number))))
		 (apply #'* (hash-table-values greatest)))))
     (apply #'+)
     (print))

