(defpackage :aoc
  (:use :cl :alexandria :iterate)
  (:import-from :serapeum #:~> #:~>>))

(defparameter *input*
  (uiop:read-file-lines "input"))

;;; sample input
;; (defparameter *input*
;;   (str:lines "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
;; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
;; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
;; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
;; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

;; part 1
(let ((max-counts (serapeum:dict "red" 12 "green" 13 "blue" 14)))
  (~>> *input*
       (mapcar (lambda (line)
		 (let* ((words (str:words line))
			(game-id (parse-integer (car (str:split ":" (second words)))))
			(games (~>> (str:split ";" (str:unwords (subseq words 2)))
				    (mapcar (lambda (g)
					      (~>> g (str:split ",")
						   (mapcar (lambda (pair)
							     (let ((pair (str:words pair)))
							       (list (parse-integer (first pair))
								     (second pair))))))))))
			(impossible nil))

		   (iterate top (for game in games)
		     (iterate (for (number color) in game)
		       (if (> number (gethash color max-counts))
			   (progn (setf impossible t)
				  (in top (leave))))))
		   (if (not impossible)
		       game-id
		       0))))
       (apply #'+)
       (print)))

;; part 2
(~>> *input*
     (mapcar (lambda (line)
	       (let* ((words (str:words line))
		      (games (~>> (str:split ";" (str:unwords (subseq words 2)))
				  (mapcar (lambda (g)
					    (~>> g (str:split ",")
						 (mapcar (lambda (pair)
							   (let ((pair (str:words pair)))
							     (list (parse-integer (car pair))
								   (second pair))))))))))
		      (greatest (serapeum:dict "red" 0 "green" 0 "blue" 0)))

		 (iterate top (for game in games)
		   (iterate
		     (for (number color) in game)
		     (let ((color-count (gethash color greatest)))
		       (if (> number color-count)
			   (setf (gethash color greatest) number)))))
		 (apply #'* (hash-table-values greatest)))))
     (apply #'+)
     (print))

