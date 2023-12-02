(defpackage :aoc
  (:use :cl :alexandria :iterate)
  (:import-from :serapeum #:~> #:~>>))

(defparameter *input*
  (uiop:read-file-lines "input"))

(defparameter *input*
  (str:lines "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

;; part 1
(~>> *input*
     (mapcar (lambda (line)
	       (let* ((words (str:words line))
		      (game-id (parse-integer (car (str:split ":" (second words)))))
		      (games (mapcar (lambda (g)
				       (~>> g (str:split ",")
					    (mapcar (lambda (pair)
						      (let ((pair (str:words pair)))
							(list (parse-integer (car pair))
							      (second pair)))))))
				     (str:split ";" (str:unwords (subseq words 2)))))
		      (impossible nil))

		 (print game-id)
		 (print games)
		 (iterate top (for game in games)
		   ;; (print game)
		   (iterate (for (number color) in game)
		     (if (or (and (string-equal "red" color)
				  (> number 12))
			     (and (string-equal "green" color)
				  (> number 13))
			     (and (string-equal "blue" color)
				  (> number 14)))
			 (progn (setf impossible t)
				(print (list number color))
				(leave)))))
		 (print impossible)
		 (if (not impossible)
		     game-id
		     0))))
     (print)
     (apply #'+)
     (print))

;; part 2
(~>> *input*
     (mapcar (lambda (line)
	       (let* ((words (str:words line))
		      (game-id (parse-integer (car (str:split ":" (second words)))))
		      (games (mapcar (lambda (g)
				       (~>> g (str:split ",")
					    (mapcar (lambda (pair)
						      (let ((pair (str:words pair)))
							(list (parse-integer (car pair))
							      (second pair)))))))
				     (str:split ";" (str:unwords (subseq words 2)))))
		      (greatest (vector 0 0 0)))

		 (print game-id)
		 (print games)
		 (iterate top (for game in games)
		   ;; (print game)
		   (iterate
		     (for (number color) in game)
		     (cond ((string-equal color "red") (if (> number (aref greatest 0))
							   (setf (aref greatest 0) number)))
			   ((string-equal color "green") (if (> number (aref greatest 1))
							     (setf (aref greatest 1) number)))
			   ((string-equal color "blue") (if (> number (aref greatest 2))
							    (setf (aref greatest 2) number))))))
		 (print greatest)
		 (print (apply #'* (coerce greatest 'list)))
		 (apply #'* (coerce greatest 'list)))))
     (print)
     (apply #'+)
     (print))
