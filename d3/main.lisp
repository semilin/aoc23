(defpackage :aoc
  (:use :cl :alexandria :iterate :ut)
  (:import-from :serapeum #:~> #:~>>)
  (:local-nicknames (#:spm #:serapeum)))
(in-package :aoc)

(declaim (optimize (debug 3)))

(defparameter *input*
  (spm:lines "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"))

(defparameter *input* (spm:lines "....................................18..........889.270.....748.280...997.................617..............622........763............476....
...529......434.....191..489...717...@.....................&....................939*7.....*....................606............760....*......"))

(defparameter *input*
  (uiop:read-file-lines "input"))

(defun row-matches (groups start end)
  (remove-if-not (lambda (group)
		   (and (eql t (cadar group))
			(>= (caar group)
			    (1- start))
			(<= (caar group)
			    (1+ end))))
		 groups))

;; part 1
(let* ((grid (mapcar (lambda (l) (coerce l 'list)) *input*))
       (grid (iterate
	       (for line in grid)
	       (for row = (iterate
			    (for c in line)
			    (for i upfrom 0)
			    (let ((digit (digit-char-p c)))
			      (if digit
				  (collect (list i digit))
				  (if (not (eql c #\.))
				      (collect (list i t)))))))
	       (collect (spm:runs row :test (lambda (a b)
					      (and (typep (second a) 'integer)
						   (typep (second b) 'integer)
						   (> 3 (- (car b) (car a))))))))))
  (~>> (iterate top
	 (for runs in grid)
	 (for y upfrom 0)
	 
	 (iterate
	   (for run in runs)
	   (for i upfrom 0)
	   (if (typep (cadar run) 'integer)
	       (progn
		 (for num = (apply #'+
				   (iter
				     (for (index digit) in run)
				     (for exp downfrom (1- (length run)))
				     (collect (* digit (expt 10 exp))))))
		 (for start = (car (first run)))
		 (for end = (caar (last run)))
		 (print num)
		 (if (or (and (> i 0)
			      (eql t (cadar (nth (1- i) runs)))
			      (>= (caar (nth (1- i) runs))
				  (1- start))
			      (print "left"))
			 (and (< i (1- (length runs)))
			      (eql t (cadar (nth (1+ i) runs)))
			      (<= (caar (nth (1+ i) runs))
				  (1+ end))
			      (print "right"))
			 (and (> y 0)
			      (row-matches (nth (1- y) grid) start end)
			      (print "up"))
			 (and (< y (1- (length grid)))
			      (row-matches (nth (1+ y) grid) start end)
			      (print "down")))
		     (in top (collect num))))))
	 )
       (print)
       (apply #'+)
       (print)))

(defun groups-adjacent (groups index)
  (~>> groups
       (remove-if-not (lambda (group)
			(and (typep (cadr group) 'integer)
			     (let ((start (caar group))
				   (end (cadar group)))
			       (and (>= index
					(1- start))
				    (<= index
					(1+ end)))))))
       (mapcar (lambda (group) (cadr group)))))

(let* ((grid (mapcar (lambda (l) (coerce l 'list)) *input*))
       (grid (iterate
	       (for line in grid)
	       (for row = (iterate
			    (for c in line)
			    (for i upfrom 0)
			    (let ((digit (digit-char-p c)))
			      (if digit
				  (collect (list i digit))
				  (if (eql c #\*)
				      (collect (list i c)))))))
	       (for runs = (spm:runs row :test
				     (lambda (a b)
				       (and (typep (second a) 'integer)
					    (typep (second b) 'integer)
					    (> 3 (- (car b) (car a)))))))
	       (for nums = (iter
			     (for run in runs)
			     (if (typep (cadar run) 'integer)
				 (collect
				     (let* ((res (iter
						   (for (index digit) in run)
						   (for exp downfrom (1- (length run)))
						   (collect (list index (* digit (expt 10 exp))))))
					    (indexes (mapcar #'car res)))
				       (list (list (apply #'min indexes)
						   (apply #'max indexes))
					     (apply #'+ (mapcar #'cadr res)))))
				 (collect run))
			     ))
	       (collect (if nums
			    nums
			    runs)))))
  (~>> (iterate top
	 (for runs in grid)
	 (for y upfrom 0)
	 (print runs)
	 (iterate
	   (for run in runs)
	   (for i upfrom 0)
	   (if (not (eql #\* (cadar run)))
	       (next-iteration))
	   (for index = (caar run))
	   (print "tests")
	   (let ((adjacent
		   (flatten (mapcar (lambda (group)
				      (groups-adjacent group index))
				    (remove-if (lambda (group)
						 (or (null group)
						     (null (car group))))
					       (list (if (> i 0) (list (nth (1- i) runs)))
						     (if (< i (1+ (length runs))) (list (nth (1+ i) runs)))
						     (if (> y 0) (nth (1- y) grid))
						     (if (< y (1+ (length grid))) (nth (1+ y) grid))))))))
	     (if (= 2 (length adjacent))
		 (in top (collect (apply #'* adjacent)))))
	   )
	 )
       (print)
       (apply #'+)
       (print)
       ))
