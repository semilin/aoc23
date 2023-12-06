(defpackage :aoc
  (:use :cl :alexandria :iterate :ut)
  (:import-from :serapeum #:~> #:~>>)
  (:local-nicknames (#:spm #:serapeum)))
(in-package :aoc)

(declaim (optimize (safety 3)))

(defparameter *input*
  (the list (uiop:read-file-lines "input")))

(defparameter *input*
  (spm:lines "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"))

;; part 1
(print
 (iter top
   (for seed in (~>> *input* first (subseq _ 7) spm:words (spm:filter-map #'parse-integer)))
   (iter 
     (for map in (spm:split-sequence-if
		  (lambda (l)
		    (or (= (length l) 0)
			(not (digit-char-p (aref l 0)))))
		  *input* :start 1 :remove-empty-subseqs t))
     (iter
       (for line in map)
       (for (dest src len) = (~>> line (spm:words) (mapcar #'parse-integer)))
       (if (and (>= seed src)
		(<= seed (+ src len -1)))
	   (progn
	     (setf seed (+ dest (- seed src)))
	     (leave)))))
   (finding seed minimizing seed)))

(declaim (optimize (speed 3)))

;; part 2
(defun run ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((maps
	  (iter top
	    (for map in (spm:split-sequence-if (lambda (l)
						 (or (= (length (the simple-string l)) 0)
						     (not (digit-char-p (aref l 0)))))
					       *input* :start 1 :remove-empty-subseqs t))
	    (collect (iter
		       (for line in map)
		       (collect (~>> line (spm:words) (mapcar #'parse-integer)))))))
	(least most-positive-fixnum))
    (print maps)
    (iter
      (for ((the fixnum start) (the fixnum len))
	   in (~>> *input* first (the simple-string) (subseq _ 7)
		   spm:words (spm:filter-map #'parse-integer) (spm:batches _ 2)))
      (format t "~A~%" (list start len))
      (declare (type fixnum start len))
      (iter
	(for original-seed from start to (1- (the fixnum (+ start len))))
	(for seed = original-seed)
	(declare (type fixnum original-seed seed))
	(if (= 0 (mod seed 100000000))
	    (format t "seed ~A~%" seed))
	(iter
	  (for map in maps)
	  (iter
	    (for (dest src len) in map)
	    (declare (type fixnum dest src len))
	    (if (and (>= seed src)
		     (<= seed (1- (the fixnum (+ src len)))))
		(progn
		  (setf seed (+ dest (- seed src)))
		  (leave)))))
	(if (< seed least)
	    (setf least seed))))
    (print least)
    ))
