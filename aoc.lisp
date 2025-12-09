(defun load-rotation-file (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil nil)
	  while line
	  collect line)))

(defun rot- (pos rot)
  (mod (- pos rot) 100))

(defun rot+ (pos rot)
  (mod (+ pos rot) 100))

(defun parse-rotation-command (pos srot)
  (let ((dir (subseq srot 0 1))
	(ticks (parse-integer (subseq srot 1))))
    (cond ((string= dir "L") (rot- pos ticks))
	  ((string= dir "R") (rot+ pos ticks))
	  (t nil))))

(defun main ()
  (let ((rots (load-rotation-file "day1.txt"))
	(pos 50)
	(zeros 0))
    (mapcar (lambda (val)
	      ((mapcar (lambda (srot)
	      (let ((newpos (parse-rotation-command pos srot)))
		(setf pos newpos)))
	    rots)))

