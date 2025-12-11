(defun load-file-into-list (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil nil)
	  while line
	  collect line)))

(defun load-file-into-array (file)
  (let* ((file-lines (load-file-into-list file))
         (arr (make-array (list (length (car file-lines)) (length file-lines)) 
                          :element-type 'standard-char 
                          :initial-element #\.)))
    (loop for row in file-lines 
          for j from 0 to (1- (length file-lines)) do
          (loop for ch across row 
                for i from 0 to (1- (length row)) do
                  (setf (aref arr i j) ch)))
    arr))
                 
            
(defun parse-rotation-command (srot)
  (let ((dir (subseq srot 0 1))
	(ticks (parse-integer (subseq srot 1))))
    (values dir ticks)))

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun quantize-rotations-helper (fn pos ticks)
  (if (eql ticks 0)
      nil
      (let ((newpos (mod (funcall fn pos 1) 100))
	    (newticks (1- ticks)))
	(cons newpos (quantize-rotations-helper fn newpos newticks)))))

(defun quantize-rotations (pos srot)
  (multiple-value-bind (dir ticks)
      (parse-rotation-command srot)
    (quantize-rotations-helper (if (string= dir "L") #'- #'+) pos ticks)))

(defun process-rotations (pos rotations)
  (if (null rotations)
      nil
      (let* ((subrots (quantize-rotations pos (car rotations)))
	     (nextpos (car (last subrots))))
	(cons subrots (process-rotations nextpos (cdr rotations))))))
    
(defun aoc1-part2 ()
  (let* ((rots (load-file-into-list "~/repos/aoc2025/day1.txt"))
	 (poss (flatten (process-rotations 50 rots)))
	 (c 0))
    (loop for n in poss
	  do (when (eql n 0) (incf c)))
    c))

	 

(defun valid-id-p (id)
  (let* ((sid (write-to-string id))
	 (len (length sid)))
    (if (oddp len)
	t
	(not (string= (subseq sid 0 (/ len 2))
		      (subseq sid (/ len 2)))))))

(defun equal-parts (id parts)
  (let* ((sid (write-to-string id))
         (len (length sid))
         (chunk (/ len parts)))
    (cond ((not (eql (mod len parts) 0))  nil)
          (t (loop for i from 0 to (1- parts)
                   collect (subseq sid (* i chunk) (+ (* i chunk) chunk)))))))


(defun valid-id-p (id)
  (let* ((sid (write-to-string id))
	 (len (length sid)))
    (loop for i from 2 to len
          for parts = (equal-parts id i) then (equal-parts id i)
          when (and (not (null parts)) (every #'string= parts (cdr parts)))
            return nil
          finally (return t))))
                
        
    
(defun process-startend (startend)
  (loop for i from (parse-integer (first startend)) to (parse-integer (second startend))
          when (not (valid-id-p i)) collect i))


(defun process-ranges (ranges)
  (when (null ranges) (return-from process-ranges nil))
  (let* ((curr (car ranges))
         (startend (uiop:split-string curr :separator "-")))
    (append (process-startend startend) (process-ranges (cdr ranges)))))

(defun day2-part1 (ids)
  (apply #'+ (process-ranges (split-string ids :separator ","))))

(defun max-digit (seq &key skip-last)
  (let ((first-max 0)
        (idx 0))
    (loop for i from 0 to (- (length seq) (if skip-last (1+ skip-last) 1))
          for curr = (parse-integer (subseq seq i (1+ i))) then (parse-integer (subseq seq i (1+ i)))
            when (> curr first-max)
            do (setf first-max curr)
               (setf idx (1+ i)))
    (values first-max idx)))

(defun largest-joltage (seq)
  (multiple-value-bind (first-max idx) (max-digit seq :skip-last t)
    (let ((second-max (max-digit (subseq seq idx))))
      (+ (* first-max 10) second-max))))

(defun largest-joltage-big (seq)
  (let ((total 0)
        (seqidx 0))
    (loop for i from 11 downto 0
          do 
            (multiple-value-bind (curr idx) (max-digit (subseq seq seqidx) :skip-last i)
              (setf total (+ (* total 10) curr))
              (incf seqidx idx)))
    total))
            


(defun day3-part1 ()
  (let ((joltages (load-file-into-list "~/repos/aoc2025/day3.txt")))
    (apply #'+ (mapcar #'largest-joltage joltages))))

         
(defun day3-part2 ()
  (let ((joltages (load-file-into-list "~/repos/aoc2025/day3.txt")))
    (apply #'+ (mapcar #'largest-joltage-big joltages))))

(defun check-paper-roll (m i j)
  (let ((dims (array-dimensions m)))
    (if (eql #\. (aref m i j)) 
        nil
      (> 4 (loop with cnt = 0 
                 for y from -1 to 1 do
                   (loop for x from -1 to 1 do
                           (unless (or (< (+ i x) 0) 
                                       (>= (+ i x) (car dims))
                                       (< (+ j y) 0)
                                       (>= (+ j y) (cadr dims))
                                       (and (= x 0) (= y 0)))
                             (when (eql (aref m (+ i x) (+ j y)) #\@) (incf cnt))))
                 finally (return cnt))))))
                          


(defun accessible-paper-rolls (paper-rolls)
  (let ((total 0))
    (loop for y from 0 to (1- (cadr (array-dimensions paper-rolls))) do
          (loop for x from 0 to (1- (car (array-dimensions paper-rolls))) do
                (when (check-paper-roll paper-rolls x y)
                  (incf total)
                  (setf (aref paper-rolls x y) #\.))))
    total))
  

(defun day4-part1 ()
  (let ((paper-rolls (load-file-into-array "~/repos/aoc2025/day4.txt")))
    (accessible-paper-rolls paper-rolls)))


(defun day4-part2 ()
  (let ((paper-rolls (load-file-into-array "~/repos/aoc2025/day4.txt"))
        (total 0))
    (loop for curr = (accessible-paper-rolls paper-rolls) do
            (incf total curr)
            (when (= curr 0) (return total)))))
          

(defun load-ingredient-file (file)
  (with-open-file (in file)
    (let ((fresh 
           (loop for line = (read-line in nil nil)
                 while (not (string= "" line))
                 collect line))
          (ingredients
           (loop for line = (read-line in nil nil)
                 while line
                 collect line)))           
      (values fresh ingredients))))


(defun fresh-range (range)
  (let* ((range-values (uiop:split-string range :separator "-"))
         (start (parse-integer (car range-values)))
         (end (parse-integer (cadr range-values))))
    (list start end)))

(defun make-fresh-list (fresh-list)
  (loop for range in fresh-list 
        collect (fresh-range range)))
            
(defun check-ingredient (fresh-list ingredient)
  (loop for range in fresh-list do
        (when (and (>= ingredient (car range))
                   (<= ingredient (cadr range)))
          (return t))))

(defun range-within-range-p (old-range new-range)
  (let ((old-start (first old-range))
	(old-end (second old-range))
	(new-start (first new-range))
	(new-end (second new-range)))
    (cond ((< new-end old-start) -1)
	  ((> new-start old-end) 1)
	  (t 0))))

(defun merge-ranges (old-range new-range)
  (let ((old-start (first old-range))
	(old-end (second old-range))
	(new-start (first new-range))
	(new-end (second new-range)))
    (list (min old-start new-start) (max old-end new-end))))

(defun add-ingredient-range (ingredient-ranges ingredient-range)
  (if (null ingredient-ranges)
      (list ingredient-range)
      (let* ((curr (first ingredient-ranges))
	     (within-range (range-within-range-p curr ingredient-range)))
	(cond ((< within-range 0) (cons ingredient-range ingredient-ranges))
	      ((= within-range 0)
	       (add-ingredient-range (rest ingredient-ranges) (merge-ranges curr ingredient-range)))
	      (t (cons curr (add-ingredient-range (rest ingredient-ranges) ingredient-range)))))))

(defun check-ingredient-list (fresh-list ingredient-list)
  (loop with fresh = 0
        for ingredient in ingredient-list do
        (when (check-ingredient fresh-list (parse-integer ingredient))
          (incf fresh))
        finally (return fresh)))

(defun sum-range (range)
  (1+ (- (second range) (first range))))

(defun day5-part1 ()
  (multiple-value-bind (fresh-list ingredients)
      (load-ingredient-file "~/repos/aoc2025/day5.txt")
    (check-ingredient-list (make-fresh-list fresh-list) ingredients)))

(defun day5-part2 ()
  (let* ((fresh-list (load-ingredient-file "~/repos/aoc2025/day5.txt"))
	 (range-list
	   (loop with ranges = nil
		 for range in fresh-list do
		   (setf ranges (add-ingredient-range ranges (fresh-range range)))
		 finally (return ranges))))
    (apply #'+ (mapcar #'sum-range range-list))))
	  

