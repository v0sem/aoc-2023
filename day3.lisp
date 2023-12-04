(defvar *input*
  (uiop:read-file-lines "inputs/day3.txt"))

(defun num-len (num)
  (1+ (floor (log num 10))))

(defun char=. (c) (char= #\. c))

(defun get-adj-pos (x y)
  (loop for i from -1 to 1
       append (loop for j from -1 to 1
		    when (and
			  (not (and
				(= (+ x i) x)
				(= (+ y j) y)))
			  (not (or
				(< (+ x i) 0)
				(< (+ y j) 0)))
			  (not (or
				(> (+ x i) 139)
				(> (+ y j) 139))))
	       collect (list (+ x i) (+ y j)))))

(defun get-all-adj-pos (x y len)
  (remove-duplicates (loop for i from 0 to (- len 1)
			   append (get-adj-pos x (+ y i)))
		     :test #'equal))

(defun clean-adj (x y len)
  (loop for value in (get-all-adj-pos x y len)
	when (not (loop for i from 0 to (- len 1)
		   when (equal value (list x (+ y i)))
		     return T))
	  collect value))

(defun get-char (x y)
  (char (nth x *input*) y))

(defun get-number (x y)
  (let* ((input-str (nth x *input*))
         (substring (subseq input-str y))
         (int (parse-integer substring :junk-allowed t)))
    (if (and int (< int 0))
        nil
        int)))


(defun is-not-part (x y)
  (every #'char=.
	       (mapcar (lambda (item) (get-char (car item) (nth 1 item)))
		       (clean-adj x y (num-len (get-number x y))))))

(defun get-positions-in-line (x)
  (loop for y from 0 to 139
	when (and (get-number x y)
		  (not(ignore-errors (get-number x (- y 1)))))
	  collect (list x y)))

(defun get-all-pos ()
  (loop for x from 0 to 139
	append (get-positions-in-line x)))

(defun day3-1 ()
  (apply #'+
  (loop for (x y) in (get-all-pos)
	when (not (is-not-part x y))
	  collect (get-number x y))))
