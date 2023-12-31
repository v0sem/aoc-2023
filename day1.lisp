(defun input ()
  (uiop:read-file-lines "inputs/day1.txt"))

(defun numbers-in-input (s)
  (remove nil
	  (loop for c across (num-seq (length s))
		collect (string-to-num (subseq s c)))))

(defun special-num (l)
  (+ (* 10 (car l)) (car (reverse l))))

(defun day1-1 (values)
  (apply '+
  (mapcar
   (lambda (x) (special-num (numbers-in-input x))) values)))

(defun string-to-num (s)
  (if (parse-integer (subseq s 0 1) :junk-allowed T)
      (parse-integer (subseq s 0 1))
      (car (remove nil (loop for num in (numssss)
			     collect (if (not (> (length num) (length s)))
			     (if (string= (subseq s 0 (length num)) num)
			   (position num (numssss) :test #'string=))))))))

(defun numssss ()
  '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defun sub-string (s sub)
  (if (> (length s) (- (length sub) 1))
      (if (string= (subseq s 0 (length sub)) sub)
      (string-to-num sub)
      (sub-string (subseq s 1) sub))))

(defun num-seq (l)
  (loop for n below l collect n))
