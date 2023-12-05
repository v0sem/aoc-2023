(defvar *input*
  (uiop:read-file-lines "inputs/day4.txt"))

(defun get-game (gid)
  (subseq (nth (- gid 1) *input*) 10))

(defun win-count (winning have)
  (apply #'+ (loop for number in have
		   when (position number winning)
		     collect 1)))

(defun get-winning (gid)
  (mapcar #'parse-integer
	  (remove "" (uiop:split-string
	   (subseq (get-game gid) 0 29)) :test #'string=)))

(defun get-numbers (gid)
  (mapcar #'parse-integer
	  (remove "" (uiop:split-string
		      (subseq (get-game gid) 32)) :test #'string=)))

(defun day4-1 ()
  (apply #'+
  (loop for gid from 1 to 209
	when (not (= 0 (win-count (get-winning gid) (get-numbers gid))))
	collect (expt 2 (- (win-count (get-winning gid) (get-numbers gid)) 1)))))

