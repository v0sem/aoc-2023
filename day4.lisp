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

(defun card+ (card-a num)
  (list (car card-a) (+ (nth 1 card-a) num)))

(defun init-list ()
  (loop for gid from 1 to 209
	collect (list gid 1)))

(defun process-list ()
  (let ((l (init-list)))
    (loop for gid from 1 to 209
	  do (loop for gid2 from gid to (+ (win-count (get-winning gid) (get-numbers gid)) (- gid 1))
					do (setf (nth gid2 l)
						 (card+ (nth gid2 l) (nth 1 (nth (- gid 1) l))))))
    l))

(defun day4-2 ()
  (apply #'+ (mapcar (lambda (x) (nth 1 x)) (process-list))))
