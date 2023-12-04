(defvar *input*
  (uiop:read-file-lines "inputs/day2.txt"))

(defun get-game (gid)
  (subseq (nth (- gid 1) *input*) (+ 7 (floor (log gid 10)))))

(defun get-iterations (gid)
    (loop for string in (uiop:split-string (get-game gid) :separator ";")
	  collect (uiop:split-string string :separator ",")))

(defun clean-ball (ball)
  (list (parse-integer (car ball)) (nth 1 ball)))

(defun clean-iterations (gid)
  (loop for it in (get-iterations gid) collect (loop for i in it collect (clean-ball (cdr (uiop:split-string i))))))

(defun is-valid (gid)
  (loop for iteration in (clean-iterations gid)
	when (loop for ball in iteration
		   when (or (and (< 12 (car ball)) (string= "red" (nth 1 ball)))
			    (and (< 13 (car ball)) (string= "green" (nth 1 ball)))
			    (and (< 14 (car ball)) (string= "blue" (nth 1 ball))))
		     return T)
	  return NIL finally (return T)))

(defun get-balls-by-color (gid color)
  (loop for iteration in (clean-iterations gid)
	collect (loop for ball in iteration
		      when (string= color (nth 1 ball))
			return ball)))

(defun get-biggest-ball-by-color (gid color)
  (loop for ballnum in (mapcar 'car (remove NIL (get-balls-by-color gid color)))
	maximizing ballnum))

(defun day2-1 ()
  (apply '+ (loop for i from 1 to 100 when (is-valid i) collect i)))

(defun day2-2 ()
  (apply '+ (mapcar (lambda (x) (apply '* x)) (loop for i from 1 to 100
			     collect (mapcar (lambda (x) (get-biggest-ball-by-color i x))
					     '("red" "green" "blue"))))))
