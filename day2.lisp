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

