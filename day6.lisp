(defvar *input*
  (uiop:read-file-lines "inputs/day6.txt"))

(defun get-times ()
  (loop for number in (uiop:split-string (car *input*))
	when (parse-integer number :junk-allowed T)
	  collect (parse-integer number)))

(defun get-distance ()
  (loop for number in (uiop:split-string (cadr *input*))
	when (parse-integer number :junk-allowed T)
	  collect (parse-integer number)))

(defun calc-distance (hodl max-time)
  (* hodl (- max-time hodl)))

(defun beaters (max-time min-distance)
  (loop for hodl from 0 to max-time
	when (> (calc-distance hodl max-time) min-distance)
	  collect hodl))

(defun day6-1 ()
  (apply #'* (loop for (time distance) in (mapcar #'list (get-times) (get-distance))
		   collect (length (beaters time distance)))))

(defun gtime ()
  (parse-integer (concatString (mapcar #'write-to-string (get-times)))))

(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

(defun gdistance ()
  (parse-integer (concatString (mapcar #'write-to-string (get-distance)))))

(defun day6-2 ()
  (length (beaters (gtime) (gdistance))))
