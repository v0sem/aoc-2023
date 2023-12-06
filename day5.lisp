(defvar *input*
  (uiop:read-file-lines "inputs/day5.txt"))

(defun get-range-as-list (num range)
  (loop for x from num to (- (+ num range) 1)
	collect x))

(defun parse-line (str)
  (mapcar #'parse-integer (uiop:split-string str)))

(defun get-maps (line)
  (let ((source (get-range-as-list (nth 1 line) (nth 2 line)))
	(destination (get-range-as-list (car line) (nth 2 line))))
    (mapcar #'list source destination)))

(defun seed-to-soil-map ()
  (loop for x from 3 to 49
	append (get-maps (parse-line (nth x *input*)))))
