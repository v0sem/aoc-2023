(defvar *input*
  (uiop:read-file-lines "inputs/day5.txt"))

(defvar *input2*
  (uiop:read-file-lines "inputs/test.txt"))

(defun get-range-as-list (num range)
  (loop for x from num to (- (+ num range) 1)
	collect x))

(defun parse-line (str)
  (mapcar #'parse-integer (uiop:split-string str)))

(defun parse-map (num maps)
  (loop for map in maps
	when (and
	      (> (+ (cadr map) (caddr map)) num)
	      (>= num (cadr map)))
	  return (+ (car map) (- num (cadr map)))
	))

(defun seed-to-soil ()
  (loop for x from 3 to 49
	collect (parse-line (nth x *input*))))

(defun soil-to-fert ()
  (loop for x from 52 to 83
	collect (parse-line (nth x *input*))))

(defun fert-to-water ()
  (loop for x from 86 to 95
	collect (parse-line (nth x *input*))))

(defun water-to-light ()
  (loop for x from 98 to 123
	collect (parse-line (nth x *input*))))

(defun light-to-temp ()
  (loop for x from 126 to 151
	collect (parse-line (nth x *input*))))

(defun temp-to-humid ()
  (loop for x from 154 to 193
	collect (parse-line (nth x *input*))))

(defun humid-to-loc ()
  (loop for x from 196 to 236
	collect (parse-line (nth x *input*))))

(defun h2l (num)
  (if (parse-map num (humid-to-loc))
      (parse-map num (humid-to-loc))
      num))

(defun t2l (num)
  (if (parse-map num (temp-to-humid))
      (h2l (parse-map num (temp-to-humid)))
      (h2l num)))

(defun l2l (num)
  (if (parse-map num (light-to-temp))
      (t2l (parse-map num (light-to-temp)))
      (t2l num)))

(defun w2l (num)
  (if (parse-map num (water-to-light))
      (l2l (parse-map num (water-to-light)))
      (l2l num)))

(defun f2l (num)
  (if (parse-map num (fert-to-water))
      (w2l (parse-map num (fert-to-water)))
      (w2l num)))

(defun s2l (num)
  (if (parse-map num (soil-to-fert))
      (f2l (parse-map num (soil-to-fert)))
      (f2l num)))

(defun seed2loc(num)
  (if (parse-map num (seed-to-soil))
      (s2l (parse-map num (seed-to-soil)))
      (s2l num)))

(defun get-seeds ()
  (mapcar #'parse-integer (uiop:split-string (subseq (car *input*) 7))))

(defun day5-1 ()
  (loop for seed in (get-seeds)
	minimize (seed2loc seed)))

(defun get-seeds-for-real ()
  (loop for seed from 0 to 9
	append (get-range-as-list (nth (* 2 seed) (get-seeds)) (nth (+ (* 2 seed) 1) (get-seeds)))))
