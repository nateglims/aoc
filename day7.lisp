(defparameter *input* (uiop:read-file-lines "day7.input"))

(defparameter *directories* nil)

;; Take a size (number) and add it to all parent directories in the list.
(defun apply-size (size dirs)
  (mapcar #'(lambda (d) (cons (car d) (+ (cdr d) size))) dirs))

;;
(defun create-dir-name (cd dirs)
  (cons (concatenate 'string cd "/")
	(mapcar #'car (mapcar #'car dirs))))

(defun parse-lines (lines)
  (let ((dirs nil))
    (dolist (line lines)
      (let ((words (uiop:split-string line)))
	(cond
	  ;; Moving Up
	  ((and (equal "cd" (second words))
		(equal ".." (third words))) (push (pop dirs) *directories*))
	  ;; Moving Down
	  ((equal "cd" (second words)) (push (cons
					      (create-dir-name (third words) dirs) 0) dirs))
	  ;; It's a file
	  ((every #'digit-char-p (first words)) (setf dirs (apply-size (parse-integer (first words)) dirs)))
	  ;; Ignore other entires.
	  )))
    (dolist (dir dirs)
      (print dir)
      (push dir *directories*))))

(defun part-1-check-dir (d)
  (when (< (cdr d) 100000)
    (push d *directories*)))

;; Not used, might profile later.
(defun apply-size-destructive (size dirs)
  (dolist (dir dirs)
    (setf (cdr dir) (+ size (cdr dir))))
  dirs)

;; Part 1
(print (reverse (parse-lines *input*)))

(print
 (reduce
  #'(lambda (acc n) (+ (cdr n) acc))
  (remove-if-not #'(lambda (d) (< (cdr d) 100000)) *directories*)
  :initial-value 0))

;; Part 2
(let ((target (-
	       (cdr (first *directories*))
	       (- 70000000 30000000))))
  (print (reduce #'min (remove-if-not #'(lambda (ds) (> ds target)) (cdr (mapcar #'cdr *directories*))))))


