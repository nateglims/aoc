(defparameter *day4-input* (uiop:read-file-lines "day4.input"))


(defun overlap-fully-p (p)
  (let ((elf-1 (first p))
	(elf-2 (second p)))
    (cond
      ((and (<= (car elf-1) (car elf-2))
	    (>= (cdr elf-1) (cdr elf-2)))
       t)
      ((and (>= (car elf-1) (car elf-2))
	    (<= (cdr elf-1) (cdr elf-2)))
       t)
      (t '()))))

(defun overlap-p (p)
  (let ((elf-1 (first p))
	(elf-2 (second p)))
    (or (and (<= (car elf-2) (car elf-1)) (>= (cdr elf-2) (car elf-1)))
	(and (< (car elf-1) (car elf-2)) (>= (cdr elf-1) (car elf-2))))))


(defun parse-line (line)
  (let ((idx (search '(#\,) line)))
    (mapcar #'parse-pair (list (subseq line 0 idx) (subseq line (+ idx 1))))))

(defun parse-pair (pair)
  (let ((idx (search '(#\-) pair)))
    (cons (parse-integer (subseq pair 0 idx))
	  (parse-integer (subseq pair (+ idx 1))))))

(length (remove-if-not #'overlap-fully-p (mapcar #'parse-line *day4-input*)))
(length (remove-if-not #'overlap-p (mapcar #'parse-line *day4-input*)))
