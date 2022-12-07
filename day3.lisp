(defun part-1 (filename)
  (let ((score 0))
    (dolist (line (uiop:read-file-lines filename))
      (print line)
      (incf score (car (mapcar #'prioritize (diff-string (split-in-half line))))))
    score))


(defun part-2 (lst)
  (if (null lst)
      0
      (let* ((two-elves (diff-string (cons (first lst) (second lst))))
	     (three-elves (diff-string (cons (third lst) two-elves)))
	     (score (prioritize (car three-elves))))
	(+ (part-2 (nthcdr 3 lst)) score))))

(defun split-in-half (string)
  (let ((len (length string)))
    (cons (subseq string 0 (/ len 2))
	  (subseq string (/ len 2)))))

(defun diff-string (strs)
  (let ((str1 (coerce (car strs) 'list))
	(str2 (coerce (cdr strs) 'list)))
    (remove-if-not
     (lambda (chr)
       (find chr str2))
     str1)))

(defun prioritize (chr)
  (let ((chr-int (char-int chr)))
    (cond ((and (>= chr-int (char-int #\a))
		(<= chr-int (char-int #\z))) (- chr-int 96))
	  (t (- chr-int 38)))))

(part-1 "day3.input")

(part-2 (uiop:read-file-lines "day3.input"))
