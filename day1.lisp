;; N-CALORIES
(defun n-calories (filename n)
  (let ((maxcals '())
	(acc 0))
    (dolist (line (uiop:read-file-lines filename))
      (if (or (null line) (= 0 (length line)))
	  (progn
	    (setq maxcals (insert acc maxcals))
	    (setq acc 0))
	  (incf acc (parse-integer line))))
    (subseq  maxcals 0 n)))

(defun insert (item lst)
  (cond ((null lst) (list item))
	((> item (car lst)) (cons item lst))
	(t (cons (car lst) (insert item (cdr lst))))))

(n-calories "sample.input" 3)

;; Part 1
(n-calories "input" 1)

;; Part 2
(reduce
 (lambda (n acc)
   (+ acc n))
 (n-calories "input" 3))
