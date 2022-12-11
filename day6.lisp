(defparameter *input* (uiop:read-file-lines "day6.input"))

(defun detect-n-uniq (sig n)
  (dotimes (i (- (length sig) 4))
    (let ((l (remove-duplicates
	      (coerce (subseq sig i (+ i n)) 'list))))
      (when (= (length l) n)
	(print (+ n i))
	(return (+ n i))))))

;; Part 1
(detect-n-uniq "bvwbjplbgvbhsrlpgdmjqwftvncz" 4)
(detect-n-uniq (car *input*) 4)

;; Part 2
(detect-n-uniq "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)
(detect-n-uniq (car *input*) 14)
