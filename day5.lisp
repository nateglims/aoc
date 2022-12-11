(defparameter *input* (uiop:read-file-lines "day5.input"))


(defun create-stacks ()
    (make-array 10
	     :initial-contents '(()
				 (#\F #\H #\M #\T #\V #\L #\D)
				 (#\P #\N #\T #\C #\J #\G #\Q #\H)
				 (#\H #\P #\M #\D #\S #\R)
				 (#\F #\V #\B #\L)
				 (#\Q #\L #\G #\H #\N)
				 (#\P #\M #\R #\G #\D #\B #\W)
				 (#\Q #\L #\H #\C #\R #\N #\M #\G)
				 (#\W #\L #\C)
				 (#\T #\M #\Z #\J #\Q #\L #\D #\R))))


(defun parse-line (l)
  (let ((words (str:words l)))
    (list
     (parse-integer (second words))
     (parse-integer (fourth words))
     (parse-integer (sixth words)))))

(defun process-line (stacks l move-fn)
  (let* ((ll (parse-line l))
	 (times (first ll))
	 (from (second ll))
	 (to (third ll)))
    (funcall move-fn stacks times from to)))

(defun move-1 (stacks n from to)
  (dotimes (_ n)
    (push (pop (aref stacks from))
	  (aref stacks to))))

(defun move-2 (stacks n from to)
  (let ((items '()))
    (dotimes (_ n)
      (push (pop (aref stacks from)) items))
    (dolist (item items)
      (push item (aref stacks to)))))

;; Part 1
(let ((stacks (create-stacks)))
  (dolist (input (nthcdr 10 *input*))
    (process-line stacks input #'move-1))
  (print (concatenate 'string (cdr (mapcar (lambda (x) (unless (null x) (car x))) (coerce stacks 'list))))))

;; Part 2
(let ((stacks (create-stacks)))
  (dolist (input (nthcdr 10 *input*))
    (process-line stacks input #'move-2))
  (print (concatenate 'string (cdr (mapcar (lambda (x) (unless (null x) (car x))) (coerce stacks 'list))))))


