;;; A depth-first search sudoku solver
;;;
;;; Last updated: 20180127
;;;
;;; Instructions: check out the example in sudoku-example.lisp
;;;               and either load the puzzle in the example or
;;;               input your own.

;;;               Example:
;;;               (load "sudoku-example.lisp")
;;;               (load "sudoku-solver.lisp")
;;;               (print-board *sudoku-example*)
;;;               (print-board (solve-sudoku *sudoku-example*))

(defun print-board (b)
  (let ((n 0))
    (loop for x in b do
	  (cond ((or (= n 27) (= n 54))
		 (format t "~%---------+---------+---------~%"))

		((= (mod n 9) 0)
		 (format t "~%"))
		
		((= (mod n 3) 0)
		 (princ "|"))
		
		((= (mod n 9) 0)
		 (format t "~%")))
	  
	  (setf n (1+ n))
	  (format t " ~S " x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun board-has-duplicates-p (b)
  ;; T if there is a duplicate number in a row, column, or square
  
  (labels ((find-duplicates (x l)
			    (cond ((null x) '())
				  ((and (numberp (car x))
					(member (car x) l)) t)
				  (t (find-duplicates (cdr x)
						      (cons (car x) l)))))
	   
           ;; check whether there are duplicates in a given row
	   (check-row (n b)
		      (find-duplicates (subseq b (* n 9) (+ (* n 9) 9))
				       '()))

	   ;; check whether there are duplicates in a given column
	   (check-column (n b)
			 (let ((column (mapcar (lambda (x)
						 (nth (+ n (* x 9)) b))
					       '(0 1 2 3 4 5 6 7 8))))
			   (find-duplicates column '())))

	   ;; check whether there are duplicates in a given square
	   (check-square (n b)
			 (let* ((beg (* 2 (truncate (/ n 3))))
				(beg-1 (+ (* 3 n) (* beg 9)))
				(beg-2 (+ beg-1 9))
				(beg-3 (+ beg-2 9))
				(square (append (append (subseq b beg-1
								(+ 3 beg-1))
							(subseq b beg-2
								(+ 3 beg-2)))
						(subseq b beg-3 (+ 3 beg-3)))))

			   (find-duplicates square '()))))

	  ;; check all rows, columns, and squares
	  (loop for n from 0 to 8 do
		(print (check-row 8 b))
		(cond ((or (check-row n b) (check-column n b)
			   (check-square n b))
		       (return-from board-has-duplicates-p t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solve-sudoku (b)
  (with-open-stream
   (*standard-output* (make-broadcast-stream))
   (labels ((depth-first (b n)
			 (cond ((= n 81)
				(return-from solve-sudoku b))
			       ((eq (nth n b) '-)
				(loop for x from 1 to 9 do
				      (let ((newb
					     (append (subseq b 0 n)
						     (cons x (subseq b (1+ n) 81)))))
					
					(cond ((not (board-has-duplicates-p newb))
					       (depth-first newb (1+ n)))))))
			       (t (depth-first b (1+ n))))))
	   (depth-first b 0))))
