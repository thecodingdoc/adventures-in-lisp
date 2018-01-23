;;; A simple game of Tic-Tac-Toe, using the minimax algorithm to
;;; choose the optimal move by Dario Ghersi (github: thecodingdoc)
;;;
;;; Last updated: 20180120
;;;
;;; Instructions: type '(play-against-ai)' to start.
;;;               X is the human player, O is the AI

;; identify the playable empty cells on a board
(defun find-empty-cells (b)
  (let ((empty))
    (loop for x from 0 to 9 do
	  (if (eq (nth x b) '-) (push x empty))) empty))
    
;; print out the board
(defun print-board (b)
  (format t "~% ~d ~d ~d   0 1 2~% ~d ~d ~d   3 4 5~% ~d ~d ~d   6 7 8~%~%"
	  (nth 0 b) (nth 1 b) (nth 2 b) (nth 3 b) (nth 4 b) (nth 5 b)
	  (nth 6 b) (nth 7 b) (nth 8 b)))

;; check whether 'player' has won the game
(defun won? (b player)
  (flet ((winRow (row)
		 (every #'identity (mapcar (lambda (x) (eq x player))
					   row))))
	 
	(or (winRow (subseq b 0 3)) ; top row
	    (winRow (subseq b 3 6)) ; middle row
            (winRow (subseq b 6 9)) ; bottom row
	    (winRow (list (nth 0 b) (nth 3 b) (nth 6 b))) ; left column
	    (winRow (list (nth 1 b) (nth 4 b) (nth 7 b))) ; middle column
	    (winRow (list (nth 2 b) (nth 5 b) (nth 8 b))) ; right column
	    (winRow (list (nth 0 b) (nth 4 b) (nth 8 b))) ; left diag
	    (winRow (list (nth 2 b) (nth 4 b) (nth 6 b)))))) ; right diag

;; check the current state
(defun check-state (b)
  (cond ((won? b 'X) -10)
	((won? b 'O) 10)
	((null (find-empty-cells b)) 0)
	(t NIL)))

;; generate the current game tree
(defun get-game-tree (b players)
  (let ((state (check-state b)))
    (cond ((null state)
           (mapcar (lambda (x)
		     (get-game-tree (append (subseq b 0 x)
				      (cons (car players) (subseq b (1+ x) 9))) (reverse players)))
		   (find-empty-cells b)))
	  (t state))))

;; check whether the list has a nested list
(defun has-list-p (l)
  (cond ((null l) '())
	((not (listp l)) '())
	(t (or (listp (car l)) (has-list-p (cdr l))))))

;; minimax algorithm
(defun minimax (game-tree max-min-l)
  (declare (special max-min-l))

  (cond ((null game-tree) '())
	((not (listp game-tree)) game-tree)
	((not (has-list-p game-tree)) (apply (eval '(car max-min-l)) game-tree))
	((null (cdr game-tree)) (minimax (car game-tree) (reverse max-min-l)))
	(t (apply (eval '(car max-min-l))
		  (mapcar (lambda (x)
			    (minimax x (reverse max-min-l))) game-tree)))))

;; evaluate move
(defun evaluate-move (b x)
  (minimax (get-game-tree (append (subseq b 0 x)
				  (cons 'O (subseq b (1+ x) 9)))
			  '(X O)) '(min max)))

;; play human against AI
(defun play-against-AI ()
  (let ((n 1) (human-move '()) (ai-move '()) (allowed '())
	(result '((0 ("Tie!")) (10 ("I win!"))
					 (-10 ("You win!"))))
	(b (copy-list '(- - - - - - - - -)))
	(allowed (copy-list '(0 1 2 3 4 5 6 7 8))))

    (flet ((check-move (b move)
		       (if (and (member move allowed) (eq (nth move b) '-)) t
			 '())))

      (print-board b)

      (loop until (check-state b) do
	    (loop until (check-move b human-move) do
		  (princ "Your move: ") ; let the human play
		  (setf human-move (read)))

	    ;; show the current move
	    (setf (nth human-move b) 'X)
	    
	    ;; remove the move from the allowed
	    (setf allowed (remove human-move allowed)) 
	    (print-board b)

	    ;; AI's first move
	    (if (= n 1)
		(cond ((= human-move 4) (setf ai-move
					      (nth (random 4) '(0 2 6 8))))
		      ((member human-move '(0 2 6 8)) (setf ai-move 4))
		      (t (setf ai-move 4)))

	      ;; AI's moves after 2nd (use minimax)
	      (loop for x in allowed do
		    (cond ((>= (evaluate-move b x) 0)
			   (setf ai-move x)
			   (return)))))

	      (setf (nth ai-move b) 'O)
	      (setf allowed (remove ai-move allowed))
	      (print-board b)

	    (setf n (1+ n)))) ; increment the current move

            ;; return the outcome
            (caadr (assoc (check-state b) result))))
