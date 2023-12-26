;;; A simple game of Tic-Tac-Toe, using the minimax algorithm to
;;; choose the optimal move by Dario Ghersi (github: thecodingdoc)
;;;
;;; Last updated: 20230302
;;;
;;; Instructions: type '(play-against-ai)' to start.
;;;               X is the human player, O is the AI

#lang racket

;; subseq extracts a sublist bounded by start and end (end not included)
(define (subseq lst start end)
  (let ((seq empty))
  (for ([i (range start end)])
    (set! seq (cons (nth i lst) seq)))
    (reverse seq)))

;; nth extracts the nth item from a list (counting from 0) 
(define (nth idx lst)
  (cond ((empty? lst) empty)
        ((= idx 0) (first lst))
        (else (nth (- idx 1) (rest lst)))))

;; identify the playable empty cells on a board
(define (find-empty-cells b)
  (define (check-empty lst pos)
    (cond ((null? lst) '())
          ((symbol=? (first lst) '-) (cons pos (check-empty (rest lst) (add1 pos))))
          (else (check-empty (rest lst) (add1 pos)))))
  (check-empty b 0))

;; print out the board
(define (print-board b)
  (fprintf (current-output-port) "~n ~a ~a ~a   0 1 2~n ~a ~a ~a   3 4 5~n ~a ~a ~a   6 7 8~n~n"
          (first b) (second b) (third b) (fourth b) (fifth b) (sixth b)
          (seventh b) (eighth b) (ninth b)))

;; check whether 'player' has won the game
(define (won? b player)
  (define (how-many? lst)
    (cond ((null? lst) 0)
          ((symbol=? player (first lst)) (add1 (how-many? (rest lst))))
          (else 0)))
  (or
   (= 3 (how-many? (list (first b) (second b) (third b)))) ; first row
   (= 3 (how-many? (list (fourth b) (fifth b) (sixth b)))) ; second row
   (= 3 (how-many? (list (seventh b) (eighth b) (ninth b)))) ; third row

   (= 3 (how-many? (list (nth 0 b) (nth 3 b) (nth 6 b)))) ; left column
   (= 3 (how-many? (list (nth 1 b) (nth 4 b) (nth 7 b)))) ; middle column
   (= 3 (how-many? (list (nth 2 b) (nth 5 b) (nth 8 b)))) ; right column
   
   (= 3 (how-many? (list (nth 0 b) (nth 4 b) (nth 8 b)))) ; left diag
   (= 3 (how-many? (list (nth 2 b) (nth 4 b) (nth 6 b)))))) ; right diag
    
;; check the current state
(define (check-state b)
  (cond ((won? b 'X) -10)
        ((won? b 'O) 10)
        ((empty? (find-empty-cells b)) 0)
        (else #f)))

;; generate the current game tree
(define (get-game-tree b players)
  (let ((state (check-state b)))
    (cond ((not state)
           (map (lambda (x)
                     (get-game-tree (append (subseq b 0 x)
                                      (cons (first players) (subseq b (add1 x) 9))) (reverse players)))
                   (find-empty-cells b)))
          (else state))))

;; check whether the list has a nested list
(define (has-list-p lst)
  (cond ((null? lst) empty)
        ((not (list? lst)) empty)
        (else (or (list? (first lst)) (has-list-p (rest lst))))))

;; minimax algorithm
(define (minimax game-tree max-min-l)
  (cond ((not game-tree) empty)
        ((not (list? game-tree)) game-tree)
        ((null? (has-list-p game-tree)) (apply (eval (first max-min-l)) game-tree))
        ((null? (cdr game-tree)) (minimax (first game-tree) (reverse max-min-l)))
        (else (apply (eval (first max-min-l))
                  (map (lambda (x)
                         (minimax x (reverse max-min-l))) game-tree)))))

;; evaluate move
(define (evaluate-move b x)
  (minimax (get-game-tree (append (subseq b 0 x)
                                  (cons 'O (subseq b (add1 x) 9)))
                          '(X O)) '(min max)))

;; play human against AI
(define (play-against-AI)
  (let ((n 1) (human-move empty) (ai-move empty) (best-score -10)
        (result '((0 ("Tie!")) (10 ("I win!"))
                                         (-10 ("You win!"))))
        (b '(- - - - - - - - -))
        (allowed '(0 1 2 3 4 5 6 7 8)))

    (define (check-move b move)
      (if (and (member move allowed) (symbol=? (nth move b) '-))
          #t
          #f))

    (print-board b) ; print the empty board

    ;; main loop
    (do ()
      ((check-state b))
      
      (do () ; human to move
        ((check-move b human-move))
        (display "Your move: ")
        (set! human-move (read)))

      (set! b (list-set b human-move 'X))

      
      ;; remove the move from the allowed
      (set! allowed (remove human-move allowed))
      (print-board b)

      ;; AI first move
      (cond ((= n 1)
          (cond ((= human-move 4) (set! ai-move
                                        (nth (random 4) '(0 2 6 8))))
                ((member human-move '(0 2 6 8)) (set! ai-move 4))
                (else (set! ai-move 4))))

          ;; AI's moves after 2nd (use minimax)
          (else (set! best-score -10)
                 (for ([x allowed])
                   (let ((new-score (evaluate-move b x)))
                     (cond ((> new-score best-score)
                            (set! ai-move x) (set! best-score new-score)))))))

      ;; set and display the AI move
      (set! b (list-set b ai-move 'O))
      (set! allowed (remove ai-move allowed))
      (print-board b)

      ;; increment the move
      (set! n (add1 n)))

      ;; return the outcome
      (caadr (assoc (check-state b) result))))


          
      
