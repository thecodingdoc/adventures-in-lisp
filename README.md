Collection of short programs written in Common Lisp (or other Lisp dialects).
Common Lisp programs should run in SBCL, CLISP, or other ANSI compliant Common Lisp environments.

Author:  Dario Ghersi


TIC TAC TOE
* File: tic-tac-toe.lisp

The game of tic tac toe. The AI of the game implements the minimax algorithm to choose the optimal move.
To play the game, type the following commands in your Lisp prompt:

```
> (load "tic-tac-toe.lisp")
> (play-against-AI)
```

There is also an equivalent version written in Racket Scheme.

---------------------------------------

SUDOKU SOLVER
* Files: sudoku-solver.lisp
         sudoku-example.lisp

A simple breadth-first search sudoku solver.
Check out the example in sudoku-example.lisp for setting up an input of your own.
To solve the example, run the following:

```
> (load "sudoku-solver.lisp")
> (load "sudoku-example.lisp")
> (print-board *sudoku-example*)
> (print-board (solve-sudoku *sudoku-example*))

---------------------------------------
