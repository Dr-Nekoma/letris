;;;; letris.lisp

(in-package #:letris)


;; Create a global piece
;; Create a global board
;; Create a user-input control variable

;; Call the game loop (interacts with 3)
;; Read the user-input
;; Given a piece, a board, and an user-input, can we proceed with this movement?

(defparameter *test-piece* (spawn 'o-piece))

(defparameter *test-board* (make-board))

;; Read user movement
;; Attempt to move the piece according the move
;; If we collide going down, we glue the piece onto the board, we generate a new piece
;; Else, we should attempt to go down
;; If we collide, we glue the piece onto the board, we generate a new piece
;; We draw
;; We repeat

(defparameter *board* (make-array '(5 4)
                                  :initial-contents #(#(0 0 0 0)
                                                      #(1 0 1 0)
                                                      #(1 1 1 1)
                                                      #(1 0 1 1)
                                                      #(1 1 1 1))))
;; Pick the user movement and move
;; Check for collision and glue if necessary
;; Go down
;; Check for collision and glue if necessary
;; Repeat

;; We know we want to go to the right
;; We should update the piece's pos to x + 1 and y + 0
;; We should call has-collision with the current board and the updated piece
;; If we get a collision we just return the original board
;; If we don't get a collision we just draw the updated piece in the board to get a new one



;; TODO: Treat user input
;; TODO: We need to clean the ghosts of the piece after moving (go to the UNLESS Nathan hates)
;; TODO: Add simple visuals
