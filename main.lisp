;; Main file, game loop.

(in-package #:letris)

(define-handler (board tick) ()
  (with-slots (state current-button) board
    (handle-score-level board)              ; Handle score and level management
    (update-state board)                    ; Handle internal game state
    (draw board)                            ; Draw board
    (if (not (eql :no-collision state))     ; Checking all game states aside from no-collision
	(handle-state board state)          ; Handle all remaining states
	(handle-automatic-fall board))      ; Moving piece down automatically
    (setf current-button nil)))             ; Erase past input from user

;; TODO: (????) We need to clean the ghosts of the piece after moving (go to the UNLESS Nathan hates)
;; TODO: Add the end of the game condition
;; TODO: Add more details
;; TODO: Add speed management when increasing levels
;; TODO: Add a menu
;; TODO: Add pause/resume indications
;; TODO: Add a game mode in which you can save a piece for later
;; TODO: Add a preview of the next 4 pieces in line
;; TODO: Add music to the game
;; TODO: Add an option in the menu to swap music
