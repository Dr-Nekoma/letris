;; Main file, game loop.

(in-package #:letris)

(define-handler (board tick) ()
  (with-slots (state current-button) board
    (update-state board)                      ; Handle internal game state via input
    (unless (null state)                      ; Continue only if the game is still going
      (handle-score-level board)              ; Handle score and level management    
      (draw board)                            ; Draw board
      (if (eql :no-collision state)           ; Checking all game states aside from no-collision
          (handle-automatic-fall board)       ; Moving piece down automatically
          (handle-state board state))         ; Handle all remaining states
      (setf current-button nil))))            ; Erase past input from user

;; Letris 1.0
;; TODO: Implement game logic for 2D game
;; TODO: Visualization in terminal with prints

;; Letris 2.0
;; TODO: Add music to the game
;; TODO: Add visualization of stuff (next piece in line, stored piece, controls, level, score, end game)

;; Letris 3.0
;; TODO: Add different game modes (classic, with-swap)
;; TODO: Add a menu (changing delay configuration, different game modes, changing music)
;; TODO: Transform Letris into L3tris (3D only in terms of visualization)

;; Letris 4.0
;; TODO: Add multiplayer mode to the game
;; TODO: Add a 3D game mode

