;; Main file, game loop.

(in-package #:letris)

; After you reach a certain amount of score, decrease counter, and so on and so forth

;; (defun define-speed (level)
;;   (if (= level 2) 100 200))

(defun define-delay (level) 50)

(define-handler (board tick :around) ()
  (with-slots (level delay paused current-button) board
    (unless paused
      (if (null current-button)
	  (if (> delay 0)
	      (decf delay)
	      (progn
		(setf delay (define-delay level))
		(call-next-method)))
	  (call-next-method)))))

(defun handle-input (board)
  (with-slots (board-representation current-piece current-button move-success) board
    (setf move-success (attempt-to-move board-representation current-piece current-button))))

(defun handle-score-level (board)
  (with-slots (level lines-counter board-representation score) board
    (let ((lines-cleared (check-board board-representation)))
      (incf score (give-score lines-cleared))
      (setf (values lines-counter level) (level-up level lines-counter lines-cleared)))))

(defun handle-next-round (board)
  (with-slots (board-representation current-piece) board
    (glue-piece-on-board board-representation current-piece)
    (setf current-piece (spawn-random-piece))))

(defun handle-collision (board collision-state)
  (when (or (eql collision-state :bottom-collision) (eql collision-state :board-collision))
    (handle-next-round board))
  (when (eql collision-state :no-recognized-input)
    (handle-automatic-fall board)))

(defun handle-automatic-fall (board)
  (with-slots (board-representation current-piece) board
    (let ((fall-move (attempt-to-move board-representation current-piece :s)))
      (handle-collision board fall-move))))

(define-handler (board tick) ()
  (with-slots (move-success current-button) board
    (handle-score-level board) ; Handle score and level management
    (handle-input board)
    (draw board) ; Draw board
    (print move-success)
    (if (not (eql :no-collision move-success)) ; Checking side, bottom and board collision
	(handle-collision board move-success) ; Handle all sorts of collisions
	(handle-automatic-fall board)) ; Moving piece down automatically
    (setf current-button nil)))

(define-handler (board move) ()
  (with-slots (current-button) board
    (let ((key (key (source-event move))))
      (setf current-button key))))

;;(define-event piece-ready-to-glue ())

;; TODO: (????) We need to clean the ghosts of the piece after moving (go to the UNLESS Nathan hates)
;; TODO: Add the end of the game condition
;; TODO: Add more details
;; TODO: See if it's possible to change the fps at runtime
;; TODO: Add a menu
;; TODO: Add pause/resume indications
;; TODO: Add space bar for instantaneous drop of the piece
;; TODO: Add a game mode in which you can save a piece for later
;; TODO: Add a preview of the next 4 pieces in line
;; TODO: Add music to the game
;; TODO: Add an option in the menu to swap music
