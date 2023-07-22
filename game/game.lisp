(in-package #:letris)

(defclass main (trial:main)
  ())

(define-shader-entity dummy (tile-layer listener) ())

(define-shader-entity board (tile-layer listener)
  ((board-representation :initform (make-board))
   (current-piece :initform (spawn-random-piece))
   (current-button :initform nil)
   (score :initform 0)
   (level :initform 0)
   (lines-counter :initform 0)
   (state :initform :no-collision)
   (saved-piece :initform nil)
   (paused :initform nil)
   (delay :initform base-delay)
   (current-delay :initform base-delay)))

(define-handler (board tick :around) ()
  (with-slots (level delay paused current-button current-delay) board
    (if (and (null current-button) (not paused))
        (if (> delay 0)
            (decf delay)
            (progn
              (setf delay (calculate-delay level current-delay))
              (setf current-delay delay)
              (call-next-method)))
        (call-next-method))))

(defun update-state (board)
  (with-slots (current-button current-piece state) board
    (setf state (handle-input board current-piece current-button))))

(defun handle-score-level (board)
  (with-slots (level lines-counter board-representation score) board
    (let ((lines-cleared (check-board board-representation)))
      (incf score (give-score lines-cleared))
      (setf (values lines-counter level) (level-up level lines-counter lines-cleared)))))

;; TODO: We should re-think on how we will spawn in the piece (above the board and such) because
;; this changes when the game ends and how we will the final piece to the player
(defun check-end-condition (board)
  (with-slots (board-representation current-piece state) board
    (let ((collision-result (has-collision board-representation current-piece)))
      (when (eql collision-result :board-collision)
        (glue-piece-on-board board-representation current-piece)
        (setf state nil)))))

(defun handle-next-round (board)
  (with-slots (board-representation current-piece) board
    (glue-piece-on-board board-representation current-piece)
    (setf current-piece (spawn-random-piece))
    (check-end-condition board)))

(defun handle-state (board collision-state)
  (case collision-state
    ((:bottom-collision :board-collision) (handle-next-round board))
    (:idle (handle-automatic-fall board))))

(defun handle-automatic-fall (board)
  (with-slots (board-representation current-piece) board
    (let ((fall-response (handle-input board current-piece :s)))
      (handle-state board fall-response))))
