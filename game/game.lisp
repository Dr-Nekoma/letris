(in-package #:letris)

(defclass main (trial:main)
  ())

(define-shader-entity board (tile-layer listener)
  ((board-representation :initform (make-board))
   (current-piece :initform (spawn-random-piece))
   (current-button :initform nil)
   (score :initform 0)
   (level :initform 0)
   (lines-counter :initform 0)
   (state :initform :no-collision)
   (paused :initform nil)
   (delay :initform 100)))

(define-handler (board tick :around) ()
  (with-slots (level delay paused current-button) board
    (if (and (null current-button) (not paused))
	(if (> delay 0)
	    (decf delay)
	    (progn
	      (setf delay (define-delay level))
	      (call-next-method)))
	(call-next-method))))

(defun update-state (board)
  (with-slots (current-button state) board
    (setf state (handle-input board current-button))))

(defun handle-score-level (board)
  (with-slots (level lines-counter board-representation score) board
    (let ((lines-cleared (check-board board-representation)))
      (incf score (give-score lines-cleared))
      (setf (values lines-counter level) (level-up level lines-counter lines-cleared)))))

(defun handle-next-round (board)
  (with-slots (board-representation current-piece) board
    (glue-piece-on-board board-representation current-piece)
    (setf current-piece (spawn-random-piece))))

(defun handle-state (board collision-state)
  (when (or (eql collision-state :bottom-collision) (eql collision-state :board-collision))
    (handle-next-round board))
  (when (eql collision-state :idle)
    (handle-automatic-fall board)))

(defun handle-automatic-fall (board)
  (with-slots (board-representation current-piece) board
    (let ((fall-response (handle-input board :s)))
      (handle-state board fall-response))))
