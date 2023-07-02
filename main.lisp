;; Main file, game loop.

(in-package #:letris)

; After you reach a certain amount of score, decrease counter, and so on and so forth

;; (defun define-speed (level)
;;   (if (= level 2) 100 200))

(defun define-speed (level) 50)

(define-handler (board tick :around) ()
  (with-slots (level speed) board
    (if (> speed 0)
	(decf speed)
	(progn
	  (setf speed (define-speed level))
	  (call-next-method)))))

(define-handler (board tick) ()
  (with-slots (board-representation current-piece current-button move-success score level lines-counter paused) board
    (unless paused
      (let ((lines-cleared (check-board board-representation)))
	(draw board)
	(incf score (give-score lines-cleared))
	(setf (values lines-counter level) (level-up level lines-counter lines-cleared))
	(if (and (null move-success) (eql current-button :s))
	    (progn
	      (glue-piece-on-board board-representation current-piece)
	      (setf current-piece (spawn-random-piece)))
	    (progn
	      (setf move-success (attempt-to-move board-representation current-piece :s))
	      (when (null move-success)
		(glue-piece-on-board board-representation current-piece)
		(setf current-piece (spawn-random-piece)))))))))

(define-handler (board move) ()
  (with-slots (board-representation current-button current-piece) board
    (let ((key (key (source-event move))))
      (when (attempt-to-move board-representation current-piece key)
	(setf current-button key)
	(draw board)))))

(define-event piece-ready-to-glue ())

;; TODO: (????) We need to clean the ghosts of the piece after moving (go to the UNLESS Nathan hates)
;; TODO: Do some (proper) color management
;; TODO: Add the end of the game condition
;; TODO: Add more details
;; TODO: See if it's possible to change the fps at runtime
;; TODO: Add a menu
;; TODO: Add pause/resume indications
;; TODO: Fix the crazy bug (pieces getting stuck on top)
