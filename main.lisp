;; Main file, game loop.

(in-package #:letris)


(defun game-loop ()
  (let* ((user-movement (read-char))
         (success (attempt-to-move *test-board* *test-piece* user-movement)))
    (clear-input)
    (if (and (null success) (eql user-movement #\s))
        (progn
          (glue-piece-on-board *test-board* *test-piece*)
          (setf *test-piece* (spawn-random-piece)))
        (progn
          (setf success (attempt-to-move *test-board* *test-piece* #\s))
          (when (null success)
            (glue-piece-on-board *test-board* *test-piece*)
            (setf *test-piece* (spawn-random-piece)))))
    (draw-piece-on-board *test-board* *test-piece*)
    (check-board *test-board*)
    (game-loop)))



;; TODO: We need to clean the ghosts of the piece after moving (go to the UNLESS Nathan hates)
;; TODO: Add simple visuals
;; TODO: Fix the visual bug while printing the board
;; TODO: Add time mechanics
