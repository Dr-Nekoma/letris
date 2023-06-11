;; Main file, game loop.

(in-package #:letris)

(gamekit:defgame letris ()
  ((board :initform (make-board))
   (current-piece :initform (spawn-random-piece))
   (current-button :initform :s)
   (score :initform 0)
   (move-success :initform t))
  (:viewport-width *canvas-width*)     ; window's width
  (:viewport-height *canvas-height*)   ; window's height
  (:viewport-title "Letris")  ; window's title
  (:act-rate 5)
  (:draw-rate 5))


(defmethod gamekit:post-initialize :after ((this letris))
  (with-slots (board current-piece current-button move-success) this
    (gamekit:bind-button :a :pressed
                         (lambda ()
                           (let ((x (first (pos current-piece)))
                                 (y (second (pos current-piece))))
                             (setf current-button :a
                                   move-success (move-adjustments current-piece board (change-coords `(,x ,(- y 1))))))))
    (gamekit:bind-button :d :pressed
                         (lambda ()
                           (let ((x (first (pos current-piece)))
                                 (y (second (pos current-piece))))
                             (setf current-button :d
                                   move-success (move-adjustments current-piece board (change-coords `(,x ,(+ y 1))))))))
    (gamekit:bind-button :w :pressed
                         (lambda ()
                           (setf current-button :w
                                 move-success (move-adjustments current-piece board #'rotate-piece-left))))
    (gamekit:bind-button :s :pressed
                         (lambda ()
                           (let ((x (first (pos current-piece)))
                                 (y (second (pos current-piece))))
                             (setf current-button :s
                                   move-success (move-adjustments current-piece board (change-coords `(,(+ x 1) ,y)))))))
    (gamekit:play 'tetris-music :looped-p t)))



;; TODO: (????) We need to clean the ghosts of the piece after moving (go to the UNLESS Nathan hates)
;; TODO: Do some (proper) color management
;; TODO: Add the end of the game condition
;; TODO: Add more details
;; TODO: See if it's possible to change the fps at runtime
;; TODO: Add a menu
;; TODO: Draw a rectangle to outline the board
