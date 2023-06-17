;; Main file, game loop.

(in-package #:letris)

(gamekit:defgame letris ()
  ((board :initform (make-board))
   (current-piece :initform (spawn-random-piece))
   (current-button :initform :s)
   (score :initform 0)
   (move-success :initform t)
   (mute :initform t)
   (cursor-position :initform (gamekit:vec2 0 0))
   (paused :initform nil)
   (%clone))
  (:viewport-width *canvas-width*)     ; window's width
  (:viewport-height *canvas-height*)   ; window's height
  (:viewport-title "Letris")  ; window's title
  (:act-rate 5)
  (:draw-rate 5))


(defmethod gamekit:post-initialize :after ((this letris))
  (with-slots (board current-piece current-button
               move-success mute cursor-position score paused) this
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
    (gamekit:bind-button :m :pressed
                         (lambda ()
                           (if mute
                              (progn
                                (gamekit:play 'tetris-music :looped-p t)
                                (setf mute nil))
                              (progn
                                (gamekit:stop-sound 'tetris-music)
                                (setf mute t)))))
    (gamekit:bind-button :p :pressed
                         (lambda ()
                           (setf paused (not paused))))
    (gamekit:bind-cursor (lambda (x y)
                          (setf (gamekit:x cursor-position) x
                                (gamekit:y cursor-position) y)))
    (gamekit:bind-button :mouse-left :pressed
                         (lambda ()
                           (when (inside-p (coordinates *reset-button*) cursor-position)
                              (setf board (make-board)
                                    current-piece (spawn-random-piece)
                                    score 0
                                    move-success t
                                    current-button :s))))))


   




;; TODO: (????) We need to clean the ghosts of the piece after moving (go to the UNLESS Nathan hates)
;; TODO: Do some (proper) color management
;; TODO: Add the end of the game condition
;; TODO: Add more details
;; TODO: See if it's possible to change the fps at runtime
;; TODO: Add a menu
;; TODO: Add pause/resume indications
;; TODO: Fix the crazy bug (pieces getting stuck on top)
