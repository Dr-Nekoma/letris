(in-package #:letris)


(defun print-board (board)
  (destructuring-bind (m n) (array-dimensions board)
    (loop :for i :below m
          :do (loop :for j :below n
                    :do (format t "~s " (aref board i j)))
              (break)
              (terpri))))




(defun display (piece)
  (let ((x (elt (pos piece) 0))
        (y (elt (pos piece) 1))
        (board (make-board))
        (piece-matrix (representation piece)))
    (loop :for i :below (array-dimension piece-matrix 0)
          :do
          (loop :for j :below (array-dimension piece-matrix 1)
                :do
                (setf (aref board (+ x i) (+ y j)) (aref piece-matrix i j))))
    board))


(defun print-piece (piece)
  (format t "~a~%" (kind piece))
  (destructuring-bind (m n) (array-dimensions (representation piece))
    (loop :for i :below m
          :do (loop :for j :below n
                    :do (format t "~s " (aref (representation piece) i j)))
              (terpri))))




;;; ---------------------------------------------------------------------------
;;; Experimentation zone

(setf +app-system+ "letris")

(defclass main (trial:main)
  ())

(define-pool letris-assets)


(define-asset (letris-assets pixel) mesh
    (make-rectangle-mesh 25 25))

(define-asset (letris-assets board) mesh
    (make-rectangle-mesh 300 720))

(define-asset (letris-assets test) mesh
    (make-line-grid-mesh 30 300 720 :pack T))

(define-asset (letris-assets tileset) image
    #p"tileset.png")

;(define-shader-entity my-player (vertex-entity colored-entity located-entity listener transformed-entity)
;  ())

(define-shader-entity my-board (tile-layer listener) ())


(progn
  (defmethod setup-scene ((main main) scene)
    (setf (title *context*) "Letris")
    (let* ((w (width *context*))
           (h (height *context*))
           (board (make-instance 'my-board :location (vec (floor w 2) (floor h 2) 0)
                                           :tileset (// 'letris-assets 'tileset)
                                           :size (vec 2 2)
                                           :tilemap (make-array 8 :element-type '(unsigned-byte 8)
                                                                  :initial-contents #(0 0 1 0 0 1 1 1))))
           (board2 (make-instance 'my-board :location (vec (+ 20 (floor w 2)) (+ 20 (floor h 2) 0) 0)
                                            :tileset (// 'letris-assets 'tileset)
                                            :size (vec 2 2)
                                            :tilemap (make-array 8 :element-type '(unsigned-byte 8)
                                                                   :initial-contents #(0 0 1 0 0 1 1 1)))))
      (enter board2 scene)
      (enter board scene)
      (enter (make-instance '2d-camera) scene)
      (enter (make-instance 'render-pass) scene)))

  (maybe-reload-scene))


;(define-action-set in-game)
;(define-action move (directional-action in-game))


(defun launch (&rest args)
  (let ((*package* #.*package*))
    ;(load-keymap)
    ;(setf (active-p (action-set 'in-game)) T)
    (apply #'trial:launch 'main args)))


(let ((counter 0))
  (define-handler (my-board tick :around) ()
    (if (> counter 10)
        (progn (setf counter 0)
               (call-next-method))
        (incf counter))))

(define-handler (my-board tick) ()
  (let ((data (pixel-data my-board)))
    (map-into (pixel-data my-board) (lambda () (random 2)))
    (setf (pixel-data my-board) data)))



;(define-handler (my-board resize) ()
;  (setf (location my-board) (vec (floor (width resize) 2)
;                                 (floor (height resize) 2)
;                                 0)))


#||
(defvar *canvas-width* 800)
(defvar *canvas-height* 600)


(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *transparent* (gamekit:vec4 0 0 0 0))
(defvar *origin* (gamekit:vec2 0 0))
(defparameter *reset-button* (make-instance 'button
                                            :coords (gamekit:vec4 30 530 150 580)
                                            :name "Reset"))
(defparameter *single-pixel* 25)

(gamekit:define-font noto-sans (asdf:system-relative-pathname "letris" "assets/NotoSans-Regular.ttf"))
(gamekit:define-sound tetris-music (asdf:system-relative-pathname "letris" "assets/Tetris_theme.ogg"))


(defun draw-matrix (matrix origin color offset)
  (destructuring-bind (m n) (array-dimensions matrix)
    (incf (gamekit:x origin) offset)
    (loop :for i :from (- m 1) :downto 0
          :do (let ((prev-x (gamekit:x origin)))
                (loop :for j :below n
                      :do (let ((color-to-draw (if (= 0 (aref matrix i j))
                                                   *transparent* color)))
                            (gamekit:draw-rect origin *single-pixel* *single-pixel*
                                               :fill-paint color-to-draw)
                            (incf (gamekit:x origin) *single-pixel*)))
                (incf (gamekit:y origin) *single-pixel*)
                (setf (gamekit:x origin) prev-x)))))

(defun draw-outline (matrix origin offset)
  (destructuring-bind (m n) (array-dimensions matrix)
    (incf (gamekit:x origin) offset)
    (let* ((bottom-left origin)
           (top-left (gamekit:add bottom-left (gamekit:vec2 0 (* m *single-pixel*))))
           (top-right (gamekit:add top-left (gamekit:vec2 (* n *single-pixel*) 0)))
           (bottom-right (gamekit:add bottom-left (gamekit:vec2 (* n *single-pixel*) 0))))
      (gamekit:draw-polyline (list bottom-left top-left top-right bottom-right bottom-left)
                            *black*
                             :thickness 3.0))))


(defun draw-button (button)
  (with-slots (coordinates name) button
    (let* ((height (- (gamekit:w coordinates) (gamekit:y coordinates)))
           (width (- (gamekit:z coordinates) (gamekit:x coordinates)))
           (bottom-left (gamekit:vec2 (gamekit:x coordinates) (gamekit:y coordinates)))
           (top-left (gamekit:add bottom-left (gamekit:vec2 0 height)))
           (top-right (gamekit:add top-left (gamekit:vec2 width 0)))
           (bottom-right (gamekit:add bottom-left (gamekit:vec2 width 0))))
      (gamekit:draw-polyline (list bottom-left top-left top-right bottom-right bottom-left)
                             *black*
                             :thickness 3.0)
      (gamekit:draw-text name (gamekit:add 10 bottom-left)
                         :font (gamekit:make-font 'noto-sans (calc-font-size (* height width)))))))




(defmethod gamekit:draw ((this letris))
  (with-slots (board current-piece score %clone) this
    (gamekit:draw-text (format nil "SCORE: ~s" score) (gamekit:vec2 (- *canvas-width* 140)
                                                                    (- *canvas-height* 40))
                       :font (gamekit:make-font 'noto-sans 32))
    (let ((board-clone (copy board)))
      (draw-button *reset-button*)
      (glue-piece-on-board board-clone current-piece)
      (setf %clone board-clone)
      (draw-outline board-clone *origin* (- (floor *canvas-width* 2) 125))
      (setf *origin* (gamekit:vec2 0 0))
      (draw-matrix board-clone *origin* *black* (- (floor *canvas-width* 2) 125))
      (setf *origin* (gamekit:vec2 0 0)))))
||#
