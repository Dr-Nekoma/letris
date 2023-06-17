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
