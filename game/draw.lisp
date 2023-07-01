(in-package #:letris)

(define-pool letris-assets)

(define-asset (letris-assets tileset) image
    #p"tileset.png")

;(define-shader-entity my-player (vertex-entity colored-entity located-entity listener transformed-entity)
;  ())

; First we map all the elements (numbers) of the representation to tiles

(defun number-to-tile (number)
  (ecase number
    (0 (values 0 0))
    (1 (values 1 1))
    (2 (values 1 1))
    (3 (values 1 1))
    (4 (values 1 1))
    (5 (values 1 1))
    (6 (values 1 1))
    (7 (values 1 1))))

(defun board-to-tilemap (board-representation)
  (destructuring-bind (h w) (array-dimensions board-represetation)
    (let ((tilemap (make-array 480 :element-type '(unsigned-byte 8)))
	  (index 0))
      (loop :for i :from (- h 1) :downto 0
	    :do (loop :for j :to w
		      :do (multiple-value-bind ))))))

(progn
  (defmethod setup-scene ((main main) scene)
    (setf (title *context*) "Letris")
    (let* ((w (width *context*))
           (h (height *context*))
           (board (make-instance 'board :location (vec (floor w 2) (floor h 2) 0)
					:tileset (// 'letris-assets 'tileset)
					:size (vec 10 24)
					:tilemap (make-array 480 :element-type '(unsigned-byte 8)))))
      (enter board scene)
      (enter (make-instance '2d-camera) scene)
      (enter (make-instance 'render-pass) scene)))

  (maybe-reload-scene))

(define-action-set in-game)
(define-action move (directional-action in-game))

(defun launch (&rest args)
  (let ((*package* #.*package*))
    (load-keymap)
    (setf (active-p (action-set 'in-game)) T)
    (float-features:with-float-traps-masked (:divide-by-zero)
      (apply #'trial:launch 'main args))))

(define-handler (board resize) ()
 (setf (location board) (vec (floor (width resize) 2)
			     (floor (height resize) 2)
			     0)))

#||
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
||#
