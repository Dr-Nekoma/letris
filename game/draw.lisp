(in-package #:letris)

(defun draw (board)
  (with-slots (board-representation current-piece) board
    (let ((board-clone (copy board-representation))
          (data (pixel-data board)))
      (glue-piece-on-board board-clone current-piece)
      (board-to-tilemap (extend-board board-clone) data)     
      (setf (pixel-data board) data))))

(defun extend-board (board)
  (destructuring-bind (h w) (array-dimensions board)
    (let ((new-board (make-array '(26 12) :initial-element 8)))
      (loop :for i :from 0 :below h
            :do (loop :for j :below w
                      :do (setf (aref new-board (+ i 1) (+ j 1)) (aref board i j))))
      new-board)))

(defun board-to-tilemap (board-representation tilemap)
  (destructuring-bind (h w) (array-dimensions board-representation)
    (let ((index 0))
      (loop :for i :from (- h 1) :downto 0
            :do (loop :for j :below w
                      :do (setf (values
                                 (aref tilemap index)
                                 (aref tilemap (+ 1 index)))
                                (number-to-tile (aref board-representation i j)))
                          (incf index 2))))))

(defun make-default-board (h w)
  (make-instance 'board :location (vec (floor w 2) (floor h 2) 0)
                        :tileset (// 'letris-assets 'tileset)
                        :size (vec 12 26)
                        :tile-size (vec 32 32)
                        :tilemap (make-array 624 :element-type '(unsigned-byte 8))
                        :music (harmony:play +music-path+ :repeat t)))



(define-handler (board resize) ()
 (setf (location board) (vec (floor (width resize) 2)
                             (floor (height resize) 2)
                             0)))

