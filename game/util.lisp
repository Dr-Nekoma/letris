(in-package #:letris)


(defconstant base-delay 50)

(defun calculate-delay (level delay)
  (case level
    (10 (- base-delay 15)) ;; 35
    (15 (- base-delay 30)) ;; 20
    (20 (- base-delay 40)) ;; 10
    (30 (- base-delay 45)) ;; 5
    (40 1)
    (otherwise delay)))

(defun number-to-tile (number)
  (ecase number
    (0 (values 0 0)) ; OK
    (1 (values 1 1)) ; OK
    (2 (values 1 3))
    (3 (values 1 0)) ; OK
    (4 (values 0 1))
    (5 (values 0 2))
    (6 (values 1 2))
    (7 (values 0 3))
    (8 (values 2 0))))

(defun print-board (board)
  (destructuring-bind (m n) (array-dimensions board)
    (loop :for i :below m
          :do (loop :for j :below n
                    :do (format t "~s " (aref board i j)))
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

