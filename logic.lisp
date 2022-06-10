(defpackage "l3tris"
  (:use :cl))

(in-package "l3tris")

(defun make-board () (make-array '(24 10)))

(defclass piece ()
  ((kind
    :initarg :kind
    :accessor kind)
   (pos
    :initform '(0 4)
    :accessor pos)
   (representation
    :initform nil
    :accessor representation)))

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


(defun make-representation (kind)
    (case kind
        (t-piece #2A((0 1 0)
                     (1 1 1)
                     (0 0 0)))
        (s-piece #2A((0 1 1)
                     (1 1 0)
                     (0 0 0)))
        (i-piece #2A((0 1 0 0)
                     (0 1 0 0)
                     (0 1 0 0)
                     (0 1 0 0)))
        (j-piece #2A((1 1 0)
                     (1 0 0)
                     (1 0 0)))
        (o-piece #2A((0 0 0 0)
                     (0 1 1 0)
                     (0 1 1 0)
                     (0 0 0 0)))
        (z-piece #2A((1 1 0)
                     (0 1 1)
                     (0 0 0)))
        (l-piece #2A((1 0 0)
                     (1 0 0)
                     (1 1 0)))))

(defmethod initialize-instance :after ((obj piece) &key)
  (setf (representation obj) (make-representation (kind obj))))

(defun spawn (kind)
  (make-instance 'piece :kind kind))

(defun matmul (mat1 mat2)
  (destructuring-bind (n1 n2) (array-dimensions mat1)
    (destructuring-bind (m1 m2) (array-dimensions mat2)
      (assert (= n2 m1) (mat1 mat2) "Dimensions mismatch")
      (let ((res (make-array (list n1 m2))))
        (loop :for i :below n1 ; rows of 1
              :do (loop :for j :below m2 ; cols of 2
                        :do (setf (aref res i j)
                                  (loop :for k :below n2 ; cols of 1
                                        :sum (* (aref mat1 i k)
                                                (aref mat2 k j))))))
        res))))

(defun transpose (matrix)
  (destructuring-bind (m n) (array-dimensions matrix)
    (loop :for i :below m
          :do (loop :for j :from (+ i 1) :below n
                    :do (rotatef (aref matrix i j)
                                 (aref matrix j i)))))
  matrix)

;; TODO: Spawn a piece in the board (OR with the grid and the piece)
;; TODO: Rotation of the piece (we think is transposing the 4x4 matrix)
;; TODO: Advance time tick (for testing and in OPENGL)
;; TODO: Treat user input
;; TODO: Treat collision with boundaries of the board
;; TODO: Treat collision with pieces and the grid
;; TODO: Start the OPENGL journey
