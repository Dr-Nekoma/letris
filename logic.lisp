(defpackage "letris"
  (:use :cl))

(in-package "letris")

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
    (t-piece (make-array '(3 3)
                         :initial-contents #(#(0 1 0)
                                             #(1 1 1)
                                             #(0 0 0))))
    (s-piece (make-array '(3 3)
                         :initial-contents #(#(0 1 1)
                                             #(1 1 0)
                                             #(0 0 0))))
    (i-piece (make-array '(4 4)
                         :initial-contents #(#(0 1 0 0)
                                             #(0 1 0 0)
                                             #(0 1 0 0)
                                             #(0 1 0 0))))
    (j-piece (make-array '(3 3)
                         :initial-contents #(#(1 1 0)
                                             #(1 0 0)
                                             #(1 0 0))))
    (o-piece (make-array '(4 4)
                         :initial-contents #(#(0 0 0 0)
                                             #(0 1 1 0)
                                             #(0 1 1 0)
                                             #(0 0 0 0))))
    (z-piece (make-array '(3 3)
                         :initial-contents #(#(1 1 0)
                                             #(0 1 1)
                                             #(0 0 0))))
    (l-piece (make-array '(3 3)
                         :initial-contents #(#(1 0 0)
                                             #(1 0 0)
                                             #(1 1 0))))))

(defmethod initialize-instance :after ((obj piece) &key)
  (setf (representation obj) (make-representation (kind obj))))

(defun spawn (kind)
  (make-instance 'piece :kind kind))

(defun matrix-multiplication (mat1 mat2)
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

(defparameter identity-horizontaly-mirrowed (make-array '(3 3)
                                                        :initial-contents #(#(0 0 1)
                                                                            #(0 1 0)
                                                                            #(1 0 0))))

(defun rotate-i-piece (piece)
  "Handles special case for straight piece (I piece)."
  (let ((i-state-1 (make-array '(4 4)
                               :initial-contents #(#(0 1 0 0)
                                                   #(0 1 0 0)
                                                   #(0 1 0 0)
                                                   #(0 1 0 0))))
        (i-state-2 (make-array '(4 4)
                               :initial-contents #(#(0 0 0 0)
                                                   #(1 1 1 1)
                                                   #(0 0 0 0)
                                                   #(0 0 0 0)))))
    (if (= (aref (representation piece) 0 1) 1)
        (setf (representation piece) i-state-2)
        (setf (representation piece) i-state-1))))


(defun rotate-piece (piece)
  "Rotation of pieces. Two special cases are considered."
  (case (kind piece)
    (i-piece (rotate-i-piece piece))
    (o-piece piece)
    (otherwise (setf (representation piece) (matrix-multiplication (transpose (representation piece)) identity-horizontaly-mirrowed))))
  piece)

(defun advance-game-stage (board piece user-input)
  (case user-input
    (#\d (move-piece-right board piece))
    (#\a (move-piece-left board piece))
    (#\w (rotate-piece piece))
    (#\s (advance-game-tick board piece)))
  (advance-game-tick board piece))

(defun has-collision (board piece)
  (destructuring-bind (piece-row-size piece-column-size) (array-dimensions (representation piece))
    (destructuring-bind (board-row-size board-column-size) (array-dimensions board)
      (let* ((initial-position (pos piece))
             (piece-pos-x (car initial-position))
             (piece-pos-y (cadr initial-position))
             (piece-matrix (representation piece))
             (answer nil))
        (loop :for i :below piece-row-size
              :do (loop :for j :below piece-column-size
                        :do (let* ((piece-value (aref piece-matrix i j))
                                   (board-x (+ i piece-pos-x))
                                   (board-y (+ j piece-pos-y))
                                   (is-out-of-bounds (or (>= board-x board-row-size) (>= board-y board-column-size)))
                                   (is-piece-1 (= piece-value 1)))
                              (if is-out-of-bounds
                                  (setf answer (or answer is-piece-1))
                                (let ((board-value (aref board (+ i piece-pos-x) (+ j piece-pos-y))))
                                  (setf answer (or answer (and (= board-value 1) is-piece-1))))))))
        answer))))

(defun check-for-collisions (board piece user-input)
  (destructuring-bind (rows columns) (array-dimensions board)
    (let ((initial-position (pos piece))
          (piece-matrix (representation piece)))
      (case user-input
        (#\d (can-proceed-right-boundary (- columns 1) piece))
        (#\a (can-proceed-left-boundary 0 piece))
        (#\s (can-proceed-bottom-boundary (- rows 1) piece))))))

(defparameter test-piece (spawn 'l-piece))
(setf (pos test-piece) '(0 4))

(defparameter test-board (make-array '(6 6)
                                     :initial-contents #(#(0 0 0 0 0 0)
                                                         #(0 0 0 0 0 0)                                                    
                                                         #(0 0 0 0 0 0)
                                                         #(0 0 0 0 0 0)
                                                         #(0 0 0 0 0 0)
                                                         #(0 0 0 0 0 0))))

;; TODO: Translate collision to mutation (put the shadow in the board)
;; TODO: Treat user input
;; TODO: Add simple visuals
