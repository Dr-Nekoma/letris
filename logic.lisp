(defpackage "letris"
  (:use :cl))

(in-package "letris")

(defun make-board () (make-array '(24 10)))

(defclass piece ()
  ((kind
    :initarg :kind
    :accessor kind)
   (pos
    :initarg :pos
    :initform '(0 4)
    :accessor pos)
   (representation
    :initarg :representation
    :initform nil
    :accessor representation)))

(defmethod copy ((obj piece))
  (let ((new-piece (make-instance 'piece :kind (kind obj))))
    (setf (representation new-piece) (representation obj))
    (setf (pos new-piece) (pos obj))
    new-piece))

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

(defun spawn-random-piece ()
  (let ((index (random 6)))
    (case index
      (0 (spawn 't-piece))
      (1 (spawn 's-piece))
      (2 (spawn 'i-piece))
      (3 (spawn 'j-piece))
      (4 (spawn 'o-piece))
      (5 (spawn 'z-piece)))))

;; Create a global piece
;; Create a global board
;; Create a user-input control variable

;; Call the game loop (interacts with 3)
;; Read the user-input
;; Given a piece, a board, and an user-input, can we proceed with this movement?

(defparameter *test-piece* (spawn 'o-piece))
(setf (pos *test-piece*) '(0 2))

(defparameter *test-board* (make-array '(6 6)
                                       :initial-contents #(#(0 0 0 0 0 1)
                                                           #(0 0 0 0 0 1)                                                    
                                                           #(0 0 0 0 0 1)
                                                           #(0 0 0 0 0 1)
                                                           #(0 0 0 0 0 1)
                                                           #(0 0 0 0 0 1))))

(defun copy-multi-array (array)
  (let ((res (make-board)))
    (loop :for i :below (array-total-size array)
          :do (setf (row-major-aref res i) (row-major-aref array i)))
    res))

(defun game-loop ()
  (let ((user-movement (read-char))  
        (clone-board (copy-multi-array *test-board*)))
    (case user-movement
      (#\d (princ "👉")) ;(move-piece-right clone-board *test-piece*))
      (#\a (princ "👈")) ;(move-piece-left clone-board *test-piece*))
      (#\w (princ "🌀")) ;(rotate-piece *test-piece*))
      (#\s (princ "👇"))) ;(advance-game-tick clone-board *test-piece*)))
    clone-board))

;; We know we want to go to the right
;; We should update the piece's pos to x + 1 and y + 0
;; We should call has-collision with the current board and the updated piece
;; If we get a collision we just return the original board
;; If we don't get a collision we just draw the updated piece in the board to get a new one

(defun move-adjustments (piece board movement-coord)
  (let ((new-piece (copy piece)))
    (setf (pos new-piece) movement-coord)
    (unless (has-collision board new-piece) (draw-piece-on-board board new-piece))))

(defun attempt-to-move (board piece user-movement)
  (let ((x (first (pos piece)))
        (y (second (pos piece))))
    (case user-movement
      (#\d (move-adjustments piece board `(,x ,(+ y 1)))) ;(move-piece-right clone-board *test-piece*))
      (#\a (move-adjustments piece board `(,x ,(- y 1)))) ;(move-piece-left clone-board *test-piece*))
      (#\w (move-adjustments (rotate-piece piece) board `(,x ,y))) ;(rotate-piece *test-piece*))
      (#\s (move-adjustments piece board `(,(+ x 1) ,y))))
    board))

(defun draw-piece-on-board (board piece)
  (let ((init-pos-x (first (pos piece)))
        (init-pos-y (second (pos piece)))
        (dimension (array-dimension (representation piece) 0))
        (piece-matrix (representation piece)))
    (loop :for i :from init-pos-x :below (+ init-pos-x dimension)
          :for a :from 0 :below dimension
          :do (loop :for j :from init-pos-y :below (+ init-pos-y dimension)
                    :for b :from 0 :below dimension
                    :do (when (array-in-bounds-p board i j)
                          (let ((board-value (aref board i j))
                                (piece-value (aref piece-matrix a b)))
                            (format t "~a ~a ~a ~a~%" i j a b)
                            (setf (aref board i j) (logior board-value piece-value))))))))

;; TODO: Treat user input
;; TODO: We need to clean the ghosts of the piece after moving (go to the UNLESS Nathan hates)
;; TODO: Add simple visuals
