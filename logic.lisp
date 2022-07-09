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
  (declare (optimize (debug 3) (safety 3)))
  (destructuring-bind (piece-row-size piece-column-size) (array-dimensions (representation piece))
    (destructuring-bind (board-row-size board-column-size) (array-dimensions board)
      (let* ((initial-position (pos piece))
             (piece-pos-x (first initial-position))
             (piece-pos-y (second initial-position))
             (piece-matrix (representation piece))
             (answer nil))
        (loop :for i :below piece-row-size
              :do (loop :for j :below piece-column-size
                        :do (let* ((piece-value (aref piece-matrix i j))
                                   (board-x (+ i piece-pos-x))
                                   (board-y (+ j piece-pos-y))
                                   (is-out-of-bounds (or (>= board-x board-row-size)
                                                         (>= board-y board-column-size)
                                                         (< board-y 0)))
                                   (is-piece-1 (= piece-value 1)))
                              (terpri)
                              (if is-out-of-bounds
                                  (progn
                                    (terpri)
                                    (setf answer (or answer is-piece-1)))
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

(defparameter *test-board* (make-board))

(defun copy-multi-array (array)
  (let ((res (make-board)))
    (loop :for i :below (array-total-size array)
          :do (setf (row-major-aref res i) (row-major-aref array i)))
    res))

;; Read user movement
;; Attempt to move the piece according the move
;; If we collide going down, we glue the piece onto the board, we generate a new piece
;; Else, we should attempt to go down
;; If we collide, we glue the piece onto the board, we generate a new piece
;; We draw
;; We repeat

(defun game-loop ()
  (let* ((user-movement (read-char))
         (success (attempt-to-move *test-board* *test-piece* user-movement)))
    (clear-input)
    (if (and (null success) (eql user-movement #\s))
        (progn
          (glue-piece-on-board *test-board* *test-piece*)
          (setf *test-piece* (spawn-random-piece)))
        (progn
          (setf success (attempt-to-move *test-board* *test-piece* #\s))
          (when (null success)
            (glue-piece-on-board *test-board* *test-piece*)
            (setf *test-piece* (spawn-random-piece)))))
    (draw-piece-on-board *test-board* *test-piece*)
    (game-loop)))


;; Pick the user movement and move
;; Check for collision and glue if necessary
;; Go down
;; Check for collision and glue if necessary
;; Repeat

;; We know we want to go to the right
;; We should update the piece's pos to x + 1 and y + 0
;; We should call has-collision with the current board and the updated piece
;; If we get a collision we just return the original board
;; If we don't get a collision we just draw the updated piece in the board to get a new one

(defun move-adjustments (piece board func)
  (let ((new-piece (funcall func (copy piece))))
    (if (has-collision board new-piece)
        nil
        (progn
          (setf (pos piece) (pos new-piece)
                (representation piece) (representation new-piece))
          t))))

(defun change-coords (coords)
  (lambda (piece)
    (setf (pos piece) coords)
    piece))

(defun attempt-to-move (board piece user-movement)
  (let ((x (first (pos piece)))
        (y (second (pos piece))))
    (case user-movement
      (#\d (move-adjustments piece board (change-coords `(,x ,(+ y 1)))))
      (#\a (move-adjustments piece board (change-coords `(,x ,(- y 1)))))
      (#\w (move-adjustments piece board  #'rotate-piece))
      (#\s (move-adjustments piece board (change-coords `(,(+ x 1) ,y)))))))

(defun draw-piece-on-board (board piece)
  (let ((board-clone (copy-multi-array board)))
    (glue-piece-on-board board-clone piece)
    (prin1 board-clone)
    (terpri)))

(defun glue-piece-on-board (board piece)
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
                            (setf (aref board i j) (logior board-value piece-value))))))))



;; TODO: Treat user input
;; TODO: We need to clean the ghosts of the piece after moving (go to the UNLESS Nathan hates)
;; TODO: Add simple visuals
