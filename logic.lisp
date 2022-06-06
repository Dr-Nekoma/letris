(defpackage "l3tris"
  (:use :cl))

(in-package "l3tris")

(make-array '(20 10))

(defclass piece ()
  ((kind
    :initarg :kind
    :accessor kind)
   (pos
    :initform #(0 4)
    :accessor pos)
   (representation
    :initform nil
    :accessor representation)))

(defmethod initialize-instance :after ((obj piece) &key)
  (setf (representation obj) (make-representation (kind obj))))

(make-instance 'piece :kind 't-piece)
(make-instance 'piece :kind 's-piece)
(make-instance 'piece :kind 'i-piece)
(make-instance 'piece :kind 'j-piece)
(make-instance 'piece :kind 'o-piece)
(make-instance 'piece :kind 'z-piece)
(make-instance 'piece :kind 'l-piece)

(defun make-representation (kind)
    (case kind
        (t-piece #2A((0 1 0 0)
                     (1 1 1 0)
                     (0 0 0 0)
                     (0 0 0 0)))
        (s-piece #2A((0 0 1 1)
                     (0 1 1 0)
                     (0 0 0 0)
                     (0 0 0 0)))
        (i-piece #2A((0 1 0 0)
                     (0 1 0 0)
                     (0 1 0 0)
                     (0 1 0 0)))
        (j-piece #2A((1 1 0 0)
                     (1 0 0 0)
                     (1 0 0 0)
                     (1 0 0 0)))
        (o-piece #2A((0 0 0 0)
                     (0 1 1 0)
                     (0 1 1 0)
                     (0 0 0 0)))
        (z-piece #2A((1 1 0 0)
                     (0 1 1 0)
                     (0 0 0 0)
                     (0 0 0 0)))
        (l-piece #2A((1 0 0 0)
                     (1 0 0 0)
                     (1 1 0 0)
                     (0 0 0 0)))))

;; TODO: Spawn a piece in the board (OR with the grid and the piece)
;; TODO: Rotation of the piece (we think is transposing the 4x4 matrix)
;; TODO: Advance time tick (for testing and in OPENGL)
;; TODO: Treat user input
;; TODO: Treat collision with boundaries of the board
;; TODO: Treat collision with pieces and the grid
;; TODO: Start the OPENGL journey