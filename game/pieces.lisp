(in-package #:letris)

(defun make-board () (make-array '(24 10)))

(defparameter initial-piece-position '(0 4))

(defclass piece ()
  ((kind
    :initarg :kind
    :accessor kind)
   (pos
    :initarg :pos
    :initform initial-piece-position
    :accessor pos)
   (representation
    :initarg :representation
    :initform nil
    :accessor representation)))

(defmethod copy ((obj array))
  (let ((res (make-array (array-dimensions obj))))
    (loop :for i :below (array-total-size obj)
          :do (setf (row-major-aref res i) (copy (row-major-aref obj i))))
    res))

(defmethod copy ((obj number))
  obj)

(defmethod copy ((obj list))
  (map 'list #'copy obj))

(defmethod copy ((obj piece))
  (let ((new (allocate-instance (find-class 'piece))))
    (setf (representation new) (copy (representation obj))
          (pos new) (copy (pos obj))
          (kind new) (kind obj))
    new))

(defun make-representation (kind)
  (case kind
    (t-piece (make-array '(3 3)
                         :initial-contents #(#(0 1 0)
                                             #(1 1 1)
                                             #(0 0 0))))
    (s-piece (make-array '(3 3)
                         :initial-contents #(#(0 2 2)
                                             #(2 2 0)
                                             #(0 0 0))))
    (i-piece (make-array '(4 4)
			 :initial-contents #(#(0 0 0 0)
					     #(3 3 3 3)
					     #(0 0 0 0)
					     #(0 0 0 0))))
    (j-piece (make-array '(3 3)
			 :initial-contents #(#(4 4 0)
					     #(4 0 0)
					     #(4 0 0))))
    (o-piece (make-array '(4 4)
			 :initial-contents #(#(0 0 0 0)
					     #(0 5 5 0)
					     #(0 5 5 0)
					     #(0 0 0 0))))
    (z-piece (make-array '(3 3)
			 :initial-contents #(#(6 6 0)
					     #(0 6 6)
					     #(0 0 0))))
    (l-piece (make-array '(3 3)
			 :initial-contents #(#(7 0 0)
					     #(7 0 0)
					     #(7 7 0))))))

(defmethod initialize-instance :after ((obj piece) &key)
  (setf (representation obj) (make-representation (kind obj))))

(defun spawn (kind)
  (make-instance 'piece :kind kind))

(defun spawn-random-piece ()
  (let ((index (random 7)))
    (case index
      (0 (spawn 't-piece))
      (1 (spawn 's-piece))
      (2 (spawn 'i-piece))
      (3 (spawn 'j-piece))
      (4 (spawn 'o-piece))
      (5 (spawn 'z-piece))
      (6 (spawn 'l-piece)))))

(defun put-piece-in-spot (piece spot)
  (let ((representation (representation piece)))
    (destructuring-bind (h-piece w-piece) (array-dimensions representation)
			(loop :for i :from 0 :below h-piece
			      :do (loop :for j :below w-piece
					:do (setf (aref spot (+ i 1) (+ j 1)) (aref i j representation)))))
    spot))

(define-handler (board move) ()
  (with-slots (current-button) board
    (let ((key (key (source-event move))))
      (setf current-button key))))
