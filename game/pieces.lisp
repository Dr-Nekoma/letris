

(in-package #:letris)


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
