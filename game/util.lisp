(in-package #:letris)


(defclass button ()
  ((coordinates :initform (gamekit:vec4 0 0 0 0)
                :accessor coordinates
                :initarg :coords)
   (name :initform ""
         :accessor name
         :initarg :name)))



(defun calc-font-size (area)
  (cond ((<= area 1200) 20)
        ((< 1200 area 2000) 32)
        (t 50)))
