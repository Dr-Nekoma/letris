(in-package #:letris)


(defun inside-p (region point)
  (and (<= (gamekit:x region) (gamekit:x point) (gamekit:z region))
       (<= (gamekit:y region) (gamekit:y point) (gamekit:w region))))
