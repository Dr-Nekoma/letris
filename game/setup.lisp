(in-package #:letris)

(define-global +settings+
    (copy-tree '(:audio (:latency 0.005
                         :backend :default
                         :device :default
                         :volume (:master 0.5
                                  :effect 1.0
                                  :music 1.0)))))


(define-action-set in-game)
(define-action move (directional-action in-game))

(defun launch (&rest args)
  (let ((*package* #.*package*))
    (load-keymap :path (asdf:system-relative-pathname "letris" "keymap.lisp"))
    (setf (active-p (action-set 'in-game)) T)
    (float-features:with-float-traps-masked (:divide-by-zero)
      (apply #'trial:launch 'main args))))

(progn
  (defmethod setup-scene ((main main) scene)
    (setf (title *context*) "Letris")
    (let* ((w (width *context*))
           (h (height *context*))
           (board (make-default-board h w)))
      (enter board scene)
      (enter (make-instance '2d-camera) scene)
      (enter (make-instance 'render-pass) scene)))
  (maybe-reload-scene))
