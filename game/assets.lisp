(in-package #:letris)

;; FIXME Learn how to properly interact with harmony/trial-harmony
(define-global +music-path+ #p"data/tetris-rush.ogg")

(define-pool letris-assets)

#||
(defmacro define-track (name file &rest args)
  `(define-asset (letris-assets ,name) trial-harmony:sound
       ,file
     ,@args
     :name ,(file-namestring file)
     :repeat T
     :mixer :music
     :voice-class 'harmony:music-segment))

(define-track background-rush #p"tetris-rush.ogg")
(define-track background-classic #p"tetris-classic.ogg")

(define-asset (letris-assets music/background) trial-harmony:environment
    '((:option1 "background-rush tetris-rush.ogg")
      (:option2 "background-classic tetris-classic.ogg")))
||#

(define-asset (letris-assets tileset) image
    #p"tileset.png")


(define-asset (letris-assets square) mesh
    (make-rectangle-mesh 200 200))

(define-asset (letris-assets drn-button) image
  #p"drn-button.png")

(define-shader-entity drn-button (vertex-entity textured-entity located-entity listener)
  ((vertex-array :initform (// 'letris-assets 'square))
   (texture :initform (// 'letris-assets 'drn-button))))
