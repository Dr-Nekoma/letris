(in-package #:letris)

(defmacro define-track (name file &rest args)
  `(define-asset (music ,name) trial-harmony:sound
       ,file
     ,@args
     :name ,(file-namestring file)
     :repeat T
     :mixer :music
     :voice-class 'harmony:music-segment))

(define-track background-rush #p"tetris-rush.mp3")
(define-track background-classic #p"tetris-classic.ogg")

(define-asset (music music/background) trial-harmony:environment
    '((:option1 "background-rush tetris-rush.ogg")
      (:option2 "background-classic tetris-classic.ogg")))
