;;;; letris.asd

(asdf:defsystem #:letris
  :description "A simple Tetris game"
  :author "drnekoma@gmail.com"
  :license  "MIT"
  :version "2.0"
  :serial t
  :depends-on (:trial
               :trial-glfw
               :trial-harmony
               :trial-png)
  :components ((:file "package")
               (:file "util")
               (:module "game"
                  :components
                  ((:file "util")
           (:file "logic")
           (:file "assets")
           (:file "game")
           (:file "draw")
           (:file "setup")
           (:file "pieces")))
           (:file "main")))
