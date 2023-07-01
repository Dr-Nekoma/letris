;;;; letris.asd

(asdf:defsystem #:letris
  :description "Describe letris here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:trial
               :trial-glfw
               :trial-png)
  :components ((:file "package")
               (:file "util")
               (:file "main")
               (:module "game"
                  :components
                  ((:file "util")
                   (:file "draw")
                   (:file "pieces")
                   (:file "logic")))))
