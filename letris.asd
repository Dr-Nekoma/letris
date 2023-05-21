;;;; letris.asd

(asdf:defsystem #:letris
  :description "Describe letris here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:module "game"
                  :components
                  ((:file "draw")
                   (:file "pieces")
                   (:file "logic")))
               (:file "main")
               (:file "letris")))
