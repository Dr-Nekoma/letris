;;;; package.lisp

(defpackage #:letris
  (:use #:cl+trial)
  (:shadow #:main #:launch)
  (:local-nicknames
   (#:v #:org.shirakumo.verbose))
  (:export #:main #:launch))
