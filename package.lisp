;;;; package.lisp

(defpackage #:letris
  (:use #:cl+trial)
  (:shadow #:main #:launch)
  (:local-nicknames
   (#:v #:org.shirakumo.verbose)
   (#:trial-harmony #:org.shirakumo.fraf.trial.harmony)
   (#:harmony #:org.shirakumo.fraf.harmony.user))
  (:export #:main #:launch))
