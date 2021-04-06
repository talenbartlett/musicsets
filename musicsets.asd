;;;; musicsets.asd

(asdf:defsystem #:musicsets
  :description "This is an attempt to set to code some concepts from music set theory."
  :author "Talen"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria")
  :components ((:file "package")
               (:file "musicsets")))
