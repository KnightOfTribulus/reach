;;;; reach.asd

(asdf:defsystem #:reach
  :description "Describe reach here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("trib-utils" "cl-arrows" "petri" "trivia")
  :components ((:file "package")
               (:file "reach")))
