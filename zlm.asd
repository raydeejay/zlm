;;;; zlm.asd

(asdf:defsystem #:zlm
  :description "Describe zlm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "zscii")
               (:file "opcodes")
               (:file "cpu")
               (:file "zlm")))
