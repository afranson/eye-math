;;;; eye-math.asd

(asdf:defsystem #:eye-math
  :description "Performs extraction of paramters necessary for assesing eye health"
  :author "Andrew Franson"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "eye-math")))
