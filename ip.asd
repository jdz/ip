(asdf:defsystem #:ip
  :description "Internet Protocol address utilities"
  :author "Jānis Džeriņš <smuglispweenie@gmail.com>"
  :version "0.1"
  :license "MIT"
  :long-description "Data structures and functions to work with IP addresses."
  :depends-on ("cl-ppcre")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "ip")))))
