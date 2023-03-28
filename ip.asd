(defsystem "ip"
  :description "Internet Protocol address utilities"
  :author "Jānis Džeriņš <smuglispweenie@gmail.com>"
  :version "0.2"
  :license "zlib"
  :long-description "Data structures and functions to work with IP addresses."
  :pathname "src/"
  :components ((:file "package")
               (:file "ip" :depends-on ("package")))
  :in-order-to ((test-op (test-op "ip/tests"))))
