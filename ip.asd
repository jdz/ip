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

(defsystem "ip/tests"
  :depends-on ("ip")
  :pathname "tests/"
  :components ((:file "ipv4"))
  :perform (test-op (operation component)
                    (symbol-call '#:ip.tests.ipv4 '#:run-tests)))
