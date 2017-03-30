(defpackage #:ip
  (:use #:common-lisp)
  (:export #:ip-address
           #:ip-network
           #:ipv4-address
           #:ipv4-address-from-quad
           #:ipv4-address-bits
           #:ipv4-network
           #:ipv4-network-bits
           #:ipv4-network-prefix
           #:parse-address
           #:address-in-network-p
           #:addresses-same-p
           #:count-addresses
           #:map-addresses
           #:enumerate-network
           #:to-inet-address))
