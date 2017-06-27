(in-package #:ip)

(define-condition invalid-ip (parse-error)
  ((string
    :initarg :string
    :reader invalid-ip-string))
  (:report (lambda (condition stream)
             (format stream "Invalid IP address/network: ~A"
                     (invalid-ip-string condition)))))

(define-condition invalid-ip-address (parse-error)
  ((string
    :initarg :string
    :reader invalid-ip-string))
  (:report (lambda (condition stream)
             (format stream "Invalid IP address: ~A"
                     (invalid-ip-string condition)))))

(define-condition invalid-ip-network (parse-error)
  ((string
    :initarg :string
    :reader invalid-ip-string))
  (:report (lambda (condition stream)
             (format stream "Invalid IP network: ~A"
                     (invalid-ip-string condition)))))

(defclass ip-network ()
  ()
  (:documentation "IP network protocol class."))

(defclass ip-address ()
  ()
  (:documentation "IP address protocol class."))

(defgeneric address-in-network-p (address network)
  (:documentation "Returns true if ADDRESS belongs to NETWORK."))

(defgeneric addresses-same-p (address1 address2)
  (:documentation "Returns true if ADDRESS1 is the same as ADDRESS2."))

(defgeneric count-addresses (network)
  (:documentation "Returns the number of addresses in NETWORK."))

(defgeneric enumerate-network (network)
  (:documentation "Returns a function that returns all addresses in
  NETWORK (sequentially), or NIL if invoked after all address have
  been returned."))

(defgeneric map-addresses (function network)
  (:documentation "Calls FUNCTION on each address of NETWORK."))

(defgeneric to-inet-address (address)
  (:documentation "Convert IP address ADDRESS to vector of
  octets (format used by sb-bsd-sockets)."))

(defclass IPv4-address (ip-address)
  ((bits
    :initarg :bits
    :reader IPv4-address-bits
    :type (unsigned-byte 32))))

(defmethod to-inet-address ((address IPv4-address))
  (with-accessors ((ip ipv4-address-bits))
      address
    (make-array 4 :element-type '(unsigned-byte 8)
                  :initial-contents `(,(ldb (byte 8 24) ip)
                                      ,(ldb (byte 8 16) ip)
                                      ,(ldb (byte 8 8) ip)
                                      ,(ldb (byte 8 0) ip)))))

(defmethod to-inet-address ((address string))
  (to-inet-address (parse-address address)))

(defmethod print-object ((object IPv4-address) stream)
  (flet ((print-it (stream)
           (with-accessors ((ip IPv4-address-bits))
               object
             (format stream "~D.~D.~D.~D"
                     (ldb (byte 8 24) ip)
                     (ldb (byte 8 16) ip)
                     (ldb (byte 8 8) ip)
                     (ldb (byte 8 0) ip)))))
    (if (or *print-escape* *print-readably*)
        (print-unreadable-object (object stream :type t)
          (print-it stream))
        (print-it stream))))

(defun IPv4-address (bits)
  "IPv4 address constructor."
  (make-instance 'ipv4-address :bits bits))

(defun IPv4-address-from-quad (a b c d)
  "Another IPv4 address constructor.  Callers better make sure that
the parameters are of type (unsigned-byte 8)."
  (declare (type (unsigned-byte 8) a b c d))
  (let ((bits (logior (ash a 24) (ash b 16) (ash c 8) d)))
    (make-instance 'ipv4-address :bits bits)))

(defclass IPv4-network (ip-network)
  ((bits
    :initarg :bits
    :reader IPv4-network-bits
    :type (unsigned-byte 32))
   (prefix
    :initarg :prefix
    :reader IPv4-network-prefix
    :type (integer 1 32))))

(defmethod print-object ((object IPv4-network) stream)
  (flet ((print-it (stream)
           (with-accessors ((ip IPv4-network-bits)
                            (prefix IPv4-network-prefix))
               object
             (format stream "~D.~D.~D.~D/~D"
                     (ldb (byte 8 24) ip)
                     (ldb (byte 8 16) ip)
                     (ldb (byte 8 8) ip)
                     (ldb (byte 8 0) ip)
                     prefix))))
    (if (or *print-escape* *print-readably*)
        (print-unreadable-object (object stream :type t)
          (print-it stream))
        (print-it stream))))

(defun IPv4-network (bits prefix)
  "IPv4 network constructor."
  (make-instance 'IPv4-network :bits bits :prefix prefix))

(defun valid-ip-p (string &optional (kind :guess))
  (handler-case
      (multiple-value-bind (ip kind)
          (parse-address string kind)
        (declare (ignore ip))
        kind)
    (invalid-ip ()
      nil)))

(defun parse-address (string &optional (kind :guess))
  "Parse IP address (currently only IPv4) or network from STRING.
KIND can be one of: :address, :network or :guess."
  (check-type string string)
  (check-type kind (member :address :network :guess))
  (ppcre:register-groups-bind ((#'parse-integer a b c d prefix))
      ("^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})(?:/(\\d{1,2}))?$"
       string)
    (unless (and (<= 1 a 255)
                 (<= 0 b 255)
                 (<= 0 c 255)
                 (<= 0 d 255)
                 (or (null prefix) (<= 1 prefix 32)))
      ;; XXX: Add a USE-VALUE restart?
      (error 'invalid-ip :string string))
    (let ((bits (logior (ash a 24) (ash b 16) (ash c 8) d)))
      (return-from parse-address
        (ecase kind
          (:address
           (when prefix
             (error 'invalid-ip-address :string string))
           (values (IPv4-address bits)
                   :address))
          (:network
           (unless prefix
             (error 'invalid-ip-network :string string))
           (values (IPv4-network (dpb 0 (byte (- 32 prefix) 0) bits) prefix)
                   :network))
          (:guess
           (if prefix
               (values (IPv4-network (dpb 0 (byte (- 32 prefix) 0) bits) prefix)
                       :network)
               (values (IPv4-address bits)
                       :address)))))))
  ;; XXX: Should not use register-groups-bind but one of the scan
  ;; functions so that we don't have to repeat this checking.
  (error (ecase kind
           (:address 'invalid-ip-address)
           (:network 'invalid-ip-network)
           (:guess 'invalid-ip))
         :string string))

(defmethod address-in-network-p ((address IPv4-address) (network IPv4-network))
  (let ((address-bits (ipv4-address-bits address))
        (network-bits (ipv4-network-bits network))
        (prefix (ipv4-network-prefix network)))
    (zerop (logxor network-bits
                   (mask-field (byte prefix (- 32 prefix)) address-bits)))))

(defmethod addresses-same-p ((address1 IPv4-address) (address2 IPv4-address))
  (= (ipv4-address-bits address1)
     (ipv4-address-bits address2)))

(defmethod count-addresses ((network IPv4-network))
  (1+ (ldb (byte (- 32 (ipv4-network-prefix network)) 0)
           #xFFFFFFFF)))

(defmethod map-addresses (function (network IPv4-network))
  (loop with enumerator = (enumerate-network network)
        for address = (funcall enumerator)
        while address
        do (funcall function address)))

(defmethod enumerate-network ((network IPv4-network))
  (let* ((bits (ipv4-network-bits network))
         (prefix (ipv4-network-prefix network))
         (naddrs (ldb (byte (- 32 prefix) 0) #xFFFFFFFF))
         (i 0))
    #'(lambda ()
        (cond ((<= i naddrs)
               (multiple-value-prog1
                   (values (make-instance 'IPv4-address :bits (logior bits i))
                           t)
                 (incf i)))
              (t
               (values nil nil))))))
