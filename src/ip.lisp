(in-package #:ip)

;;; Until we have a real need to use Alexandria.
(deftype array-index ()
  `(integer 0 (,(1- array-dimension-limit))))

(deftype array-length ()
  `(integer 0 ,(1- array-dimension-limit)))

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

;;; XXX: Rename to to-byte-array?
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
  (to-inet-address (parse-ipv4-address address)))

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

(defun %parse-ipv4-address (string start end junk-allowed)
  (declare (type string string)
           (type array-index start)
           (type array-length end)
           #-sbcl (optimize (speed 3) (safety 1)))
  (let ((pos start))
    (declare (type array-index pos))
    (labels ((done (value)
               (return-from %parse-ipv4-address
                 (values value pos)))
             (invalid ()
               (error 'invalid-ip-address :string (subseq string start end)))
             (parse-digits (limit)
               (when (<= end pos)
                 (invalid))
               (let ((char (aref string pos)))
                 (case char
                   (#\0
                    (incf pos)
                    (cond ((<= end pos)
                           (values 0 nil))
                          (t
                           (setq char (aref string pos))
                           (case char
                             (#\x
                              (incf pos)
                              (parse-hex limit))
                             (#\.
                              (values 0 t))
                             (t
                              (parse-oct limit char))))))
                   ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                    (parse-dec limit char))
                   (t
                    (invalid)))))
             (parse-dec (limit char)
               (let ((x 0))
                 (declare (type (integer 0 #.(+ (* #xFFFFFFFF 10) 9)) x))
                 (loop
                   (case char
                     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                      (setq x (+ (* x 10)
                                 (- (char-code char) #.(char-code #\0))))
                      (unless (<= x limit)
                        (invalid))
                      (incf pos)
                      (when (<= end pos)
                        (return (values x nil)))
                      (setq char (aref string pos)))
                     (#\.
                      (return (values x t)))
                     (t
                      (if junk-allowed
                          (return (values x nil))
                          (invalid)))))))
             (parse-oct (limit char)
               (let ((x 0))
                 (declare (type (integer 0 #.(+ (* #xFFFFFFFF 10) 9)) x))
                 (loop
                   (case char
                     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                      (setq x (+ (* x 8)
                                 (- (char-code char) #.(char-code #\0))))
                      (unless (<= x limit)
                        (invalid))
                      (incf pos)
                      (when (<= end pos)
                        (return (values x nil)))
                      (setq char (aref string pos)))
                     (#\.
                      (return (values x t)))
                     (t
                      (if junk-allowed
                          (return (values x nil))
                          (invalid)))))))
             (parse-hex (limit)
               (let ((x 0)
                     (char (aref string pos)))
                 (declare (type (integer 0 #.(+ (* #xFFFFFFFF 10) 9)) x)
                          (type character char))
                 (loop
                   (case char
                     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                           #\a #\b #\c #\d #\e #\f
                           #\A #\B #\C #\D #\E #\F)
                      (setq x (+ (* x 16) (digit-char-p char 16)))
                      (unless (<= x limit)
                        (invalid))
                      (incf pos)
                      (when (<= end pos)
                        (return (values x nil)))
                      (setq char (aref string pos)))
                     (#\.
                      (return (values x t)))
                     (t
                      (if junk-allowed
                          (return (values x nil))
                          (invalid))))))))
      (declare (inline done parse-dec parse-oct parse-hex))
      (let ((bits 0))
        (declare (type (unsigned-byte 32) bits))
        (multiple-value-bind (x more)
            (parse-digits #xFFFFFFFF)
          (cond ((not more)
                 (done x))
                ((<= x #xFF)
                 (incf pos)
                 (setq bits (ash x 24)))
                (t
                 (invalid))))
        (multiple-value-bind (x more)
            (parse-digits #xFFFFFF)
          (cond ((not more)
                 (done (logior bits x)))
                ((<= x #xFF)
                 (incf pos)
                 (setq bits (logior bits (ash x 16))))
                (t
                 (invalid))))
        (multiple-value-bind (x more)
            (parse-digits #xFFFF)
          (cond ((not more)
                 (done (logior bits x)))
                ((<= x #xFF)
                 (incf pos)
                 (setq bits (logior bits (ash x 8))))
                (t
                 (invalid))))
        (multiple-value-bind (x more)
            (parse-digits #xFF)
          (cond ((or (not more)
                     junk-allowed)
                 (done (logior bits x)))
                (t
                 (invalid))))))))

(defun parse-ipv4-address (string &key (start 0) end junk-allowed)
  (multiple-value-bind (bits pos)
      (%parse-ipv4-address string start (or end (length string)) junk-allowed)
    (values (ipv4-address bits)
            pos)))

(defun parse-ipv4-network (string &key (start 0) end junk-allowed)
  (flet ((invalid ()
           (error 'invalid-ip-network :string (subseq string start end))))
    (let ((end (or end (length string))))
      (multiple-value-bind (bits pos)
          (%parse-ipv4-address string start end t)
        (unless (and (< (1+ pos) end)
                     (char= #\/ (char string pos))
                     (digit-char-p (char string (1+ pos))))
          (invalid))
        (let ((prefix-length (parse-integer string :start (1+ pos)
                                                   :end end
                                                   :junk-allowed junk-allowed)))
          (if (<= prefix-length 32)
              (values (ipv4-network (dpb 0 (byte (- 32 prefix-length) 0) bits)
                                    prefix-length))
              (invalid)))))))

(defun valid-ipv4-address-p (string &key (start 0) end junk-allowed)
  (declare (type string string)
           (type array-index start)
           (type (or null array-length) end))
  (handler-case
      (%parse-ipv4-address string start (or end (length string)) junk-allowed)
    (invalid-ip-address ()
      nil)))

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
