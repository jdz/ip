(defpackage #:ip.tests.ipv4
  (:use #:common-lisp)
  (:export #:run-tests))

(in-package #:ip.tests.ipv4)

(defun parse-ipv4-address ()
  (flet ((test (expected-output input &rest args)
           (let ((result (apply #'ip:parse-ipv4-address input args)))
             (assert (string= expected-output (princ-to-string result))))))
    (test "192.0.2.235"   "192.0.2.235")
    (test "192.0.2.235"   "3221226219")
    (test "127.0.0.1"     "127.1")
    (test "127.1.0.1"     "127.1.1")
    (test "127.0.255.250" "127.65530")
    (test "127.1.0.1"     "127.65537")
    (test "0.0.0.1"       "0.1")
    (test "0.1.0.1"       "0.1.1")
    (test "127.0.0.1"     "a127.1z" :start 1 :end 6)
    (test "127.0.0.1"     "127.1/8" :junk-allowed t)
    t))

(defun run-tests ()
  (parse-ipv4-address))
