;;;; cl-stripe-client.asd

(asdf:defsystem #:cl-stripe-client-test
  :serial t
  :description "Tests for CL-STRIPE-CLIENT."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:cl-stripe-client
               #:fiveam)
  :components ((:file "tests")))
