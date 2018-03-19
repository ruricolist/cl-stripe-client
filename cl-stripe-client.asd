;;;; cl-stripe-client.asd

(asdf:defsystem #:cl-stripe-client
  :serial t
  :description "Client for Stripe"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-stripe-client/test)))
  :version "1.0.0"
  :depends-on (#:alexandria
               #:serapeum
               #:puri
               #:drakma
               #:yason
               #:wu-decimal)
  :components ((:file "package")
               (:file "model")))

(asdf:defsystem #:cl-stripe-client/test
  :serial t
  :description "Tests for CL-STRIPE-CLIENT."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :perform (asdf:test-op (o c) (uiop:symbol-call :cl-stripe-client.test :run-tests))
  :depends-on (#:cl-stripe-client
               #:fiveam)
  :components ((:file "tests")))

