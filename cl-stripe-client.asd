;;;; cl-stripe-client.asd

(asdf:defsystem #:cl-stripe-client
  :serial t
  :description "Client for Stripe"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum
               #:puri
               #:drakma
               #:yason
               #:wu-decimal)
  :components ((:file "package")
               (:file "model")))
