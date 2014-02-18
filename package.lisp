;;;; package.lisp

(defpackage #:cl-stripe-client
  (:use #:cl #:alexandria #:serapeum)
  (:nicknames #:stripe)
  (:shadow #:get #:delete)
  (:export #:*api-key*
           #:*api-version*
           #:*api-base*
           #:*charset*
           #:id
           #:deletedp
           #:stripe-error
           #:stripe-error-message
           #:stripe-error-code
           #:stripe-error-param
           #:stripe-error-url
           #:api-connection-error
           #:invalid-request-error
           #:authentication-error
           #:api-error
           #:card-error
           #:retrieve-account
           #:retrieve-balance
           #:retrieve-balance-transaction
           #:list-balance-transactions
           #:list-cards
           #:refund-charge
           #:capture-charge
           #:update-dispute
           #:close-dispute
           #:create-card
           #:update-subscription
           #:cancel-subscription
           #:delete-discount
           #:upcoming-invoice
           #:pay-invoice))
