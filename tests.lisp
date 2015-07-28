(in-package #:cl)

(defpackage #:cl-stripe-client.test
  (:use #:cl #:fiveam #:cl-stripe-client)
  (:export #:run-tests))

(in-package #:cl-stripe-client.test)

(defparameter *test-api-key* "tGN0bIwXnHdwOa85VABjPdSn8nWY7G7I")

(defun run-tests ()
  (run! 'stripe))

(def-suite stripe)

(in-suite stripe)

(defun debug-test (test)
  (let ((5am:*debug-on-error* t)
        (5am:*debug-on-failure* t))
    (run! test)))

(defmacro deftest (name &body body)
  `(test (,name :suite stripe)
     (locally (declare (optimize debug))
       (let ((*api-key* *test-api-key*))
         ,@body))))

(defparameter *default-card*
  '(("number" . "4242424242424242")
    ("exp_month" . 12)
    ("exp_year" . 2015)
    ("cvc" . "123")
    ("name" . "Lisp Bindings Cardholder")
    ("address_line1" . "140 2nd Street")
    ("address_line2" . "4th Floor")
    ("address_city" . "San Francisco")
    ("address_zip" . "94105")
    ("address_state" . "CA")
    ("address_country" ."USA")))

(defparameter *default-charge*
  `(("amount" . 100)
    ("currency" . "usd")
    ("card" . ,*default-card*)))

(defparameter *default-token*
  `(("card" . ,*default-card*)))

(defparameter *default-customer*
  `(("card" . ,*default-card*)
    ("description" . "Lisp Bindings Customer")))

(defparameter *default-plan*
  '(("amount" . 100)
    ("currency" . "usd")
    ("interval" . "month")
    ("interval_count" . 2)
    ("name" . "Lisp Bindings Plan")))

(defparameter *default-coupon*
  '(("duration" . "once")
    ("percent_off" . 10)))

(defparameter *default-bank-account*
  '(("country" . "US")
    ("routing_number" . "110000000")
    ("account_number" . "000123456789")))

(defparameter *default-recipient*
  `(("name" . "Lisp Test")
    ("type" . "individual")
    ("tax_id" . "000000000")
    ("bank_account" . ,*default-bank-account*)))

(defparameter *default-transfer*
  '(("amount" . 100)
    ("currency" . "usd")))

(deftest account-retrieve
  (let ((account (retrieve-account)))
    (is (equal "test+bindings@stripe.com"
               (account-email account)))
    (is-false (account-charge-enabled account))
    (is-false (account-details-submitted account))
    (is-false (account-statement-descriptor account))
    (is (equal "usd"
               (first
                (account-currencies-supported account))))))

(deftest balance-retrieve
  (let ((balance (retrieve-balance)))
    (is (not (livep balance)))
    (is (= 1 (length (balance-pending balance))))
    (is (= 1 (length (balance-available balance))))))

(deftest charge-create
  (let ((charge (create-charge *default-charge*)))
    (is (not (charge-refunded charge)))))

(deftest balance-transaction-retrieval
  (create-charge *default-charge*)

  (let* ((balance-transactions (list-balance-transactions))
         (first (first balance-transactions)))
    (is (not (null balance-transactions)))
    (is-true (balance-transaction-status first))

    (let ((params `(("count" . 2))))
      (is (= 2 (length (list-balance-transactions params)))))

    (let ((retrieved (retrieve-balance-transaction first)))
      (is (equal (id retrieved) (id first)))
      (is (equal (balance-transaction-source retrieved)
                 (balance-transaction-source first))))))

(deftest charge-retrieve
  (let* ((created-charge (create-charge *default-charge*))
         (retrieved-charge (retrieve-charge (id created-charge))))
    (is (= (created created-charge) (created retrieved-charge)))
    (is (equal (id created-charge)
               (id retrieved-charge)))))

(deftest charge-refund
  (let ((charge (refund-charge
                 (create-charge *default-charge*))))
    (is (charge-refunded charge))
    (let ((refunds (charge-refunds charge)))
      (is (listp refunds))
      (is (= 1 (length refunds)))
      (is (typep (first refunds) 'refund)))))

(deftest charge-capture
  (let* ((options (acons "capture" "false" *default-charge*))
         (created (create-charge options)))
    (is (not (charge-captured created)))
    (let ((captured (capture-charge created)))
      (is (charge-captured captured)))))

(deftest charge-partial-refund
  (let* ((created (create-charge *default-charge*))
         (amount 50)
         (refunded (refund-charge created `(("amount" . ,amount)))))
    (is (not (charge-refunded refunded)))
    (is (= (charge-amount-refunded refunded) amount))))

(deftest charge-list
  (is (= 1 (length (list-charges '(("count" . 1)))))))

(deftest invalid-card
  (let* ((invalid-card
           `(("number" . "4242424242424241")
             ("exp_month" . 12)
             ("exp_year" . 2015)))
         (invalid-charge
           (acons "card" invalid-card
                  *default-charge*)))
    (signals card-error
      (create-charge invalid-charge))))

(deftest invalid-address-zip
  (let* ((invalid-card
           '(("number" . "4000000000000036")
             ("address_zip" . "94024")
             ("address_line1" . "42 Foo Street")
             ("exp_month" . 12)
             ("exp_year" . 2015)))
         (invalid-charge
           (acons "card" invalid-card
                  *default-charge*)))
    (let ((charge (create-charge invalid-charge)))
      (is (charge-paid charge))
      (let ((card (charge-card charge)))
        (is (equal "fail" (card-address-zip-check card)))
        (is (equal "pass" (card-address-line1-check card)))))))

(deftest invalid-address-line1
  (let* ((invalid-card
           '(("number" . "4000000000000028")
             ("address_zip" . "94024")
             ("address_line1" . "42 Foo Street")
             ("exp_month" . 12)
             ("exp_year" . 2015)))
         (invalid-charge
           (acons "card" invalid-card
                  *default-charge*))
         (charge (create-charge invalid-charge)))
    (is (charge-paid charge))
    (let ((card (charge-card charge)))
      (is (equal "pass" (card-address-zip-check card)))
      (is (equal "fail" (card-address-line1-check card))))))

(deftest customer-create
  (let ((customer (create-customer *default-customer*)))
    (is (equal "Lisp Bindings Customer" (customer-description customer)))
    (let ((cards (customer-cards customer)))
      (is (= 1 (length cards)))
      (is (equal "4242" (card-last4 (first cards)))))))

(deftest customer-retrieve
  (let* ((created-customer (create-customer *default-customer*))
         (retrieved-customer (retrieve-customer (id created-customer))))
    (is (equal (created created-customer) (created retrieved-customer)))
    (is (equal (id created-customer) (id retrieved-customer)))))

(deftest customer-list
  (is (= 1 (length (list-customers '(("count" . 1)))))))

(deftest customer-update
  (let* ((created-customer
           (create-customer *default-customer*))
         (updated-customer
           (update-customer created-customer
                            '(("description" . "Updated Description")))))
    (is (equal "Updated Description" (customer-description updated-customer)))))

;; TODO
(deftest customer-update-to-blank
  (let ((created-customer (create-customer *default-customer*)))
    (signals invalid-request-error
      (update-customer created-customer '(("description" . ""))))))

;; TODO
(deftest customer-update-to-null
  (let* ((created-customer (create-customer *default-customer*))
         (updated-customer (update-customer created-customer `(("description" . nil)))))
    (is (null (customer-description updated-customer)))))

(deftest customer-card-addition
  (let* ((created-customer (create-customer *default-customer*))
         (original-default-card (customer-default-card created-customer))
         (added-card (create-card created-customer *default-card*))
         (token (create-token *default-token*)))
    (create-card created-customer (id token))
    (let ((updated-customer (retrieve-customer (id created-customer))))
      (is (= 3 (length (customer-cards updated-customer))))
      (is (equal (customer-default-card updated-customer)
                 original-default-card))
      (let* ((update-params `(("default_card" . ,(id added-card))))
             (customer-after-default-card-update (update-customer updated-customer update-params)))
        (is (= 3 (length (customer-cards customer-after-default-card-update))))
        (is (equal (customer-default-card customer-after-default-card-update)
                   (id added-card)))
        (is (equal original-default-card
                   (id (find original-default-card
                             (list-cards customer-after-default-card-update)
                             :test #'equal :key #'id))))
        (is (equal (id added-card)
                   (id (find (id added-card)
                             (list-cards customer-after-default-card-update)
                             :test #'equal :key #'id))))))))

(deftest customer-card-update
  (let* ((name "Lisp Bindings Cardholder, Jr.")
         (customer (create-customer *default-customer*))
         (original-card (first (customer-cards customer)))
         (updated-card (update-card original-card `(("name" . ,name)))))
    (is (equal name (card-name updated-card)))))

(deftest customer-card-delete
  (let ((customer (create-customer *default-customer*)))
    (create-card customer *default-card*)
    (let* ((card (first (customer-cards customer)))
           (deleted-card (delete-card card))
           (retrieved-customer (retrieve-customer (id customer))))
      (is (deletedp deleted-card))
      (is (equal (id deleted-card) (id card)))
      (dolist (retrieved-card (customer-cards retrieved-customer))
        (is (not (equal (id card) (id retrieved-card)))
            "Card ~a was not actually deleted" (id card))))))

(deftest customer-delete
  (let* ((created-customer (create-customer *default-customer*))
         (deleted-customer (delete-customer created-customer))
         (deleted-retrieved-customer (retrieve-customer (id created-customer))))
    (is (deletedp deleted-customer))
    (is (equal (id deleted-customer) (id created-customer)))
    (is (deletedp deleted-retrieved-customer))))

(deftest plan-create
  (let ((plan (create-plan (unique-plan))))
    (is (equal "month" (plan-interval plan)))
    (is (= 2 (plan-interval-count plan)))))

(deftest plan-update
  (let* ((name "Updated Plan Name")
         (plan (create-plan (unique-plan)))
         (updated-plan (update-plan plan `(("name" . ,name)))))
    (is (equal name (plan-name updated-plan)))))

(deftest plan-retrieve
  (let* ((created-plan (create-plan (unique-plan)))
         (retrieved-plan (retrieve-plan (id created-plan))))
    (is (equal (id created-plan)
               (id retrieved-plan)))))

(deftest plan-list
  (is (= 1 (length (list-plans '(("count" . 1)))))))

(deftest plan-delete
  (let* ((created-plan (create-plan (unique-plan)))
         (deleted-plan (delete-plan created-plan)))
    (is (deletedp deleted-plan))
    (is (equal (id deleted-plan) (id created-plan)))))

(deftest customer-create-with-plan
  (let* ((plan (create-plan (unique-plan)))
         (customer (create-default-customer/plan plan)))
    (is (equal (id (subscription-plan (customer-subscription customer)))
               (id plan)))))

(deftest update-subscription
  (let* ((plan (create-plan (unique-plan)))
         (customer (create-customer *default-customer*))
         (subscription (update-subscription customer
                                            `(("plan" . ,(id plan))))))
    (is (equal (id (subscription-plan subscription))
               (id plan)))
    (is (equal (subscription-customer subscription)
               (id customer)))))

(deftest cancel-subscription
  (let* ((plan (create-plan (unique-plan)))
         (customer (create-default-customer/plan plan)))
    (is (equal "active" (subscription-status (customer-subscription customer))))
    (let ((canceled-subscription
            (cancel-subscription customer)))
      (is (equal "canceled" (subscription-status canceled-subscription))))))

(deftest cancel-subscription-at-period-end
  (let* ((plan (create-plan (unique-plan)))
         (customer (create-default-customer/plan plan)))
    (is (equal "active" (subscription-status (customer-subscription customer))))
    (let ((canceled-subscription
            (cancel-subscription customer :at-period-end t)))
      (is (equal "active" (subscription-status canceled-subscription)))
      (is (subscription-cancel-at-period-end canceled-subscription)))))

(deftest invoice-item-create
  (let* ((customer (create-customer *default-customer*))
         (invoice-item (create-default-invoice-item customer)))
    (is (= 100 (invoice-item-amount invoice-item)))))

(deftest invoice-item-retrieve
  (let* ((customer (create-customer *default-customer*))
         (created-invoice-item (create-default-invoice-item customer))
         (retrieved-invoice-item (retrieve-invoice-item (id created-invoice-item))))
    (is (equal (id retrieved-invoice-item) (id created-invoice-item)))))

(deftest invoice-item-list
  (is (= 1 (length (list-invoice-items '(("count" . 1)))))))

(deftest invoice-item-update
  (let* ((description "Updated Description")
         (customer (create-customer *default-customer*))
         (created-invoice-item (create-default-invoice-item customer))
         (updated-invoice-item
           (update-invoice-item created-invoice-item
                                `(("description" . ,description)
                                  ("amount" . 200)))))
    (is (= (invoice-item-amount updated-invoice-item) 200))
    (is (equal description (invoice-item-description updated-invoice-item)))))

(deftest invoice-item-delete
  (let* ((customer (create-customer *default-customer*))
         (created-invoice-item (create-default-invoice-item customer))
         (deleted-invoice-item (delete-invoice-item created-invoice-item)))
    (is (deletedp deleted-invoice-item))
    (is (equal (id deleted-invoice-item) (id created-invoice-item)))))

(deftest invoice-list-and-retrieve
  (let ((plan (create-plan (unique-plan))))
    (create-default-customer/plan plan)
    (let* ((created-invoice (first (list-invoices '(("count" . 1)))))
           (retrieved-invoice (retrieve-invoice (id created-invoice))))
      (is (equal (id created-invoice) (id retrieved-invoice)))
      (is-true (invoice-lines retrieved-invoice)))))

(deftest invoice-retrieve-for-customer
  (let* ((plan (create-plan (unique-plan)))
         (customer (create-default-customer/plan plan))
         (invoice (first (list-invoices `(("customer" . ,(id customer))
                                          ("count" . 1))))))
    (is (equal (invoice-customer invoice) (id customer)))))

(deftest upcoming-invoice
  (let ((customer (create-customer *default-customer*)))
    (create-default-invoice-item customer)
    (let ((upcoming-invoice (upcoming-invoice `(("customer" . ,(id customer))))))
      (is (not (invoice-attempted upcoming-invoice))))))

(deftest token-create
  (let ((token (create-token *default-token*)))
    (is (not (token-used token)))))

(deftest token-retrieve
  (let* ((created-token (create-token *default-token*))
         (retrieved-token (retrieve-token (id created-token))))
    (is (equal (id created-token) (id retrieved-token)))))

(deftest token-use
  (let* ((created-token (create-token *default-token*)))
    (create-charge `(("amount" . 199)
                     ("currency" . "usd")
                     ("card" . ,(id created-token))))
    (let ((retrieved-token (retrieve-token (id created-token))))
      (is (token-used retrieved-token)))))

(deftest coupon-create
  (let ((coupon (create-coupon (unique-coupon))))
    (is (equal "once" (coupon-duration coupon)))))

(deftest coupon-retrieve
  (let* ((created (create-coupon (unique-coupon)))
         (retrieved (retrieve-coupon (id created))))
    (is (equal (id created) (id retrieved)))))

(deftest coupon-list
  (is (= 1 (length (list-coupons '(("count" . 1)))))))

(deftest coupon-delete
  (let* ((created (create-coupon (unique-coupon)))
         (deleted (delete-coupon created)))
    (is (deletedp deleted))
    (is (equal (id deleted) (id created)))))

(deftest customer-create-with-coupon
  (let* ((coupon (create-coupon (unique-coupon)))
         (customer (create-customer `(("coupon" . ,(id coupon))))))
    (is (equal (id (discount-coupon (customer-discount customer)))
               (id coupon)))
    (delete-discount customer)
    (is (not (customer-discount (retrieve-customer (id customer)))))))

(deftest transfer-create
  (let* ((recipient (create-recipient *default-recipient*))
         (transfer (create-transfer
                    (acons "recipient" (id recipient)
                           *default-transfer*))))
    (is (equal "pending" (transfer-status transfer)))))

(deftest transfer-retrieve
  (let* ((recipient (create-recipient *default-recipient*))
         (created (create-transfer (acons "recipient" (id recipient)
                                          *default-transfer*)))
         (retrieved (retrieve-transfer (id created))))
    (is (equal (transfer-date created) (transfer-date retrieved)))
    (is (equal (id created) (id retrieved)))))

(deftest transfer-list
  (is (= 1 (length (list-transfers '(("count" . 1)))))))

(deftest recipient-create
  (let ((recipient (create-recipient *default-recipient*)))
    (is (equal "6789"
               (bank-account-last4
                (recipient-active-account
                 recipient))))
    (is (typep recipient 'recipient))))

(deftest recipient-retrieve
  (let* ((created (create-recipient *default-recipient*))
         (retrieved (retrieve-recipient (id created))))
    (is (equal (created created) (created retrieved)))
    (is (equal (id created) (id retrieved)))
    (let ((account (recipient-active-account retrieved)))
      (is (typep account 'bank-account))
      (is (not (bank-account-validated account))))))

(deftest recipient-list
  (is (= 1 (length (list-recipients '(("count" . 1)))))))

(deftest recipient-update
  (let* ((description "Updated Description")
         (created (create-recipient *default-recipient*))
         (updated (update-recipient created `(("description" . ,description)))))
    (is (equal description (recipient-description updated)))))

(deftest recipient-delete
  (let* ((created (create-recipient *default-recipient*))
         (deleted (delete-recipient created))
         (deleted+retrieved (retrieve-recipient (id created))))
    (is (deletedp deleted))
    (is (equal (id deleted) (id created)))
    (is (deletedp deleted+retrieved))))

(deftest event-retrieve
  (let* ((event (first (list-events '(("count" . 1)))))
         (retrieved (retrieve-event (id event))))
    (is (equal (id event) (id retrieved)))))

(deftest event-list
  (is (= 1 (length (list-events '(("count" . 1)))))))

(defun unique-plan ()
  (acons "id" (unique-plan-id)
         (copy-alist *default-plan*)))

(defun unique-coupon ()
  (acons "id" (unique-coupon-id)
         (copy-alist *default-coupon*)))

(defun create-default-invoice-item (customer)
  (create-invoice-item `(("amount" . 100)
                         ("currency" . "usd")
                         ("customer" . ,(id customer)))))

(defun create-default-customer/plan (plan)
  (create-customer
   (acons "plan" (id plan)
          (copy-alist *default-customer*))))

(defun create-default-recipient ()
  (create-recipient
   (copy-alist *default-recipient*)))

(defun unique-plan-id ()
  (string (gensym "PLAN")))

(defun unique-coupon-id ()
  (string (gensym "COUPON")))
