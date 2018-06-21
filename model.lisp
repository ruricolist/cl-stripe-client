(in-package #:cl-stripe-client)

(defparameter *api-base*
  (puri:parse-uri "https://api.stripe.com/v1/")
  "The base URI of Stripe's API.")

(defparameter *charset* :utf-8
  "The desired charset.")

(defvar *api-version* "2013-12-03"
  "The supported Stripe API version.")

(defvar *api-key*)
(setf (documentation '*api-key* 'variable)
      "The current Stripe API key.")

(defgeneric serialize-slots (object)
  (:method-combination progn :most-specific-last)
  (:documentation "Responsible for serializing the direct slots of models.

Rather than using the MOP to walk the slots of each class, instead, as
part of `defmodel', we define a method on `serialize-slots'."))

(defgeneric id (x)
  (:method ((x string)) x)
  (:documentation "ID of an object; a string stands for its object."))

(defgeneric deletedp (x)
  (:method (x)
    (declare (ignore x)))
  (:documentation "Has this object been deleted?"))

(defun get-api-key ()
  "Return the current API key.
If no API key is set, signal a continuable error."
  (unless (boundp '*api-key*)
    (cerror "Provide an API key"
            "The API key is unbound")
    (setf *api-key* (read))
    (check-type *api-key* string))
  *api-key*)


;;;; API error conditions.

(defcondition stripe-error (error)
  ((message :initarg :message :accessor stripe-error-message)
   (code :initarg :code :accessor stripe-error-code)
   (param :initarg :param :accessor stripe-error-param)
   (url :initarg :url :accessor stripe-error-url))
  (:report (lambda (c s)
             (with-slots (message) c
               (format s "Stripe error:~%~A" message))))
  (:documentation "A Stripe API error."))

(defcondition api-connection-error (stripe-error)
  ())

(defcondition invalid-request-error (stripe-error)
  ())

(defcondition authentication-error (stripe-error)
  ())

(defcondition api-error (stripe-error)
  ())

(defcondition card-error (stripe-error)
  ())

(defun handle-api-error (body status url)
  "Signal the appropriate Stripe error for BODY and STATUS."
  (let* ((json (yason:parse body))
         (error (gethash "error" json))
         (message (gethash "message" error))
         (code (gethash "code" error))
         (param (gethash "param" error)))
    (case status
      (400 (error 'invalid-request-error
                  :message message
                  :param param
                  :url url))
      (404 (error 'invalid-request-error
                  :message message
                  :param param
                  :url url))
      (401 (error 'authentication-error
                  :message message
                  :url url))
      (402 (error 'card-error
                  :message message
                  :param param
                  :code code
                  :url url))
      (t   (error 'api-error
                  :message message
                  :url url)))))


;;;; The defmodel macro.

(defmacro defmodel (class-name supers &body (slots &rest options))
  "Define a class for a Stripe object, sans boilerplate.
\"Boilerplate\" means initargs, accessors, serializers and,
optionally, trivial CRUD methods."
  (let ((slots (mapcar (lambda (slot)
                         (let ((slot (if (listp slot)
                                         (copy-list slot)
                                         (list slot))))
                           ;; Ensure an accessor.
                           (unless (get-properties (cdr slot) '(:accessor :reader :writer))
                             (setf (getf (cdr slot) :accessor)
                                   (symbolicate class-name "-" (car slot))))
                           ;; Ensure an initarg.
                           (unless (getf (cdr slot) :initarg)
                             (setf (getf (cdr slot) :initarg)
                                   (make-keyword (string (car slot)))))
                           slot))
                       slots))
        ;; Associate a deleted class with this class. An instance of
        ;; the deleted class is returned when an instance of this
        ;; class is deleted.
        (deleted-class (cadr (assoc :deleted-class options)))
        ;; Which trivial CRUD operations to implement?
        (simple-methods (cdr (assoc :simple-methods options))))
    `(progn
       (defclass ,class-name ,supers
         ;; Define the accessors in the macro, rather than the
         ;; metaclass, so they exist at compile time.
         (,@slots
          ,@(unsplice
             (when deleted-class
               `(deleted-class :allocation :class
                               :initform ',deleted-class
                               :reader deleted-class))))
         ,@(remove '(:simple-methods :deleted-class) options :key #'car :test (flip #'member)))
       ;; Compile the (partial) JSON serializer.
       ,@(unsplice
          (when slots
            `(defmethod serialize-slots progn ((self ,class-name))
               (with-slots ,(mapcar #'car slots) self
                 ,@(mapcar (lambda (name)
                             `(ignoring unbound-slot
                                (yason:encode-object-element
                                 ,(substitute #\_ #\- (string-downcase name))
                                 ,name)))
                           (mapcar #'car slots))))))
       ;; Define any trivial class methods.
       ,@(when simple-methods
           (setf simple-methods (nub simple-methods))
           (loop for m in simple-methods
                 for fn = (if (eql m 'list)
                             (symbolicate m '- class-name 's)
                             (symbolicate m '- class-name))
                 collect `(export ',fn)
                 collect (ecase m
                           (create `(defun ,fn (params)
                                      (create ',class-name params)))
                           (retrieve `(defun ,fn (,class-name)
                                        (retrieve ',class-name (id ,class-name))))
                           (list `(defun ,fn (&optional params)
                                    (list-all-aux ',class-name params)))
                           (update `(defun ,fn (self params)
                                      (update self params)))
                           (delete `(defun ,fn (self)
                                      (delete self))))))
       ;; Export the name of the class.
       (export ',class-name)
       ;; Export the accessors.
       ,@(unsplice
          (when slots
            `(export ',(remove-duplicates
                        (mappend
                         (lambda (slot)
                           (let ((plist (cdr slot)))
                             (cl:delete
                              nil
                              (list (getf plist :accessor)
                                    (getf plist :reader)
                                    (getf plist :writer)))))
                         slots)))))
       ;; Return the class name.
       ',class-name)))


;;;; The base classes for Stripe objects and resources.

(defclass stripe-object ()
  ((id :initarg :id :accessor id)
   (kind :initarg :kind :reader stripe-object-kind :type string)
   (deleted :initarg :deleted :reader deletedp))
  (:default-initargs :id "" :deleted nil)
  (:documentation "Base class of all Stripe objects."))

(defclass unrecognized-object (stripe-object)
  ((initargs :accessor unrecognized-object-initargs))
  (:documentation "Class for unrecognized objects; better to return a cipher than to crash."))

(defmethods unrecognized-object (self initargs kind)
  (:method initialize-instance (self &rest args &key &allow-other-keys)
    (setf initargs args))
  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~a ~s" kind initargs))))

(defmodel api-resource (stripe-object)
  ((livemode :initarg :livemode :accessor livep)
   (created :initarg :created :accessor created)
   (description :initarg :description :reader description)
   (metadata :initarg :metadata :reader metadata))
  (:documentation "Base class for API resources (Stripe objects with URLs)."))

(defmethod print-object ((self stripe-object) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "JSON: ")
    (yason:encode self stream)))

(defmethod yason:encode ((self stripe-object) &optional stream)
  (yason:with-output (stream :indent *print-pretty*)
    (yason:with-object ()
      (serialize-slots self))))

(defmethod serialize-slots progn ((self stripe-object))
  (ignoring unbound-slot
    (yason:encode-object-element "id" (id self))))


;;;; Making API requests.

(defmacro with-deadline (seconds &body body)
  "Compatibility wrapper for SBCL's `with-deadline'."
  (declare (ignorable seconds))
  #+sbcl `(sb-sys:with-deadline (:seconds ,seconds)
            ,@body)
  #-sbcl `(progn ,@body))

(defun request (method url &optional params)
  (check-type method (member :get :post :delete))
  (setf url (puri:merge-uris url *api-base*))
  (multiple-value-bind (body status)
      (handler-case
          (with-deadline 80
            (let ((drakma:*text-content-types* (list (cons "application" "json"))))
              (drakma:http-request url
                                   :allow-other-keys t
                                   :method method
                                   :basic-authorization `(,(get-api-key) "")
                                   :form-data nil
                                   :force-ssl t
                                   :connection-timeout 30
                                   :additional-headers `(("Accept-Charset" . ,*charset*)
                                                         ,@(if (boundp '*api-version*)
                                                               `(("Stripe-Version" . ,*api-version*))))
                                   :parameters (values (normalize-parameters params))
                                   ;; For CCL.
                                   :deadline (+ (get-internal-real-time)
                                                (* 80 internal-time-units-per-second)))))
        ((or drakma:drakma-error
          chunga:syntax-error
          usocket:socket-condition)
          (err)
          (error 'api-connection-error
                 :message (fmt "Could not connect to Stripe: ~a" err))))
    (if (<= 200 status 299)
        (json->object-tree body)
        (handle-api-error body status url))))

(defun normalize-parameters (alist)
  "Make sure everything is a string and expand any nested alists into
bracketed arrays."
  (flet ((stringify (x)
           (typecase x
             (string x)
             ((eql t) "true")
             ;; Stripe treats "" as null.
             ((eql nil) "")
             (symbol (substitute #\_ #\- (string-downcase x)))
             (t (fmt "~a" x))))
         (check-value (key value)
           (when (and (stringp value) (string= "" value))
             (error 'invalid-request-error
                    :message (format nil "You cannot set ~a to an ~
                    empty string. Stripe interprets empty strings as ~
                    null in requests. You may set ~:*~a to NIL to ~
                    delete the property." key)))))
    (let ((params (loop for (key . value) in alist
                        for skey = (stringify key)
                        do (check-value key value)
                        if (consp value)
                          nconc (loop for (k . v) in value
                                      do (check-value k v)
                                      collect (cons (fmt "~a[~a]" skey (stringify k))
                                                    (stringify v)))
                        else collect (cons skey (stringify value)))))
      (delete-duplicates params
                         :from-end t
                         :test #'equal
                         :key #'car))))

(assert (equal '(("number" . "123")
                 ("card[name]" . "Foo Barzenby")
                 ("card[address-line1]" . "123 Foo St")
                 ("card[address-line2]" . "Footown, USA"))
               (normalize-parameters
                '(("number" . 123)
                  ("card" . (("name" . "Foo Barzenby")
                             ("address-line1" . "123 Foo St")
                             ("address-line2" . "Footown, USA")))))))

(assert (equal '(("at_period_end" . "true"))
               (normalize-parameters '((at-period-end . t)))))
(assert (equal '(("at_period_end" . ""))
               (normalize-parameters '((at-period-end . nil)))))

(defun request-instance (class method url &optional params)
  (let ((data (request method url params)))
    (cond ((listp data)
           (plist->instance class data))
          (t (unless (typep data class)
               (error "~a is not of type ~a" data class))
             data))))

(defun request-update (object params &key (url (instance-url object)))
  (request-instance (class-name (class-of object)) :post url params))

(defmethod instance-url ((self api-resource))
  (id-url (class-of self) (id self)))

(defun id-url (class id)
  (check-type id string)
  (fmt "~a/~a" (class-url class) id))

(defun class-url (class)
  (remove #\- (fmt "~as" (single-class-url class))))

(defun single-class-url (class)
  (string-downcase (class-name (find-class-safe class))))

(defun key->keyword (key)
  "Convert a Stripe JSON object key to a keyword."
  (make-keyword (substitute #\- #\_ (string-upcase key))))

(defun plist->instance (class plist)
  (apply #'make-instance class :allow-other-keys t plist))

(defun json->object-tree (body)
  "Convert a Stripe response into a tree of Stripe objects."
  (labels ((kind-class (kind)
             (string-case kind
               ("invoiceitem" 'invoice-item)
               ("application_fee" 'application-fee)
               ("balance_transaction" 'balance-transaction)
               ("bank_account" 'bank-account)
               ("line_item" 'invoice-line-item)
               (otherwise (find-symbol (string-upcase kind) :cl-stripe-client))))
           (handle-object (object kind)
             (let* ((class-name (or (kind-class kind) 'unrecognized-object))
                    (class (find-class class-name t)))
               (apply #'make
                      class
                      :allow-other-keys t
                      :kind kind
                      (apply #'nconc
                             (maphash-return (lambda (k v)
                                               (list k (rec v)))
                                             object)))))
           (rec-event (ev)
             "Handle events, which use the :object key differently."
             (let ((data (@ ev :data)))
               (setf (@ ev :data)
                     (make-instance 'event-data
                                    :previous-attributes (rec (@ data :previous-attributes))
                                    :object (rec (@ data :object))))
               (rec ev t)))
           (rec (object &optional event?)
             (cond ((listp object)      ;E.g. balance.pending
                    (mapcar #'rec object))
                   ((not (hash-table-p object))
                    object)
                   (t (let ((kind (@ object :object)))
                        (cond ((not kind)
                               (hash-table-plist object))
                              ((and (not event?) (equal kind "event"))
                               (rec-event object))
                              ((equal kind "list")
                               (mapcar #'rec (@ object :data)))
                              (t (handle-object object kind))))))))
    (rec (yason:parse body :object-key-fn #'key->keyword))))


;;;; CRUD helpers.

(defun create (class params &key (url (class-url class)))
  (request-instance class :post url params))

(defun retrieve (class id &key (url (id-url class id)))
  (check-type id string)
  (request-instance class :get url))

(defun update (instance params &key (url (instance-url instance)))
  (request-update instance params :url url))

(defun delete (instance &key (url (instance-url instance))
                             (class (deleted-class instance)))
  (request-instance class :delete url))

(defun list-all-aux (class params &key (url (class-url class)))
  (request :get url params))


;;;; Money.
(defclass money ()
  ((amount :initarg :amount :accessor money-amount)
   (currency :initarg :currency :accessor currency)))

(defmethod print-object ((self money) stream)
  (with-slots (currency amount) self
    (print-unreadable-object (self stream)
      (format stream "~a ~/wu-decimal:f/" currency amount))))

(defmethod yason:encode ((self money) &optional stream)
  (with-slots (amount) self
    (yason:with-output (stream)
      (format stream "~/wu-decimal:f/" amount))))


;;;; The actual models.

(defmodel deleted-object (stripe-object)
  ())

(defmodel deleted-card (deleted-object)
  ())

(defmodel deleted-coupon (deleted-object)
  ())

(defmodel deleted-customer (deleted-object)
  ())

(defmodel deleted-invoice-item (deleted-object)
  ())

(defmodel deleted-plan (deleted-object)
  ())

(defmodel deleted-recipient (deleted-object)
  ())

(defmodel account (api-resource)
  (charge-enabled
   details-submitted
   currencies-supported
   email
   statement-descriptor)
  (:default-initargs
   :currencies-supported nil))

(defun retrieve-account ()
  (request-instance 'account :get (single-class-url 'account)))

(defmodel balance (api-resource)
  (pending available))

(defmethod shared-initialize :after ((self balance) slots &key pending available)
  (declare (ignore slots))
  ;; NB. These hashes don't have the "object" property.
  (flet ((plist->transaction (plist)
           (plist->instance 'balance-transaction plist)))
    (setf (balance-pending self)   (mapcar #'plist->transaction pending)
          (balance-available self) (mapcar #'plist->transaction available))))

(defun retrieve-balance ()
  (request-instance 'balance :get (single-class-url 'balance)))

(defmodel balance-transaction (api-resource)
  (source
   amount
   currency
   net
   type
   available-on
   status))

(defmethod retrieve-balance-transaction ((id string))
  (request-instance 'balance-transaction
                    :get
                    (fmt "balance/history/~a" id)))

(defmethod retrieve-balance-transaction ((self balance-transaction))
  (retrieve-balance-transaction (id self)))

(defun list-balance-transactions (&optional params)
  (list-all-aux 'balance-transaction params :url "balance/history"))

(defmodel bank-account (stripe-object)
  (country
   last4
   bank-name
   validated))

(defmodel card (api-resource)
  (exp-month
   exp-year
   last4
   country
   type
   name
   customer
   address-line1
   address-line2
   address-zip
   address-city
   address-state
   address-country
   address-zip-check
   address-line1-check
   cvc-check
   fingerprint)
  (:deleted-class deleted-card)
  (:simple-methods update delete))

(defun list-cards (customer &optional params)
  (list-all-aux 'card params :url (fmt "customers/~a/cards" (id customer))))

(defmethod instance-url ((self card))
  (fmt "~a/~a/cards/~a"
       (class-url 'customer)
       (card-customer self)
       (id self)))

(defmodel charge (api-resource)
  (amount
   currency
   paid
   refunded
   disputed
   captured
   description
   failure-message
   failure-code
   amount-refunded
   customer
   invoice
   refunds
   card
   dispute
   balance-transaction)
  (:default-initargs :refunds nil)
  (:simple-methods create retrieve update list))

(defmethod refund-charge ((self charge) &optional params)
  (request-update self params :url (fmt "~a/refund" (instance-url self))))

(defmethod capture-charge ((self charge) &optional params)
  (request-update self params :url (fmt "~a/capture" (instance-url self))))

(defmethod update-dispute ((self charge) params)
  (request-instance 'dispute (instance-url self) params))

(defmethod close-dispute ((self charge) params)
  (request-instance 'dispute
                    :post
                    (fmt "~a/dispute/close"
                         (instance-url self))
                    params))

(defmodel coupon (api-resource)
  (percent-off
   amount-off
   currency
   duration
   duration-in-months
   max-redemptions
   redeem-by
   times-redeemed)
  (:deleted-class deleted-coupon)
  (:simple-methods create retrieve delete list))

(defmodel customer (api-resource)
  (description
   default-card
   email
   plan
   trial-end
   discount
   next-recurring-charge
   subscription
   subscriptions
   deliquent
   account-balance
   cards)
  (:default-initargs :cards '())
  (:deleted-class deleted-customer)
  (:simple-methods create retrieve update delete list))

(defmethod create-card ((customer customer) (token string))
  (request-instance
   'card :post
   (fmt "~a/cards" (instance-url customer))
   `(("card" . ,token))))

(defmethod create-card ((customer customer) (params list))
  (request-instance
   'card
   :post
   (fmt "~a/cards" (instance-url customer))
   `(("card" ,@params))))

(defmethod update-subscription ((customer customer) params)
  (request-instance 'subscription
                    :post
                    (fmt "~a/subscription"
                         (instance-url customer))
                    params))

(defmethod cancel-subscription ((customer customer) &key at-period-end)
  (request-instance 'subscription
                    :delete
                    (fmt "~a/subscription"
                         (instance-url customer))
                    (when at-period-end
                      '(("at_period_end" . t)))))

(defmethod delete-discount ((customer customer))
  (request-instance 'discount
                    :delete
                    (fmt "~a/discount" (instance-url customer))))

(defmodel discount (stripe-object)
  (end
   start
   coupon
   customer))

(defmodel dispute (api-resource)
  (charge amount currency status
   evidence evidence-due-by
   reason balance-transaction))

(defmodel event (api-resource)
  (type user-id data pending-webhooks)
  (:simple-methods retrieve list))

(defmodel event-data (stripe-object)
  ((previous-attributes :accessor event-data-previous-attributes)
   (object :accessor event-data-object)))

(defmethod shared-initialize :after ((self event-data) slots &key previous-attributes)
  (declare (ignore slots))
  (when previous-attributes
    (setf (event-data-previous-attributes self)
          (plist-hash-table previous-attributes))))

(defmodel fee (api-resource)
  (type application amount description currency))

(defmodel invoice (api-resource)
  (subtotal
   total amount-due start-balance ending-balance
   next-payment-attempt
   attempted
   charge closed customer
   date
   paid period-start period-end
   discount
   lines
   attempt-count currency)
  (:default-initargs :lines nil)
  (:simple-methods create retrieve update delete list))

(defun upcoming-invoice (params)
  (request-instance 'invoice
                    :get
                    (fmt "~a/upcoming" (class-url 'invoice))
                    params))

(defmethod pay-invoice ((invoice invoice))
  (request-update invoice nil :url (fmt "~a/pay" (instance-url invoice))))

(defmodel invoice-item (api-resource)
  (amount
   currency description date
   customer invoice)
  (:deleted-class deleted-invoice-item)
  (:simple-methods create retrieve update delete list))

(defmodel invoice-line-item-period (stripe-object)
  (start end))

(defmodel invoice-line-item (stripe-object)
  (type
   amount currency proration period
   quantity plan description))

(defmodel next-recurring-charge (stripe-object)
  (amount date))

(defmodel plan (api-resource)
  (amount currency interval interval-count name trial-period-days)
  (:deleted-class deleted-plan)
  (:simple-methods create retrieve delete list update))

(defmodel recipient (api-resource)
  (type name deleted description active-account email verified)
  (:deleted-class deleted-recipient)
  (:simple-methods create retrieve update delete list))

(defmodel refund (stripe-object)
  (amount currency balance-transaction))

(defmodel subscription (api-resource)
  (current-period-end
   current-period-start
   cancel-at-period-end
   customer
   start
   status
   trial-start
   trial-end
   plan
   canceled-at
   ended-at
   quantity))

(defmodel summary (stripe-object)
  (charge-feeds
   net refund-gross adjustment-gross refund-feeds
   validation-count validation-fees refund-count
   adjustment-count charge-count charge-gross))

(defmodel token (stripe-object)
  (amount currency used card)
  (:simple-methods create retrieve))

(defmethod create-card ((customer customer) (token token))
  (create-card customer (id token)))

(defmodel transfer (api-resource)
  (status
   date summary description statement-descriptor amount currency
   other-transfers recipient bank-account balance-transaction)
  (:simple-methods create retrieve list))

(defmodel transfer-transaction (stripe-object)
  (amount net type created description fee)
  (:simple-methods list))
