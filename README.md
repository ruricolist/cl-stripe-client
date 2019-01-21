NB: This implements an [older version](https://web.archive.org/web/20140122053928/https://stripe.com/docs/api) (2013-12-03) of the Stripe API. It still works, but it lacks support for (among other things, presumably) the Product model.

CL-STRIPE-CLIENT is a boring Common Lisp client for the Stripe payment
system. It closely follows the Java version of the official Stripe
client, including its test suite. It can be used against the
[Stripe documentation][docs], with a few caveats.

- Configuration uses dynamic variables: in particular, you must set or
  bind `*api-key*`.
- Static methods become functions: `Charge.create` becomes
  `create-charge`.
- Except for some common methods, methods become prefixed readers:
  `charge.card` becomes `(charge-card charge)`.

  The exceptions are `id`, `livep`, `created`, `deletedp`,
  `description` and `metadata`, which are common to all Stripe
  objects.
- Nested alists are used for the parameter dictionaries.

Bear in mind that API objects are immutable; when you call
`(update-card ...)`, you get a different card object each time.

For example, to create a charge:

    (setf stripe:*api-key* "my_api_key")

    (stripe:create-charge
     '(("amount" . 100)
       ("currency" . "usd")
       ("card"
        ("number" . "4242424242424242")
        ("exp_month" . 12)
        ("exp_year" . 2015)
        ("cvc" . "123")
        ("name" . "Lisp Bindings Cardholder")
        ("address_line1" . "140 2nd Street")
        ("address_line2" . "4th Floor")
        ("address_city" . "San Francisco")
        ("address_zip" . "94105")
        ("address_state" . "CA")
        ("address_country" ."USA"))))

[The test suite](tests.lisp) contains plenty of examples.

[docs]: https://stripe.com/docs/api/java
