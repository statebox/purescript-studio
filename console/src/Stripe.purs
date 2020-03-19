module Stripe where

import Data.Maybe (Maybe)


-- | Stripe populates this with things like `"customer"`, `"object"`, `"list"` and so on.
type ObjectTag = String

--------------------------------------------------------------------------------

-- | https://stripe.com/docs/api/customers/object
type Customer =
  { object           :: ObjectTag
  , id               :: CustomerId
  , name             :: Maybe String
  , description      :: Maybe String
  , email            :: Email
  , phone            :: Maybe String
  , balance          :: Amount
  , currency         :: Currency
  , invoice_prefix   :: String
  , invoice_settings :: InvoiceSettings
  , subscriptions    :: SubscriptionsInfo
  , delinquent       :: Boolean
  }

type CustomerId = String

type InvoiceSettings =
  { default_payment_method :: PaymentMethodId
  }

--------------------------------------------------------------------------------

-- | https://stripe.com/docs/api/payment_methods
type PaymentMethod =
  { id              :: PaymentMethodId
  , object          :: ObjectTag
  , "type"          :: PaymentMethodType
  , billing_details :: Maybe BillingDetails
  , card            :: Maybe Card
  , customer        :: CustomerId
  }

type PaymentMethodId = String

-- | One of `"card"` | `"fpx"` | `"ideal"` | `"sepa_debit"`. See https://stripe.com/docs/api/payment_methods/object.
type PaymentMethodType = String

type BillingDetails =
  { name    :: Maybe String
  , phone   :: Maybe Phone
  , email   :: Maybe Email
  , address :: Maybe Address
  }

type Card =
 { fingerprint :: String
 , brand       :: CardBrand
 , last4       :: String -- ^ last four digits of the card number.
 , exp_month   :: MonthNr
 , exp_year    :: Year
 , country     :: Country
 , funding     :: String
 }

-- | Uniquely identifies this particular card number. You can use this attribute to check whether two
-- | customers who’ve signed up with you are using the same card number, for example.
type CardFingerprint = String

-- | One of `"amex"` | `"diners"` | `"discover"` | `"jcb"` | `"mastercard"` | `"unionpay"` | `"visa"` | `"unknown"`.
type CardBrand = String

-- | One of `"credit"` | `"debit"` | `"prepeaid"` | `"unknown"`.
type Funding = String

--------------------------------------------------------------------------------

-- | https://stripe.com/docs/api/invoices/object
type Invoice =
  { object           :: ObjectTag
  , id               :: InvoiceId
  , account_name     :: String
  , account_country  :: Country
  , customer         :: CustomerId
  , customer_email   :: String
  , currency         :: String
  , amount_due       :: Amount
  , amount_paid      :: Amount
  , amount_remaining :: Amount
  }

type InvoiceId = String

--------------------------------------------------------------------------------

type SubscriptionsInfo =
  { object      :: ObjectTag
  , has_more    :: Boolean
  , total_count :: Int
  , url         :: URLSuffix -- ^ e.g. "/v1/customers/:customerId:/subscriptions"
  , data        :: Array Subscription
  }

--------------------------------------------------------------------------------

type Subscription =
  { id                   :: SubscriptionId
  , customer             :: CustomerId
  , object               :: ObjectTag
  , created              :: Timestamp
  , current_period_start :: Timestamp
  , current_period_end   :: Timestamp
  , latest_invoice       :: Maybe InvoiceId
  , items                :: { object    :: ObjectTag
                            , data      :: Array SubscriptionItem
                            , url       :: URLSuffix
                            , has_more  :: Boolean
                            }
  }

type SubscriptionId = String

type SubscriptionItem =
  { id           :: SubscriptionItemId
  , object       :: ObjectTag
  , quantity     :: Int
  , subscription :: SubscriptionId
  , plan         :: Plan
  , created      :: Timestamp
  }

type SubscriptionItemId = String

-- | E.g. `"charge_automatically"`
type CollectionMethod = String

type Plan =
  { id             :: PlanId
  , object         :: ObjectTag
  , nickname       :: Maybe String
  , product        :: ProductId
  , amount         :: Amount
  , amount_decimal :: AmountDecimal
  , currency       :: Currency
  , billing_scheme :: BillingScheme
  , created        :: Timestamp
  , interval       :: Interval
  , interval_count :: Int
  }

type PlanId = String

-- | E.g. `"per_unit"`
type BillingScheme = String

-- | E.g. `"month"`
type Interval = String

type ProductId = String

--------------------------------------------------------------------------------

type Address =
  { postal_code :: Maybe PostalCode
  , city        :: Maybe String
  , country     :: Maybe Country
  , line1       :: Maybe String
  , line2       :: Maybe String
  , state       :: Maybe State
  }

-- | Two-letter country code (ISO 3166-1 alpha-2).
type Country = String

type PostalCode = String

type State = String

type Phone = String

type Email = String

--------------------------------------------------------------------------------

type Currency = String

type Amount = Int

type AmountDecimal = String

--------------------------------------------------------------------------------

type URLSuffix = URL

type URL = String

--------------------------------------------------------------------------------

type Timestamp = Int

type MonthNr = Int

-- | Year, starting from zero, i.e. the year 2020 is represented as `2020`.
type Year = Int
