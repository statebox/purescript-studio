module Statebox.Console.DAO where

import Prelude
import Affjax as Affjax
import Affjax (Response, URL)
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.HTTP.Method (Method(GET))
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Effect.Aff (Aff)

import Stripe as Stripe

import Debug.Trace (spy)

mkUrl suffix = "http://localhost" <> suffix

--------------------------------------------------------------------------------

type InvoicesResponse =
  { object :: String
  , "data" :: Array Stripe.Invoice
  }

listInvoices :: Aff (Affjax.Error \/ String \/ InvoicesResponse)
listInvoices = listInvoices' # map (map (_.body >>> spy "invoices body dump" >>> decodeJson))

listInvoices' :: Aff (Affjax.Error \/ Response Json)
listInvoices' =
  Affjax.request $ Affjax.defaultRequest { url = mkUrl "/invoices"
                                         , method = Left GET
                                         , responseFormat = ResponseFormat.json
                                         }

--------------------------------------------------------------------------------

fetchCustomer :: Aff (Affjax.Error \/ String \/ Stripe.Customer)
fetchCustomer = fetchCustomer' # map (map (_.body >>> spy "customer body dump" >>> decodeJson))

fetchCustomer' :: Aff (Affjax.Error \/ Response Json)
fetchCustomer' =
  Affjax.request $ Affjax.defaultRequest { url = mkUrl "/customer"
                                         , method = Left GET
                                         , responseFormat = ResponseFormat.json
                                         }

--------------------------------------------------------------------------------

type PaymentMethodsResponse =
  { object   :: Stripe.ObjectTag
  , "data"   :: Array Stripe.PaymentMethod
  , has_more :: Boolean
  , url      :: Stripe.URLSuffix
  }

listPaymentMethods :: Aff (Affjax.Error \/ String \/ PaymentMethodsResponse)
listPaymentMethods = listPaymentMethods' # map (map (_.body >>> spy "paymentMethods dump" >>> decodeJson))

listPaymentMethods' :: Aff (Affjax.Error \/ Response Json)
listPaymentMethods' =
  Affjax.request $ Affjax.defaultRequest { url = mkUrl "/payment-methods"
                                         , method = Left GET
                                         , responseFormat = ResponseFormat.json
                                         }
