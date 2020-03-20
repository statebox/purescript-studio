module Statebox.Console where

import Prelude
import Data.Either (either)
import Data.Lens
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen (ComponentHTML)
import Halogen.HTML (HTML, p, text, br, div, ul, li, h2, h3, table, tr, th, td)
import Halogen.Query.HalogenM (HalogenM)

import Statebox.Console.DAO as DAO

import Stripe as Stripe

import Debug.Trace (spy)

--------------------------------------------------------------------------------

type State =
  { customer       :: Maybe Stripe.Customer
  , paymentMethods :: Array Stripe.PaymentMethod
  , accounts       :: Array { invoices :: Array Stripe.Invoice
                            }
  , status         :: AppStatus
  }

_accounts = prop (SProxy :: SProxy "accounts")
_invoices = prop (SProxy :: SProxy "invoices")

--------------------------------------------------------------------------------

data AppStatus = Ok | ErrorStatus String

derive instance eqAppStatus :: Eq AppStatus

instance showAppStatus :: Show AppStatus where
  show = case _ of
    Ok            -> "Ok"
    ErrorStatus x -> "(ErrorStatus " <> x <> ")"

type Input = State

data Action = FetchStuff

data Query a = DoAction Action a

type ChildSlots = ()

ui :: ∀ m. MonadAff m => H.Component HTML Query Input Void m
ui =
  H.mkComponent
    { initialState: mkInitialState
    , eval:         H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    , render:       render
    }

mkInitialState :: Input -> State
mkInitialState input = input

handleQuery = case _ of
  (DoAction x next) -> do
    handleAction x
    pure (Just next)

handleAction :: ∀ m. MonadAff m => Action -> HalogenM State Action ChildSlots Void m Unit
handleAction = case _ of
  FetchStuff -> do
    H.liftEffect $ log "handling action FetchStuff..."

    -- fetch the customer
    customerEE <- H.liftAff $ DAO.fetchCustomer
    customerEE # either (\e ->         H.modify_ $ _ { customer = Nothing, status = ErrorStatus "Failed to fetch customer." })
                        (either (\e -> H.modify_ $ _ { customer = Nothing, status = ErrorStatus "Decoding customer failed."})
                                (\x -> H.modify_ $ _ { customer = Just x }))
    spyM "customerEE" $ customerEE

    -- fetch some invoices for the customer
    invoicesEE <- H.liftAff $ DAO.listInvoices
    invoicesEE # either (\e ->         H.modify_ $ _ { status = ErrorStatus "Failed to fetch invoices." })
                        (either (\e -> H.modify_ $ _ { status = ErrorStatus "Decoding invoices failed."})
                                (\x -> H.modify_ $ _ { accounts = [ { invoices: x.data } ] }))
    spyM "invoicesEE" $ invoicesEE

    -- fetch the payment methods for this customer
    paymentMethodsEE <- H.liftAff $ DAO.listPaymentMethods
    paymentMethodsEE # either (\e ->   H.modify_ $ _ { status = ErrorStatus "Failed to fetch payment methods." })
                        (either (\e -> H.modify_ $ _ { status = ErrorStatus "Decoding payment methods failed."})
                                (\x -> H.modify_ $ _ { paymentMethods = x.data }))
    spyM "paymentMethodsEE" $ paymentMethodsEE

    H.liftEffect $ log "FetchStuff done."

--------------------------------------------------------------------------------

render :: ∀ m. MonadAff m => State -> ComponentHTML Action ChildSlots m
render state =
  div []
      [ p [] [ text $ if state.status == Ok then "" else "status: " <> show state.status ]
      , h2 [] [ text "Customer" ]
      , div [] (maybe [] (pure <<< customerHtml) state.customer)
      , h3 [] [ text "Customer's payment methods" ]
      , div [] (state.paymentMethods <#> paymentMethodHtml)
      , h2 [] [ text "Invoices" ]
      , div [] (state.accounts <#> \account -> invoiceSummaries account.invoices)
      ]

invoiceSummaries :: ∀ m. MonadAff m => Array Stripe.Invoice -> ComponentHTML Action ChildSlots m
invoiceSummaries invoices =
  table [] (invoices <#> invoiceSummaryLineHtml)
  where
    invoiceSummaryLineHtml :: ∀ m. MonadAff m => Stripe.Invoice -> ComponentHTML Action ChildSlots m
    invoiceSummaryLineHtml i =
      tr [] [ td [] [ text $ i.customer_email ]
            , td [] [ text $ i.account_name ]
            , td [] [ text $ i.currency ]
            , td [] [ text $ show i.amount_due ]
            ]

customerHtml :: ∀ m. MonadAff m => Stripe.Customer -> ComponentHTML Action ChildSlots m
customerHtml c =
  table []
        [ tr [] [ th [] [ text "name" ]
                , td [] [ text $ fold c.name ]
                ]
        , tr [] [ th [] [ text "email" ]
                , td [] [ text $ fold c.email ]
                ]
        , tr [] [ th [] [ text "phone" ]
                , td [] [ text $ fold c.phone ]
                ]
        , tr [] [ th [] [ text "description" ]
                , td [] [ text $ fold c.description ]
                ]
        , tr [] [ th [] [ text "address" ]
                , td [] [ maybe (text "no address") addressHtml c.address ]
                ]
        , tr [] [ th [] [ text "balance" ]
                , td [] [ text $ c.currency <> " " <> show c.balance <> " cents" ]
                ]
        , tr [] [ th [] [ text "tax ids" ]
                , td [] [ taxIdsHtml c.tax_ids ]
                ]
        ]

taxIdsHtml :: ∀ m. MonadAff m => Stripe.ArrayWrapper Stripe.TaxIdData -> ComponentHTML Action ChildSlots m
taxIdsHtml x =
  table [] (taxIdDataHtml <$> x.data)

taxIdDataHtml :: ∀ m. MonadAff m => Stripe.TaxIdData -> ComponentHTML Action ChildSlots m
taxIdDataHtml x =
  tr [] [ td [] [ text x.value ]
        , td [] [ text x.type ]
        ]

paymentMethodHtml :: ∀ m. MonadAff m => Stripe.PaymentMethod -> ComponentHTML Action ChildSlots m
paymentMethodHtml pm =
  table []
        [ tr [] [ td [] [ text "type" ]
                , td [] [ text pm.type ]
                ]
        , tr [] [ td [] [ text "card" ]
                , td [] [ maybe (text "no card") cardHtml pm.card ]
                ]
        ]

billingDetailsHtml :: ∀ m. MonadAff m => Stripe.BillingDetails -> ComponentHTML Action ChildSlots m
billingDetailsHtml bd =
  table []
        [ tr [] [ th [] [ text "name" ]
                , td [] [ text $ fold bd.name ]
                ]
        , tr [] [ th [] [ text "email" ]
                , td [] [ text $ fold bd.email ]
                ]
        , tr [] [ th [] [ text "phone" ]
                , td [] [ text $ fold bd.phone ]
                ]
        , tr [] [ th [] [ text "address" ]
                , td [] [ maybe (text "no address") addressHtml bd.address ]
                ]
        ]

addressHtml :: ∀ m. MonadAff m => Stripe.Address -> ComponentHTML Action ChildSlots m
addressHtml a =
  table []
        [ tr [] [ th [] [ text "address" ]
                , td [] [ text $ fold a.line1, br [], text $ fold a.line2 ]
                ]
        , tr [] [ th [] [ text "city" ]
                , td [] [ text $ fold a.city ]
                ]
        , tr [] [ th [] [ text "postal code" ]
                , td [] [ text $ fold a.postal_code ]
                ]
        , tr [] [ th [] [ text "state" ]
                , td [] [ text $ fold a.state ]
                ]
        , tr [] [ th [] [ text "country" ]
                , td [] [ text $ fold a.country ]
                ]
        ]

cardHtml :: ∀ m. MonadAff m => Stripe.Card -> ComponentHTML Action ChildSlots m
cardHtml c =
  text $ c.country <> " " <> c.brand <> " " <>
         formatCCNumber c <>
         " EXP " <> formatExpiryDate c <>
         " (" <> c.funding <> ")"
  where
    formatCCNumber :: Stripe.Card -> String
    formatCCNumber card = "**** **** **** " <> card.last4

    formatExpiryDate :: Stripe.Card -> String
    formatExpiryDate card = show c.exp_month <> "/" <> show c.exp_year

--------------------------------------------------------------------------------

spyM :: ∀ m a. Applicative m => String -> a -> m Unit
spyM tag value = do
  let dummy1 = spy tag value
  pure unit
