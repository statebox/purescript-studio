module Statebox.Console where

import Prelude
import Data.Array (cons, filter)
import Data.Either (either)
import Data.Generic.Rep
import Data.Lens
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Data.Foldable (fold, foldMap)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen (ComponentHTML)
import Halogen.HTML (HTML, p, text, br, span, div, ul, li, h2, h3, table, tr, th, td, button)
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.Query.HalogenM (HalogenM)

import Statebox.Console.DAO as DAO

import Stripe as Stripe

import Debug.Trace (spy)

-- TODO
fakeCustomerId = "TODO"

type ApiKey = { hex :: Hex, name :: String }
type RootId = String -- TODO get from stbx-core
type TxHash = Hex    -- TODO get from stbx-core
type Hex = String    -- TODO get from stbx-core

--------------------------------------------------------------------------------

-- | projects are collections of root-transactions and are used to manage the public keys associated to those.
type Project =
  { name             :: String
  , rootTransactions :: Array TxHash
  }

type ProjectId = String

--------------------------------------------------------------------------------

type TxPubInfo =
  { name    :: String -- TODO seems redundant if we have the hash
  , message :: String -- TODO seems redundant if we have the hash
  , hash    :: TxHash
  , key     :: Unit   -- TODO is this the key of a genesis tx?
  }

--------------------------------------------------------------------------------

type State =
  { route            :: Route
  , customer         :: Maybe Stripe.Customer
  , paymentMethods   :: Array Stripe.PaymentMethod
  , subscriptions    :: Array Stripe.Subscription
  , plans            :: Array Stripe.PlanWithExpandedProduct
  , accounts         :: Array { invoices :: Array Stripe.Invoice
                              }
  , projects         :: Map ProjectId Project
  , apiKeys          :: Array ApiKey
  , rootTransactions :: Array TxHash
  , status           :: AppStatus
  }

_accounts = prop (SProxy :: SProxy "accounts")
_invoices = prop (SProxy :: SProxy "invoices")

--------------------------------------------------------------------------------

data Route
  = Home
  | Projects
  | ProjectR ProjectId
  | APIKeys
  | RootTx
  | Invoices Stripe.CustomerId
  | Account Stripe.CustomerId
  | Subscription
  | Plan

derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
derive instance genericRoute :: Generic Route _

--------------------------------------------------------------------------------

data AppStatus = Ok | ErrorStatus String

derive instance eqAppStatus :: Eq AppStatus

instance showAppStatus :: Show AppStatus where
  show = case _ of
    Ok            -> "Ok"
    ErrorStatus x -> "(ErrorStatus " <> x <> ")"

type Input = State

data Action
  = SelectRoute Route

  | CreateRootTx
  | PublishRootTx TxPubInfo

  | CreateApiKey
  | DeprecateApiKey ApiKey
  | AssociateApiKeyWithProject ApiKey ProjectId
  | AssociateApiKeyWithRoot ApiKey RootId

  | FetchStuff

data Query a
  = DoAction Action a

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
  DoAction x next -> do
    handleAction x
    pure (Just next)

  -- NavigateTo newRoute next -> do
  --   H.modify_ $ \state -> state -- { route = newRoute }
  --   pure (Just next)

handleAction :: ∀ m. MonadAff m => Action -> HalogenM State Action ChildSlots Void m Unit
handleAction = case _ of

  -- NavigateTo newRoute ->
  --   H.modify_ $ \state -> state { route = newRoute }

  SelectRoute newRoute -> do
    H.modify_ \state -> state { route = newRoute }

  CreateRootTx -> do
    H.modify_ $ _ { status = ErrorStatus "Create root transaction." }

  PublishRootTx txPubInfo -> do
    H.modify_ $ \state -> state { status = ErrorStatus "Publish root transaction."
                                , rootTransactions = txPubInfo.hash `cons` state.rootTransactions
                                }

  CreateApiKey -> do
    H.modify_ $ _ { status = ErrorStatus "Create API key." }

  AssociateApiKeyWithProject apiKey projectId -> do
    H.modify_ $ _ { status = ErrorStatus $ "Associate API Key '" <> apiKey.name <> "' (hex: " <> apiKey.hex <> ") with project " <> projectId <> "." }

  AssociateApiKeyWithRoot apiKey rootTxId -> do
    H.modify_ $ _ { status = ErrorStatus $ "Associate API Key '" <> apiKey.name <> "' (hex: " <> apiKey.hex <> ") with root transaction " <> rootTxId <> "." }

  DeprecateApiKey apiKey -> do
    H.modify_ $ \state -> state { status = ErrorStatus $ "Successfully deprecated API key '" <> apiKey.name <> "'."
                                , apiKeys = filter (\k -> k /= apiKey) state.apiKeys
                                }

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

    -- fetch subscriptions for this customer
    subscriptionsEE <- H.liftAff $ DAO.listSubscriptions
    subscriptionsEE # either (\e ->    H.modify_ $ _ { status = ErrorStatus "Failed to fetch subscriptions." })
                        (either (\e -> H.modify_ $ _ { status = ErrorStatus "Decoding subscriptions failed."})
                                (\x -> H.modify_ $ _ { subscriptions = x.data }))
    spyM "subscriptionsEE" $ subscriptionsEE

    -- fetch plans for this customer
    plansEE <- H.liftAff $ DAO.listPlans
    plansEE # either (\e ->         H.modify_ $ _ { status = ErrorStatus "Failed to fetch plans." })
                     (either (\e -> H.modify_ $ _ { status = ErrorStatus "Decoding plans failed."})
                             (\x -> H.modify_ $ _ { plans = x.data }))
    spyM "plansEE" $ plansEE

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
      [ navMenuHtml state
      , contentHtml state
      , p [] [ text $ if state.status == Ok then "" else "status: " <> show state.status ]
      ]

navMenuHtml :: ∀ m. MonadAff m => State -> ComponentHTML Action ChildSlots m
navMenuHtml state =
  div []
      [ button [ onClick \e -> Just $ SelectRoute $ Home                    ] [ text "Home" ]
      , button [ onClick \e -> Just $ SelectRoute $ Projects                ] [ text "Projects" ]
      , button [ onClick \e -> Just $ SelectRoute $ APIKeys                 ] [ text "API Keys" ]
      , button [ onClick \e -> Just $ SelectRoute $ Invoices fakeCustomerId ] [ text "Invoices" ]
      , button [ onClick \e -> Just $ SelectRoute $ Subscription            ] [ text "Subscriptions" ]
      , button [ onClick \e -> Just $ SelectRoute $ Plan                    ] [ text "Plans" ]
      ]

contentHtml :: ∀ m. MonadAff m => State -> ComponentHTML Action ChildSlots m
contentHtml state = case state.route of
  Home ->
    div []
        [ h2 [] [ text "Statebox Cloud Admin Console" ]

        , h3 [] [ text "Projects" ]
        , ul [] $ Map.toUnfoldable state.projects <#> (\(projectId /\ project) ->
                li [] [ button [ onClick \e -> Just $ SelectRoute $ ProjectR projectId ] [ text project.name ] ])

        , h3 [] [ text "Billing accounts" ]
        , ul [] $ customers <#> \customer ->
            li [] [ button [ onClick \e -> Just $ SelectRoute $ Account customer.id ] [ text $ fold customer.name ]
                  , text $ fold customer.description
                  ]

        , h3 [] [ text "API keys" ]          
        , ul [] $ state.apiKeys <#> \key -> li [] [ p [] [ text key.name ]
                                                  , p [] [ text key.hex ]
                                                  ]
        ]
    where
      -- TODO in reality we should have multiple customers
      customers :: Array Stripe.Customer
      customers = maybe [] (\c -> [c]) state.customer
  Projects ->
    div [] $
        [ h2 [] [ text "Projects" ]
        , div [] $ Map.toUnfoldable state.projects  <#>
             (\(projectId /\ project) -> button [ onClick \e -> Just $ SelectRoute $ ProjectR projectId ] [ text project.name ])
        ]
  ProjectR projectId ->
    projectMaybe # maybe (text $ "project " <> projectId <> " not found.") (\project ->
      div []
          [ h2 [] [ text $ "Project " <> show projectId ]
          , h3 [] [ text $ "API keys" ]
          , h3 [] [ text $ "Roots" ]
          , ul [] (project.rootTransactions <#> \txHash -> li [] [ text txHash ])
          , p [] [ button [ onClick \e -> Just $ SelectRoute $ RootTx ] [ text "Create new root tx" ] ]
          ]
      )
    where
      projectMaybe = Map.lookup projectId state.projects
  APIKeys ->
    div [] $
        [ h2 [] [ text "API keys" ]
        , p [] [ button [ onClick \e -> Just $ CreateApiKey ] [ text "Create new API key" ] ]
        , ul [] $ state.apiKeys <#> \key -> li [] [ p [] [ text key.name ]
                                                  , p [] [ text key.hex ]
                                                  , p [] [ button [ onClick \e -> Just $ DeprecateApiKey key ] [ text "Deprecate" ] ]
                                                  ]
        , p [] [ text "* Assign to a root" ]
        ]
  RootTx ->
    div []
        [ h2 [] [ text "Create root transaction" ]
        , p [] [ text "name" ]
        , p [] [ text "message" ]
        , p [] [ text "hash" ]
        , p [] [ text "valid key [key 1] (add)" ]
        , p [] [ button [ onClick \e -> Just $ PublishRootTx { name: "Example tx", message: "Hi there!", hash: "CAF3CAF3", key: unit } ] [ text "Publish" ] ]
        ]
  Account customerId ->
    div []
        [ h2 [] [ text "Customer" ]
        , div [] (maybe [] (pure <<< customerHtml) state.customer)
        , h3 [] [ text "Customer's payment methods" ]
        , div [] (state.paymentMethods <#> paymentMethodHtml)
        ]
  Subscription ->
    div []
        [ h2 [] [ text "Subscriptions" ]
        , div [] (state.subscriptions <#> subscriptionHtml)
        ]
  Invoices x ->
    div []
        [ h2 [] [ text "Invoices" ]
        , div [] (state.accounts <#> \account -> invoiceSummaries account.invoices)
        ]
  Plan ->
    div []
        [ h2 [] [ text "Plans" ]
        , div [] (state.plans <#> planWithExpandedProductHtml)
        ]

--------------------------------------------------------------------------------

invoiceSummaries :: ∀ m. MonadAff m => Array Stripe.Invoice -> ComponentHTML Action ChildSlots m
invoiceSummaries invoices =
  table [] (invoices <#> invoiceSummaryLineHtml)
  where
    invoiceSummaryLineHtml :: ∀ m. MonadAff m => Stripe.Invoice -> ComponentHTML Action ChildSlots m
    invoiceSummaryLineHtml i =
      tr [] [ td [] [ text $ i.customer_email ]
            , td [] [ text $ i.account_name ]
            , td [] [ text $ formatCurrency i.currency i.amount_due ]
            ]

customerHtml :: ∀ m. MonadAff m => Stripe.Customer -> ComponentHTML Action ChildSlots m
customerHtml c =
  table [] $
        [ tr [] [ th [] [ text "name" ]
                , td [] [ text $ fold c.name ]
                ]
        , tr [] [ th [] [ text "description" ]
                , td [] [ text $ fold c.description ]
                ]
        , tr [] [ th [] [ text "email" ]
                , td [] [ text $ fold c.email ]
                ]
        , tr [] [ th [] [ text "phone" ]
                , td [] [ text $ fold c.phone ]
                ]
        ] <>
        foldMap addressRowsHtml c.address <>
        [ tr [] [ th [] [ text "balance" ]
                , td [] [ text $ formatCurrency c.currency c.balance ]
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
billingDetailsHtml bd = nameAddressPhoneHtml bd

nameAddressPhoneHtml :: ∀ m. MonadAff m => { | Stripe.NameAddressPhoneRow () } -> ComponentHTML Action ChildSlots m
nameAddressPhoneHtml x =
  table [] $
        [ tr [] [ th [] [ text "name" ]
                , td [] [ text $ fold x.name ]
                ]
        , tr [] [ th [] [ text "email" ]
                , td [] [ text $ fold x.email ]
                ]
        , tr [] [ th [] [ text "phone" ]
                , td [] [ text $ fold x.phone ]
                ]
        ] <>
        foldMap addressRowsHtml x.address

addressHtml :: ∀ m. MonadAff m => Stripe.Address -> ComponentHTML Action ChildSlots m
addressHtml a = table [] (addressRowsHtml a)

addressRowsHtml :: ∀ m. MonadAff m => Stripe.Address -> Array (ComponentHTML Action ChildSlots m)
addressRowsHtml a =
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

formatCurrency :: Stripe.Currency -> Stripe.Amount -> String
formatCurrency currency amount =
  show amount <> " " <> currency <> " cents"

timestampHtml :: ∀ m. MonadAff m => Stripe.Timestamp -> ComponentHTML Action ChildSlots m
timestampHtml ts = text $ show ts

timestampRangeHtml :: ∀ m. MonadAff m => Stripe.Timestamp -> Stripe.Timestamp -> ComponentHTML Action ChildSlots m
timestampRangeHtml start end =
  span [] [ timestampHtml start, text " thru ", timestampHtml end ]

subscriptionHtml :: ∀ m. MonadAff m => Stripe.Subscription -> ComponentHTML Action ChildSlots m
subscriptionHtml s =
  table []
        [ tr [] [ td [] [ text "id" ]
                , td [] [ text s.id ]
                ]
        , tr [] [ td [] [ text "status" ]
                , td [] [ text s.status ]
                ]
        , tr [] [ td [] [ text "quantity" ]
                , td [] [ text $ show s.quantity ]
                ]
        , tr [] [ td [] [ text "start date" ]
                , td [] [ timestampHtml s.start_date ]
                ]
        , tr [] [ td [] [ text "current period" ]
                , td [] [ timestampRangeHtml s.current_period_start s.current_period_end ]
                ]
        , tr [] [ td [] [ text "trial period" ]
                , td [] [ timestampRangeHtml s.trial_start s.trial_end ]
                ]
        , tr [] [ td [] [ text "collection method" ]
                , td [] [ text s.collection_method ]
                ]
        , tr [] [ td [] [ text "live mode" ]
                , td [] [ text $ show s.livemode ]
                ]
        , tr [] [ td [] [ text "items" ]
                , td [] (s.items.data <#> subscriptionItemHtml)
                ]
        ]

subscriptionItemHtml :: ∀ m. MonadAff m => Stripe.SubscriptionItem -> ComponentHTML Action ChildSlots m
subscriptionItemHtml item =
  table []
        [ tr [] [ td [] [ text "plan" ]
                , td [] [ planHtml item.plan ]
                ]
        , tr [] [ td [] [ text "created" ]
                , td [] [ text $ show item.created ]
                ]
        ]

planHtml :: ∀ m. MonadAff m => Stripe.Plan -> ComponentHTML Action ChildSlots m
planHtml plan =
  table []
        [ tr [] [ td [] [ text "nickname" ]
                , td [] [ text $ fromMaybe "-" plan.nickname ]
                ]
        , tr [] [ td [] [ text "product id" ]
                , td [] [ text plan.product ]
                ]
        , tr [] [ td [] [ text "created on" ]
                , td [] [ timestampHtml plan.created ]
                ]
        , tr [] [ td [] [ text "amount" ]
                , td [] [ text $ formatCurrency plan.currency plan.amount ]
                ]
        , tr [] [ td [] [ text "billing scheme" ]
                , td [] [ text plan.billing_scheme ]
                ]
        , tr [] [ td [] [ text "interval" ]
                , td [] [ text $ plan.interval <> " (" <> show plan.interval_count <> "x)" ]
                ]
        ]

--------------------------------------------------------------------------------

planWithExpandedProductHtml :: ∀ m. MonadAff m => Stripe.PlanWithExpandedProduct -> ComponentHTML Action ChildSlots m
planWithExpandedProductHtml plan =
  table []
        [ tr [] [ td [] [ text "nickname" ]
                , td [] [ text $ fromMaybe "-" plan.nickname ]
                ]
        , tr [] [ td [] [ text "product" ]
                , td [] [ productHtml plan.product ]
                ]
        , tr [] [ td [] [ text "created on" ]
                , td [] [ timestampHtml plan.created ]
                ]
        , tr [] [ td [] [ text "amount" ]
                , td [] [ text $ formatCurrency plan.currency plan.amount ]
                ]
        , tr [] [ td [] [ text "billing scheme" ]
                , td [] [ text plan.billing_scheme ]
                ]
        , tr [] [ td [] [ text "interval" ]
                , td [] [ text $ plan.interval <> " (" <> show plan.interval_count <> "x)" ]
                ]
        ]

productHtml :: ∀ m. MonadAff m => Stripe.Product -> ComponentHTML Action ChildSlots m
productHtml product =
  table []
        [ tr [] [ td [] [ text "product id" ]
                , td [] [ text product.id ]
                ]
        , tr [] [ td [] [ text "name" ]
                , td [] [ text product.name ]
                ]
        , tr [] [ td [] [ text "description" ]
                , td [] [ text $ fromMaybe "-" product.description ]
                ]
        , tr [] [ td [] [ text "unit" ]
                , td [] [ text $ fromMaybe "-" product.unit_label ]
                ]
        ]

--------------------------------------------------------------------------------

spyM :: ∀ m a. Applicative m => String -> a -> m Unit
spyM tag value = do
  let dummy1 = spy tag value
  pure unit
