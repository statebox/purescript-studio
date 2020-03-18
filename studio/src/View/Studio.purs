module View.Studio where

import Prelude hiding (div)
import Affjax as Affjax
import Affjax (URL)
import Affjax.ResponseFormat as ResponseFormat
import Control.Coroutine (Consumer, Producer, Process, runProcess, consumer, connect)
import Control.Monad.State.Class
import Data.AdjacencySpace as AdjacencySpace
import Data.Either (either)
import Data.Lens ((.=), preview, _Just, Optic)
import Data.Lens.At
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map as Map
import Data.Profunctor.Choice
import Data.Profunctor.Strong
import Data.Set as Set
import Data.String (drop)
import Data.Traversable (for_)
import Effect.Exception (try)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Random (random)
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen (mkEval, defaultEval)
import Halogen.HTML (HTML)
import Halogen.Query.HalogenM (HalogenM)
import Routing.Duplex (print)
import Web.Event.Event as Event

import Data.Petrinet.Representation.PNPRO as PNPRO
import Language.Statebox.Wiring.Generator.DiagramV2.Operators as DiagramV2
import Statebox.Client as Stbx
import Statebox.Client (evalTransactionResponse)
import Statebox.Core.Transaction as Stbx
import Statebox.Core.Transaction (HashTx, TxId)
import View.Diagram.Update as DiagramEditor
import View.Petrinet.Model (Msg(NetUpdated))
import View.KDMonCat.App as KDMonCat.Bricks
import View.KDMonCat.Bricks as KDMonCat.Bricks
import View.Model (Project, ProjectId, _kdmoncats)
import View.Studio.Model
import View.Studio.Model.Route as Route
import View.Studio.Model.Route (Route, RouteF(..), ProjectRoute, ProjectRouteF(..), NodeIdent(..))
import View.Studio.View (render, ChildSlots)

type Input = State

data Query a
  = LoadTransactionsThenView URL TxId a
  | AddProject ProjectId Project a
  | Navigate Route a

data Output = ProjectChanged String (Maybe Project)

ui :: ∀ m. MonadAff m => H.Component HTML Query Input Output m
ui =
  H.mkComponent
    { initialState: mkInitialState
    , eval:         mkEval $ defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    , render:       render
    }

mkInitialState :: Input -> State
mkInitialState input = input

handleQuery :: ∀ m a. MonadAff m => Query a -> H.HalogenM State Action ChildSlots Output m (Maybe a)
handleQuery = case _ of
  LoadTransactionsThenView endpointUrl hash next -> do
    handleAction (LoadTransactions endpointUrl hash)
    pure (Just next)

  AddProject projectId project next -> do
    H.modify_ $ \state -> state { projects = Map.insert projectId project state.projects }
    pure (Just next)

  Navigate newRoute next -> do
    H.modify_ _ { route = newRoute }
    pure (Just next)

handleAction :: ∀ m. MonadAff m => Action -> HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  ShowDiagramNodeContent route -> do
    handleAction (SelectRoute route)

  SelectRoute route -> do
    { nav } <- H.get
    H.liftEffect $ nav.pushState (unsafeToForeign {}) $ print Route.codex route
    H.modify_ \state -> state { route = route }

  SetApiUrl url -> do
    H.modify_ \state -> state { apiUrl = url }

  LoadTransaction hash -> do
    endpointUrl <- H.get <#> _.apiUrl
    H.liftEffect $ log $ "LoadTransaction: requesting transaction " <> hash <> " from " <> endpointUrl
    res <- H.liftAff $ Stbx.requestTransaction endpointUrl hash
    res # evalTransactionResponse
      (\err                        -> H.liftEffect $ log $ "failed to decode HTTP response into JSON: " <> Affjax.printError err)
      (\(Stbx.JsonDecodeError err) -> H.liftEffect $ log $ "Expected to decode a valid Statebox transaction: " <> show err)
      (\txError                    -> H.liftEffect $ log $ "Handling error of received data: " <> show txError)
      (\{id, tx}                   -> do H.modify_ (\state -> state { hashSpace = AdjacencySpace.update Stbx.getPrevious state.hashSpace id tx })
                                         H.liftEffect $ log $ show tx)

  LoadTransactions endpointUrl startHash -> do
    H.liftEffect $ log $ "LoadTransactions: requesting transactions up to root, starting at " <> startHash <> " from " <> endpointUrl
    runProcess txIngester

    -- after the transaction and its history have been loaded, display it
    state <- H.get
    let txSumMaybe = AdjacencySpace.lookup startHash state.hashSpace
    for_ txSumMaybe $ handleAction <<< SelectRoute <<< Route.fromTxSum endpointUrl startHash

    where
      -- | This ingests transactions produced from the HTTP API into our transaction storage.
      txIngester :: Process (HalogenM State Action ChildSlots Output m) Unit
      txIngester = txProducer `connect` txConsumer

      txProducer :: Producer HashTx (HalogenM State Action ChildSlots Output m) Unit
      txProducer = Stbx.requestTransactionsToRootM endpointUrl startHash

      txConsumer :: Consumer HashTx (HalogenM State Action ChildSlots Output m) Unit
      txConsumer = consumer txStorer
        where
          txStorer :: HashTx -> (HalogenM State Action ChildSlots Output m) (Maybe Unit)
          txStorer itx@{id, tx} = do
            H.modify_ (\state -> state { hashSpace = AdjacencySpace.update Stbx.getPrevious state.hashSpace id tx })
            H.liftEffect $ log $ show itx
            pure Nothing

  LoadPNPRO url -> do
    H.liftEffect $ log $ "LoadPNPRO: requesting PNPRO file from " <> url
    resE <- H.liftAff $ Affjax.request $ Affjax.defaultRequest { url = url, responseFormat = ResponseFormat.string }
    resE # either
      (\err -> H.liftEffect $ log $ "failed to decode HTTP response into JSON: " <> Affjax.printError err)
      (\res -> do
           pnproDocumentE <- H.liftEffect $ try $ PNPRO.fromString res.body
           pnproDocumentE # either
             (\err      -> H.liftEffect $ log $ "Error decoding PNPRO document: " <> show err)
             (\pnproDoc -> handleAction $ CRUDProject $ CreateAction $ fromPNPROProject pnproDoc.project)
      )

  CRUDProject action -> handleCRUDAction _projects action \id mProject -> H.raise $ ProjectChanged id mProject

  CRUDKDMonCat action -> handleCRUDActionInProject _kdmoncats action

  HandleDiagramEditorMsg (DiagramEditor.OperatorClicked opId) -> do
    H.liftEffect $ log $ "DiagramEditor.OperatorClicked: " <> opId
    state <- H.get
    let
      -- TODO #87 we hardcode the assumption here that opId is a net (NetNode opId) but it could be (LeDiagram opId)
      newRouteMaybe :: Maybe Route
      newRouteMaybe = case state.route of
        ProjectRoute pid (Diagram dname _) -> Just (ProjectRoute pid (Diagram dname (Just (NetNode opId))))
        _                                  -> Nothing
    maybe (pure unit) (handleAction <<< SelectRoute) newRouteMaybe

  HandleDiagramEditorMsg (DiagramEditor.OperatorsChanged ops) -> do
    modifyProject \proute p ->
      case proute of
        Diagram dname _ -> p { diagrams = fromMaybe p.diagrams (modifyDiagramInfo dname (_ {ops = ops}) p.diagrams) }
        _               -> p
  HandleKDMonCatBricksMsg diagramInfo (KDMonCat.Bricks.SelectionChanged selBox) -> do
    let boxes = (KDMonCat.Bricks.toBricksInput (DiagramV2.fromOperators diagramInfo.ops) selBox).selectedBoxes
    maybe (pure unit) (handleAction <<< HandleDiagramEditorMsg <<< DiagramEditor.OperatorClicked) $ do
      box <- Set.findMin boxes
      op <- DiagramV2.fromPixel diagramInfo.ops box.bid
      pure op.identifier

  HandleKDMonCatAppMsg kdmoncatInput -> do
    modifyProject \proute p ->
      case proute of
        KDMonCatR kdName -> p { kdmoncats = modifyKDMonCat kdName (const kdmoncatInput) p.kdmoncats }
        _                -> p

  HandlePetrinetEditorMsg NetUpdated -> do
    pure unit

  StopEvent mAction event -> do
    H.liftEffect $ Event.preventDefault event
    H.liftEffect $ Event.stopPropagation event
    for_ mAction handleAction

type Affine s t a b = forall p. Strong p => Choice p => Optic p s t a b
type Affine' s a = Affine s s a a

handleCRUDAction :: ∀ m s t a. MonadEffect m => MonadState s m => At t String a => Affine' s t -> CRUDAction a -> (String -> Maybe a -> m Unit) -> m Unit
handleCRUDAction l action eventHandler =
  case action of
    CreateAction a -> do
      rnd <- liftEffect random
      let id = "id" <> drop 2 (show rnd)
      handle id $ Just a
    UpdateAction id f -> do
      s <- get
      handle id (map f (join (preview (l <<< at id) s)))
    DeleteAction id -> do
      handle id Nothing
  where
    handle :: String -> Maybe a -> m Unit
    handle id mValue = do
      l <<< at id .= mValue
      eventHandler id mValue

handleCRUDActionInProject :: ∀ m t a. MonadAff m => At t String a => Affine' Project t -> CRUDAction a -> HalogenM State Action ChildSlots Output m Unit
handleCRUDActionInProject l action = do
  state <- H.get
  case state.route of
    ProjectRoute pid proute -> do
      handleCRUDAction (_projects <<< at pid <<< _Just <<< l) action
        \_ _ -> do
          state' <- H.get
          for_ (Map.lookup pid state'.projects) (Just >>> ProjectChanged pid >>> H.raise)
    _ -> pure unit

modifyProject :: ∀ m. MonadAff m => (ProjectRoute -> Project -> Project) -> HalogenM State Action ChildSlots Output m Unit
modifyProject fn = do
  state <- H.get
  case state.route of
    ProjectRoute pid proute -> do
      for_ (Map.lookup pid state.projects) \p -> do
        let newProject = fn proute p
        H.raise $ ProjectChanged pid $ Just newProject
        H.modify_ (_ { projects = Map.insert pid newProject state.projects })
    _ -> pure unit
