module View.Studio where

import Prelude hiding (div)
import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Control.Coroutine (Consumer, Producer, Process, runProcess, consumer, connect)
import Data.Array (cons)
import Data.AdjacencySpace as AdjacencySpace
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Effect.Exception (try)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen (mkEval, defaultEval)
import Halogen.HTML (HTML)
import Halogen.Query.HalogenM (HalogenM)
import TreeMenu as MenuTree

import Data.Petrinet.Representation.PNPRO as PNPRO
import Statebox.Client as Stbx
import Statebox.Client (evalTransactionResponse)
import Statebox.Core.Transaction as Stbx
import Statebox.Core.Transaction (HashStr, HashTx, TxSum, evalTxSum)
import Statebox.Core.Transaction.Codec (DecodingError(..))
import View.Diagram.Update as DiagramEditor
import View.Petrinet.Model (Msg(NetUpdated))
import View.Studio.Model (Action(..), State, fromPNPROProject, resolveRoute)
import View.Studio.Model.Route (Route, RouteF(..), ResolvedRouteF(..), NodeIdent(..))
import View.Studio.View (render, ChildSlots)

import ExampleData as Ex

ui :: ∀ m q. MonadAff m => H.Component HTML q Unit Void m
ui =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: mkEval $ defaultEval { handleAction = handleAction }
    }
  where
    initialState :: State
    initialState =
      { msg:         "Welcome to Statebox Studio!"
      , projects:    Ex.projects
      , hashSpace:   AdjacencySpace.empty
      , route:       Home
      }

    handleAction :: Action -> HalogenM State Action ChildSlots Void m Unit
    handleAction = case _ of
      ShowDiagramNodeContent route -> do
        handleAction (SelectRoute route)

      SelectRoute route -> do
        -- H.liftEffect $ log $ "route = " <> show route
        H.modify_ \state -> state { route = route }

      LoadTransaction endpointUrl hash -> do
        H.liftEffect $ log $ "LoadTransaction: requesting transaction " <> hash <> " from " <> endpointUrl
        res <- H.liftAff $ Stbx.requestTransaction endpointUrl hash
        res # evalTransactionResponse
          (\err                 -> H.liftEffect $ log $ "failed to decode HTTP response into JSON: " <> Affjax.printResponseFormatError err)
          (\(DecodingError err) -> H.liftEffect $ log $ "Expected to decode a valid Statebox transaction: " <> show err)
          (\{id, tx}            -> do H.modify_ (\state -> state { hashSpace = AdjacencySpace.update Stbx.getPrevious state.hashSpace id tx })
                                      H.liftEffect $ log $ show tx)

      LoadTransactions endpointUrl startHash -> do
        H.liftEffect $ log $ "LoadTransactions: requesting transactions up to root, starting at " <> startHash <> " from " <> endpointUrl
        let
          txProducer :: Producer HashTx (HalogenM State Action _ Void m) Unit
          txProducer = Stbx.requestTransactionsToRootM endpointUrl startHash

          txConsumer :: Consumer HashTx (HalogenM State Action _ Void m) Unit
          txConsumer = consumer txStorer
            where
              txStorer :: HashTx -> (HalogenM State Action _ Void m) (Maybe _)
              txStorer itx@{id, tx} = do
                H.modify_ (\state -> state { hashSpace = AdjacencySpace.update Stbx.getPrevious state.hashSpace id tx })
                H.liftEffect $ log $ show itx
                pure Nothing

          -- | This ingests transactions from the HTTP API into our transaction storage.
          txIngester :: Process (HalogenM State Action _ Void m) Unit
          txIngester = txProducer `connect` txConsumer

        runProcess txIngester

      LoadPNPRO url -> do
        H.liftEffect $ log $ "LoadPNPRO: requesting PNPRO file from " <> url
        res <- H.liftAff $ Affjax.request $ Affjax.defaultRequest { url = url, responseFormat = ResponseFormat.string }
        res.body # either
          (\err -> H.liftEffect $ log $ "failed to decode HTTP response into JSON: " <> Affjax.printResponseFormatError err)
          (\body -> do
               pnproDocumentE <- H.liftEffect $ try $ PNPRO.fromString body
               pnproDocumentE # either
                 (\err      -> H.liftEffect $ log $ "Error decoding PNPRO document: " <> show err)
                 (\pnproDoc -> H.modify_ $ \state -> state { projects = fromPNPROProject pnproDoc.project `cons` state.projects })
          )

      HandleDiagramEditorMsg (DiagramEditor.OperatorClicked opId) -> do
        H.liftEffect $ log $ "DiagramEditor.OperatorClicked: " <> opId
        state <- H.get
        let
          -- TODO #87 we hardcode the assumption here that opId is a net (NetNode opId) but it could be (LeDiagram opId)
          newRouteMaybe :: Maybe Route
          newRouteMaybe = case state.route of
            Diagram pname dname _ -> Just (Diagram pname dname (Just (NetNode opId)))
            _                     -> Nothing
        maybe (pure unit) (handleAction <<< SelectRoute) newRouteMaybe

      HandlePetrinetEditorMsg NetUpdated -> do
        pure unit
