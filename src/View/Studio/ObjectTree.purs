module View.Studio.ObjectTree where

import Prelude hiding (div)
import Effect.Aff.Class (class MonadAff)
import Control.Comonad.Cofree
import Control.Comonad
import Data.Foldable (foldMap, null)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map as Map
import Data.Map (Map)
import Data.Monoid (guard)
import Halogen as H
import Halogen (Component, ComponentDSL, ParentHTML)
import Halogen.HTML (HTML, nav, div, p, a, text, ul, li, aside, span, i)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, src, href)
import Halogen.HTML.Properties.ARIA as ARIA

import View.Studio.Route (Route, RouteF(..), routesObjNameEq)

--------------------------------------------------------------------------------

componentCssClassNameStr = "css-object-chooser"

componentCssClassName = ClassName componentCssClassNameStr

--------------------------------------------------------------------------------

type Tree = Cofree Array Item

data Query a
  = VisitRoute PathId Route a
  | ToggleExpandCollapse PathId a
  | UpdateTree (Cofree Array Item) a

-- | What the component emits to the outside world.
data Msg = Clicked PathId Route

type State =
  { tree       :: Maybe Tree
  , expansion  :: Map PathId Boolean
  , activeItem :: Maybe PathId
  , hideRoot   :: Boolean
  }

--------------------------------------------------------------------------------

type Item =
  { id    :: PathId
  , label :: String
  , route :: Maybe Route
  }

mkItem id label route = { id, label, route }

-- TODO this is a list because it stores the path down to the current node, but we can just scan the tree instead
type PathId = Array String

--------------------------------------------------------------------------------

menuComponent
  :: forall m
   . MonadAff m
  => (Route -> Boolean)
  -> Component HTML Query Tree Msg m
menuComponent isSelected =
  H.component { initialState, render, eval, receiver: HE.input UpdateTree }
  where
    initialState :: Tree -> State
    initialState tree =
      { tree: pure tree
      , expansion: Map.empty
      , activeItem: Nothing
      , hideRoot: true
      }

    eval :: Query ~> ComponentDSL State Query _ m
    eval = case _ of
      UpdateTree tree next -> do
        H.modify_ \state -> state { tree = pure tree }
        pure next

      VisitRoute pathId route next -> do
        H.modify_ (\state -> state { activeItem = Just pathId })
        H.raise (Clicked pathId route)
        pure next

      ToggleExpandCollapse pathId next -> do
        state <- H.get
        let e = Map.lookup pathId state.expansion
        let e' = not (fromMaybe true e)
        let state' = state { expansion = Map.insert pathId e' state.expansion }
        H.put state'
        pure next

    render :: State -> HTML Void (Query Unit)
    render state = fromMaybe (div [] []) $ state.tree <#> \tree ->
      nav [ clzz [ componentCssClassNameStr, "p-4" ] ]
          [ ul [ clzz [ "list-reset" ] ] $
               if state.hideRoot then (semifoldCofree menuItemHtml <$> tail tree)
                                 else [semifoldCofree menuItemHtml  $       tree]
          ]
      where
        menuItemHtml :: Item -> Array (HTML Void (Query Unit)) -> HTML Void (Query Unit)
        menuItemHtml treeNode kids =
          li [ clzz ([ "block", "flex", "cursor-pointer", "px-2", "py-2", "text-grey-darkest" ] <> activeClasses)]
             [ div []
                   [ arrowIcon
                   , span [ clzz [ "pl-2" ]
                          , onClick (HE.input_ clickQuery)
                          ]
                          [ text treeNode.label ]
                     , if isExpanded then ul   [ clzz [ "list-reset", "mt-2" ] ] kids
                                     else span [ clzz [ "no-children" ] ] []
                     ]
             ]
          where
            activeClasses = if isActive then [ "is-active", "bg-purple-darker", "text-purple-lighter" ] else []
            arrowIcon     = if null kids then text ""
                                         else span [ clzz [ "fas" , "fa-xs"
                                                          , "fa-caret-" <> if isExpanded then "down" else "right"
                                                          ]
                                                   , onClick (HE.input_ clickQuery)
                                                   ] []

            clickQuery    = maybe (ToggleExpandCollapse treeNode.id) (VisitRoute treeNode.id) treeNode.route

            -- TODO handling of Nothing case of map retrieval is spread over 2 diff places
            isExpanded = not null kids && (fromMaybe true $ Map.lookup treeNode.id state.expansion)
            isActive = state.activeItem == pure treeNode.id

clzz :: Array String -> _
clzz classStrs = classes (ClassName <$> classStrs)

semifoldCofree :: forall f a b. Functor f => (a -> f b -> b) -> Cofree f a -> b
semifoldCofree f1 tree = f1 (head tree) (semifoldCofree f1 <$> tail tree)
