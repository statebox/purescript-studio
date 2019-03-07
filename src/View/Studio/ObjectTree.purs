module View.Studio.ObjectTree where

import Prelude hiding (div)
import Effect.Aff.Class (class MonadAff)
import Control.Comonad.Cofree
import Control.Comonad
import Data.Foldable (null)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map as Map
import Data.Map (Map)
import Data.Monoid (guard)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen as H
import Halogen (Component, ComponentDSL, ParentHTML)
import Halogen.HTML (HTML, nav, div, p, a, text, ul, li, aside, span, i)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, src, href)
import Halogen.HTML.Properties.ARIA as ARIA

import View.Studio.Route (Route, RouteF(..))
import View.Common (classesWithNames)

--------------------------------------------------------------------------------

componentCssClassNameStr = "css-object-chooser"

componentCssClassName = ClassName componentCssClassNameStr

--------------------------------------------------------------------------------

data Query a
  = VisitRoute NodeId Route a
  | ToggleExpandCollapse NodeId a
  | UpdateTree (RoseTree Item) a

-- | What the component emits to the outside world.
data Msg = Clicked NodeId Route

type State =
  { tree       :: Maybe (RoseTree (NodeId /\ Item))
  , expansion  :: Map NodeId Boolean
  , activeItem :: Maybe NodeId
  , hideRoot   :: Boolean
  }

--------------------------------------------------------------------------------

type MenuTree = RoseTree Item

type RoseTree a = Cofree Array a

type Item = { label :: String, route :: Maybe Route }

mkItem :: String -> Maybe Route -> Item
mkItem label route = { label, route }

-- TODO this is a list because it stores the path down to the current node, but we could just scan the tree instead
type NodeId = Array Int

--------------------------------------------------------------------------------

menuComponent
  :: forall m
   . MonadAff m
  => (Route -> Boolean)
  -> Component HTML Query (RoseTree Item) Msg m
menuComponent isSelected =
  H.component { initialState, render, eval, receiver: HE.input UpdateTree }
  where
    initialState :: RoseTree Item -> State
    initialState tree =
      { tree: pure (decorateWithIds tree)
      , expansion: Map.empty
      , activeItem: Nothing
      , hideRoot: true
      }

    eval :: Query ~> ComponentDSL State Query _ m
    eval = case _ of
      UpdateTree tree next -> do
        H.modify_ \state -> state { tree = pure $ decorateWithIds tree }
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
      nav [ classesWithNames [ componentCssClassNameStr, "p-4" ] ]
          [ ul [ classesWithNames [ "list-reset" ] ] $
               if state.hideRoot then (semifoldCofree menuItemHtml <$> tail tree)
                                 else [semifoldCofree menuItemHtml  $       tree]
          ]
      where
        menuItemHtml :: (NodeId /\ Item)  -> Array (HTML Void (Query Unit)) -> HTML Void (Query Unit)
        menuItemHtml (treeNodeId /\ treeNode) kids =
          li [ classesWithNames ([ "block", "flex", "cursor-pointer", "px-2", "py-2", "text-grey-darkest" ] <> activeClasses)]
             [ div []
                   [ arrowIcon
                   , span [ classesWithNames [ "pl-2" ]
                          , onClick (HE.input_ clickQuery)
                          ]
                          [ text treeNode.label ]
                     , if isExpanded then ul   [ classesWithNames [ "list-reset", "mt-2" ] ] kids
                                     else span [ classesWithNames [ "no-children" ] ] []
                     ]
             ]
          where
            activeClasses = if isActive then [ "is-active", "bg-purple-darker", "text-purple-lighter", "rounded" ] else []
            arrowIcon     = if null kids then text ""
                                         else span [ classesWithNames [ "fas" , "fa-xs"
                                                          , "fa-caret-" <> if isExpanded then "down" else "right"
                                                          ]
                                                   , onClick (HE.input_ clickQuery)
                                                   ] []

            clickQuery    = maybe (ToggleExpandCollapse treeNodeId) (VisitRoute treeNodeId) treeNode.route

            -- TODO handling of Nothing case of map retrieval is spread over 2 diff places
            isExpanded = not null kids && (fromMaybe true $ Map.lookup treeNodeId state.expansion)
            isActive = state.activeItem == pure treeNodeId

decorateWithIds :: RoseTree Item -> RoseTree (NodeId /\ Item)
decorateWithIds tree = mapWithIndexCofree (/\) tree

semifoldCofree :: forall f a b. Functor f => (a -> f b -> b) -> Cofree f a -> b
semifoldCofree f1 tree = f1 (head tree) (semifoldCofree f1 <$> tail tree)

-- TODO not stack safe, see `Control.Monad.Rec` etc:
-- - https://github.com/dmbfm/purescript-tree/blob/3480a95c938920dcef10997de1782f99f1f272ba/src/Data/Tree.purs#L28
mapWithIndexCofree :: forall f a b
   . FunctorWithIndex Int f
  => Applicative f
  => Monoid (f Int)
  => (f Int -> a -> b)
  -> Cofree f a
  -> Cofree f b
mapWithIndexCofree = mapWithIndexCofree' mempty
  where
    mapWithIndexCofree' :: f Int -> (f Int -> a -> b) -> Cofree f a -> Cofree f b
    mapWithIndexCofree' level f cf =
      f level (head cf) :< f' `mapWithIndex` tail cf
      where
        f' :: Int -> Cofree f a -> Cofree f b
        f' i xs = mapWithIndexCofree' (level <> pure i) f xs
