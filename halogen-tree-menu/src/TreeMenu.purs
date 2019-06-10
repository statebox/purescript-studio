module TreeMenu where

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
import Halogen.HTML.Properties (classes, src, href, id_)
import Halogen.HTML.Properties.ARIA as ARIA

--------------------------------------------------------------------------------

componentCssClassNameStr = "css-object-chooser"

componentCssClassName = ClassName componentCssClassNameStr

--------------------------------------------------------------------------------

data Query r a
  = VisitRoute NodeId r a
  | ToggleExpandCollapse NodeId a
  | UpdateTree (RoseTree (Item r)) a

-- | What the component emits to the outside world.
data Msg r = Clicked NodeId r

type State r =
  { tree       :: Maybe (RoseTree (NodeId /\ Item r))
  , expansion  :: Map NodeId Boolean
  , activeItem :: Maybe NodeId
  , hideRoot   :: Boolean
  }

--------------------------------------------------------------------------------

type MenuTree r = RoseTree (Item r)

type RoseTree a = Cofree Array a

type Item r = { label :: String, route :: Maybe r }

mkItem :: forall r. String -> Maybe r -> Item r
mkItem label route = { label, route }

-- TODO this is a list because it stores the path down to the current node, but we could just scan the tree instead
type NodeId = Array Int

--------------------------------------------------------------------------------

menuComponent
  :: forall m r
   . MonadAff m
  => (r -> Boolean)
  -> Component HTML (Query r) (RoseTree (Item r)) (Msg r) m
menuComponent isSelected =
  H.component { initialState, render, eval, receiver: HE.input UpdateTree }
  where
    initialState :: RoseTree (Item r) -> State r
    initialState tree =
      { tree: pure (decorateWithIds tree)
      , expansion: Map.empty
      , activeItem: Nothing
      , hideRoot: true
      }

    eval :: Query r ~> ComponentDSL (State r) (Query r) _ m
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

    render :: State r -> HTML Void (Query r Unit)
    render state = fromMaybe (div [] []) $ state.tree <#> \tree ->
      nav [ classesWithNames [ componentCssClassNameStr, "p-4" ] ]
          [ ul [ classesWithNames [ "list-reset" ] ] $
               if state.hideRoot then (semifoldCofree menuItemHtml <$> tail tree)
                                 else [semifoldCofree menuItemHtml  $       tree]
          ]
      where
        menuItemHtml :: (NodeId /\ Item r)  -> Array (HTML Void (Query r Unit)) -> HTML Void (Query r Unit)
        menuItemHtml (treeNodeId /\ treeNode) kids =
          li [ classesWithNames [ "block", "flex", "cursor-pointer", "pr-2", "text-grey-darkest" ] ]
             [ div [ classesWithNames [ "inline-flex" ] ]
                   [ arrowIcon
                   , div [ classesWithNames [ "flex-1" ] ]
                         [ div ( [ classesWithNames $ [ "p-1", "rounded" ] <> activeClasses ] <> onClickVisitRoute )
                               [ text treeNode.label ]
                         , div []
                               [ ul [ classesWithNames $ [ "list-reset", if isExpanded then "block" else "hidden" ] ]
                                    kids
                               ]
                         ]
                     ]
             ]
          where
            activeClasses = if isActive then [ "is-active", "bg-purple-darker", "text-purple-lighter" ]
                                        else [ "hover:bg-grey-lighter" ]
            arrowIcon     = div ([ classesWithNames [ "fas", "fa-xs"
                                                    , "pr-1"
                                                    , "rounded-l"
                                                    , "text-grey-dark"
                                                    , "hover:bg-grey-lighter", "hover:text-grey-darker"
                                                    , if null kids then "fa-fw"
                                                                   else "fa-caret-" <> if isExpanded then "down"
                                                                                                     else "right"
                                                    ]
                                 ] <> onClickExpandCollapse
                                )
                                []

            -- TODO handling of Nothing case of map retrieval is spread over 2 diff places
            isExpanded = not null kids && (fromMaybe true $ Map.lookup treeNodeId state.expansion)
            isActive = state.activeItem == pure treeNodeId

            onClickVisitRoute     = maybe [] (pure <<< onClick <<< HE.input_ <<< VisitRoute treeNodeId) treeNode.route
            onClickExpandCollapse = guard (not null kids) [ onClick <<< HE.input_ $ ToggleExpandCollapse treeNodeId ]

decorateWithIds :: forall r. RoseTree (Item r) -> RoseTree (NodeId /\ Item r)
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

--------------------------------------------------------------------------------

classesWithNames = classes <<< map ClassName
