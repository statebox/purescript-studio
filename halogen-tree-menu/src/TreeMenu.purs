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
import Halogen (Component, ComponentHTML, HalogenM, mkEval, defaultEval)
import Halogen.HTML (HTML, nav, div, p, a, text, ul, li, aside, span, i, b, details, summary)
import Halogen.HTML.Core (ClassName(..), AttrName(..))
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, src, href, id_, attr, IProp)
import Halogen.HTML.Properties.ARIA as ARIA
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

--------------------------------------------------------------------------------

componentCssClassNameStr = "css-object-chooser"

--------------------------------------------------------------------------------

data Action r
  = VisitRoute NodeId r
  | UpdateTree (RoseTree (Item r))
  | PreventDefault (Action r) Event

-- | What the component emits to the outside world.
data Msg r = Clicked NodeId r

type State r =
  { tree       :: Maybe (RoseTree (NodeId /\ Item r))
  , activeItem :: Maybe NodeId
  , hideRoot   :: Boolean
  }

--------------------------------------------------------------------------------

type MenuTree r = RoseTree (Item r)

type RoseTree a = Cofree Array a

type Item r = { label :: String, route :: Maybe r }

mkItem :: forall r. String -> Maybe r -> Item r
mkItem label route = { label, route }

mapMenuTreeRoutes :: forall ra rb. (ra -> rb) -> MenuTree ra -> MenuTree rb
mapMenuTreeRoutes f = map (\{ label, route } -> { label, route: map f route })

-- TODO this is a list because it stores the path down to the current node, but we could just scan the tree instead
type NodeId = Array Int

--------------------------------------------------------------------------------

menuComponent
  :: forall m r q
   . MonadAff m
  => (r -> Boolean)
  -> Component HTML q (RoseTree (Item r)) (Msg r) m
menuComponent isSelected =
  H.mkComponent { eval: mkEval $ defaultEval { receive = Just <<< UpdateTree, handleAction = handleAction }, initialState, render }
  where
    initialState :: RoseTree (Item r) -> State r
    initialState tree =
      { tree: pure (decorateWithIds tree)
      , activeItem: Nothing
      , hideRoot: true
      }

    handleAction :: Action r -> HalogenM (State r) (Action r) () (Msg r) m Unit
    handleAction = case _ of
      UpdateTree tree -> do
        H.modify_ \state -> state { tree = pure $ decorateWithIds tree }

      VisitRoute pathId route -> do
        H.modify_ (\state -> state { activeItem = Just pathId })
        H.raise (Clicked pathId route)

      PreventDefault action event -> do
        H.liftEffect $ Event.preventDefault event
        handleAction action

    render :: State r -> ComponentHTML (Action r) () m
    render state = fromMaybe (div [] []) $ state.tree <#> \tree ->
      nav [ classesWithNames [ componentCssClassNameStr ] ]
          [ ul [ classesWithNames [ "is-unstyled" ] ] $
               if state.hideRoot then (semifoldCofree menuItemHtml <$> tail tree)
                                 else [semifoldCofree menuItemHtml  $       tree]
          ]
      where
        menuItemHtml :: (NodeId /\ Item r)  -> Array (ComponentHTML (Action r) () m) -> ComponentHTML (Action r) () m
        menuItemHtml (treeNodeId /\ treeNode) kids =
          li [] if null kids then leaf
                             else [ details [ attr (AttrName "open") "true" ]
                                            [ summary [] leaf
                                            , ul [ classesWithNames [ "is-unstyled" ] ] kids
                                            ]
                                  ]
          where
            leaf = [ a ([ href "#" ] <> onClickVisitRoute)
                       [ activated [ text treeNode.label ] ]
                   ]
            activated = if state.activeItem == pure treeNodeId || map isSelected treeNode.route == pure true then b [] else span []
            onClickVisitRoute = treeNode.route #
              maybe [] (\r -> pure <<< onClick $ Just <<< PreventDefault (VisitRoute treeNodeId r) <<< toEvent)

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

classesWithNames :: forall r i. Array String -> IProp (class :: String | r) i
classesWithNames = classes <<< map ClassName
