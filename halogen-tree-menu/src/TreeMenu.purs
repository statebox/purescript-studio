module TreeMenu where

import Prelude hiding (div)
import Effect.Aff.Class (class MonadAff)
import Control.Comonad.Cofree
import Data.Foldable (null)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen.HTML (HTML, a, b, details, li, nav, span, summary, text, ul)
import Halogen.HTML.Core (ClassName(..), AttrName(..))
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (IProp, attr, classes, href)
import View.ReactiveInput as ReactiveInput
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

--------------------------------------------------------------------------------

componentCssClassNameStr :: String
componentCssClassNameStr = "css-object-chooser"

--------------------------------------------------------------------------------

data Action r
  = VisitRoute r
  | PreventDefault (Action r) Event

-- | What the component emits to the outside world.
data Msg r = Clicked r

type State =
  { hideRoot   :: Boolean
  }

type Input r =
  { tree       :: MenuTree r
  , isSelected :: r -> Boolean
  }

--------------------------------------------------------------------------------

type MenuTree r = RoseTree (Item r)

type RoseTree a = Cofree Array a

type Item r = { label :: String, route :: Maybe r }

mkItem :: forall r. String -> Maybe r -> Item r
mkItem label route = { label, route }

mapMenuTreeRoutes :: ∀ ra rb. (ra -> rb) -> MenuTree ra -> MenuTree rb
mapMenuTreeRoutes f = map \{ label, route } -> { label, route: map f route }

--------------------------------------------------------------------------------

menuComponent
  :: forall m r q
   . MonadAff m
  => Component HTML q (Input r) (Msg r) m
menuComponent =
  ReactiveInput.mkComponent { initialState, render, handleAction, handleInput: \_ -> pure unit }
  where
    initialState :: State
    initialState = { hideRoot: true }

    handleAction :: Input r -> Action r -> HalogenM State (Action r) () (Msg r) m Unit
    handleAction inp = case _ of
      VisitRoute route -> do
        H.raise (Clicked route)

      PreventDefault action event -> do
        H.liftEffect $ Event.preventDefault event
        handleAction inp action

    render :: Input r -> State -> ComponentHTML (Action r) () m
    render { tree, isSelected } state =
      nav [ classesWithNames [ componentCssClassNameStr ] ]
          [ ul [ classesWithNames [ "is-unstyled" ] ] $
               if state.hideRoot then (semifoldCofree menuItemHtml <$> tail tree)
                                 else [semifoldCofree menuItemHtml  $       tree]
          ]
      where
        menuItemHtml :: Item r -> Array (ComponentHTML (Action r) () m) -> ComponentHTML (Action r) () m
        menuItemHtml treeNode kids =
          li [] if null kids then leaf
                             else [ details [ attr (AttrName "open") "true" ]
                                            [ summary [] leaf
                                            , ul [ classesWithNames [ "is-unstyled" ] ] kids
                                            ]
                                  ]
          where
            leaf = [ a ([ href "#" ] <> onClickVisitRoute)
                       [ selected [ text treeNode.label ] ]
                   ]
            selected = if map isSelected treeNode.route == pure true then b [] else span []
            onClickVisitRoute = treeNode.route #
              maybe [] (\r -> pure <<< onClick $ Just <<< PreventDefault (VisitRoute r) <<< toEvent)

semifoldCofree :: forall f a b. Functor f => (a -> f b -> b) -> Cofree f a -> b
semifoldCofree f1 tree = f1 (head tree) (semifoldCofree f1 <$> tail tree)

--------------------------------------------------------------------------------

classesWithNames :: forall r i. Array String -> IProp (class :: String | r) i
classesWithNames = classes <<< map ClassName
