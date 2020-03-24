module TreeMenu where

import Prelude hiding (div)
import Effect.Aff.Class (class MonadAff)
import Control.Comonad.Cofree
import Data.Foldable (null)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen.HTML (HTML, a, b, button, details, input, li, nav, span, summary, text, ul)
import Halogen.HTML.Core (ClassName(..), AttrName(..))
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (IProp, attr, classes, href, type_, value, InputType(InputText))
import View.CRUDAction
import View.ReactiveInput as ReactiveInput
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

--------------------------------------------------------------------------------

componentCssClassNameStr :: String
componentCssClassNameStr = "css-object-chooser"

--------------------------------------------------------------------------------

data Action act
  = Raise act
  | PreventDefault (Action act) Event

type State =
  { hideRoot   :: Boolean
  }

type Input act r =
  { tree       :: MenuTree act r
  , isSelected :: r -> Boolean
  , editMode   :: Boolean
  , onVisit    :: r -> act
  }

--------------------------------------------------------------------------------

type MenuTree act r = RoseTree (Item act r)

type RoseTree a = Cofree Array a

data WrapAction = WrapAction (forall t. String -> CRUDAction { name :: String | t })
type Edit act = Maybe (WrapAction -> act)
type Item act r = { label :: String, route :: Maybe r, onEdit :: Edit act }

mkItem :: ∀ act r. String -> Maybe r -> Edit act -> Item act r
mkItem label route onEdit = { label, route, onEdit }

mapMenuTreeRoutes :: ∀ act ra rb. (ra -> rb) -> MenuTree act ra -> MenuTree act rb
mapMenuTreeRoutes f = map \{ label, route, onEdit } -> { label, route: map f route, onEdit }

--------------------------------------------------------------------------------

menuComponent
  :: ∀ m r q act
   . MonadAff m
  => Component HTML q (Input act r) act m
menuComponent =
  ReactiveInput.mkComponent { initialState, render, handleAction, handleInput: \_ -> pure unit }
  where
    initialState :: State
    initialState = { hideRoot: true }

    handleAction :: Input act r -> Action act -> HalogenM State (Action act) () act m Unit
    handleAction inp = case _ of
      Raise act -> do
        H.raise act

      PreventDefault action event -> do
        H.liftEffect $ Event.preventDefault event
        handleAction inp action

    render :: Input act r -> State -> ComponentHTML (Action act) () m
    render { tree, isSelected, editMode, onVisit } state =
      nav [ classesWithNames [ componentCssClassNameStr ] ]
          [ ul [ classesWithNames [ "is-unstyled" ] ] $
               if state.hideRoot then (semifoldCofree menuItemHtml <$> tail tree)
                                 else [semifoldCofree menuItemHtml  $       tree]
          ]
      where
        menuItemHtml :: Item act r -> Array (ComponentHTML (Action act) () m) -> ComponentHTML (Action act) () m
        menuItemHtml treeNode kids =
          li [] if null kids then leaf
                             else [ details [ attr (AttrName "open") "true" ]
                                            [ summary [] leaf
                                            , ul [ classesWithNames [ "is-unstyled" ] ] kids
                                            ]
                                  ]
          where
            leaf = case editMode, treeNode.onEdit of
              true, Just onEdit ->
                [ button [ onClick \_ -> Just $ Raise $ onEdit $ WrapAction DeleteAction] [ text "X" ]
                , input [ type_ InputText
                        , value treeNode.label
                        , onValueInput \v -> Just $ Raise $ onEdit $ WrapAction (UpdateAction $ _ { name = v })
                        ]
                ]
              _, _ -> [ a ([ href "#" ] <> onClickVisitRoute) [ selected [ text treeNode.label ] ] ]
            selected = if map isSelected treeNode.route == pure true then b [] else span []
            onClickVisitRoute = treeNode.route #
              maybe [] (\r -> pure <<< onClick $ Just <<< PreventDefault (Raise (onVisit r)) <<< toEvent)

semifoldCofree :: ∀ f a b. Functor f => (a -> f b -> b) -> Cofree f a -> b
semifoldCofree f1 tree = f1 (head tree) (semifoldCofree f1 <$> tail tree)

--------------------------------------------------------------------------------

classesWithNames :: ∀ r i. Array String -> IProp (class :: String | r) i
classesWithNames = classes <<< map ClassName
