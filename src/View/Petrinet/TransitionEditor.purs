module View.Petrinet.TransitionEditor where

import Prelude hiding (div)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, find, elem, foldMap)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isNothing)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen as H
import Halogen.HTML (HTML, div, span, text, a, br, hr, button, input, textarea, select, option, label, fieldset, legend)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events (input_, onClick, onChecked, onValueInput, onValueChange)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes, disabled, src, width, height, type_, value, rows, placeholder, InputType(..), checked, name)

import Data.Auth (Role(..), Roles(..), Privilege(..), RoleInfo, rolesElem, isPrivileged, toPrivilege, CSSColor(..))
import Data.Auth as Auth
import View.Petrinet.Model (TransitionQueryF(..), Typedef(..))
import View.Common (styleStr)


type TransitionEditorFormModel tid =
  { tid         :: tid
  , label       :: String
  , typedef     :: Typedef
  , isWriteable :: Boolean
  , auths       :: Roles
  }

form' :: ∀ tid a. Array RoleInfo -> TransitionEditorFormModel tid -> HTML a ((TransitionQueryF tid) Unit)
form' allRoleInfos m = form allRoleInfos (Just m)

form :: ∀ tid a. Array RoleInfo -> Maybe (TransitionEditorFormModel tid) -> HTML a ((TransitionQueryF tid) Unit)
form allRoleInfos mm =
  div []
      [ div [ classes [ ClassName "field", ClassName "is-horizontal" ] ]
            [ div [ classes [ ClassName "field-label" ] ]
                  [ label [ classes [ ClassName "label" ] ]
                          [ text "label" ]
                  ]
            , div [ classes [ ClassName "field-body" ] ]
                  [ div [ classes [ ClassName "field" ] ]
                        [ div [ classes [ ClassName "control" ] ]
                              [ input [ classes [ ClassName "input" ]
                                      , value (maybe "" (_.label) mm)
                                      , maybe (disabled true)
                                              (\tid -> onValueChange (HE.input (UpdateTransitionName tid)))
                                              (mm <#> _.tid)
                                      ]
                              ]
                        ]
                  ]
            ]

      --------------------------------------------------------------------------

      , div [ classes [ ClassName "field", ClassName "is-horizontal" ] ]
            [ div [ classes [ ClassName "field-label" ] ]
                  [ label [ classes [ ClassName "label" ] ]
                          [ text "type" ]
                  ]
            , div [ classes [ ClassName "field-body" ] ]
                  [ div [ classes [ ClassName "field" ] ]
                        [ div [ classes [ ClassName "control" ] ]
                              [ input [ classes [ ClassName "input" ]
                                      , value (maybe "" (un Typedef <<< _.typedef) mm)
                                      , maybe (disabled true)
                                              (\tid -> onValueChange (HE.input (UpdateTransitionType tid <<< Typedef)))
                                              (mm <#> _.tid)
                                      ]
                              ]
                        ]
                  ]
            ]

      --------------------------------------------------------------------------

      , div [ classes [ ClassName "field", ClassName "is-horizontal" ] ]
            [ div [ classes [ ClassName "field-label" ] ]
                  [ label [ classes [ ClassName "label" ] ]
                          [ text "roles" ]
                  ]
            , div [ classes [ ClassName "field-body" ] ]
                  [ div [ classes [ ClassName "field is-narrow" ] ]
                        [ div [ classes [ ClassName "control" ] ]
                              (authCheckboxes <<< foldMap _.auths $ mm)
                        ]
                  ]
            ]
      ]
  where
    -- TODO regenerating this every time is expensive; perhaps store in State like this instead of as an Array
    allRoleInfosDict :: Map Role RoleInfo
    allRoleInfosDict = Map.fromFoldable <<< map (\role -> role.id /\ role) $ allRoleInfos

    authCheckboxes :: Roles -> Array (HTML _ _)
    authCheckboxes roles = (\roleInfo -> roleCheckbox allRoleInfosDict roleInfo $ priv roles roleInfo.id) <$> allRoleInfos

    priv :: Roles -> Role -> Privilege
    priv privilegedRoles role = toPrivilege <<< rolesElem role $ privilegedRoles

roleCheckbox :: Map Role RoleInfo -> RoleInfo -> Privilege -> HTML _ _
roleCheckbox allRoleInfosDict roleInfo privilege =
  label [ classes [ ClassName "radio" ] ]
        [ input [ classes [ ClassName "radio" ]
                , type_ InputCheckbox
                , checked (isPrivileged privilege)
                ]
        , roleTagHtml allRoleInfosDict roleInfo.id
        ]

roleTagHtml :: Map Role RoleInfo -> Role -> HTML _ _
roleTagHtml roleInfosDict role =
  span [ classes [ ClassName "tag" ]
       , styleStr [ "background-color" /\ (un CSSColor) backgroundColor
                  , "color"            /\ (un CSSColor) textColor
                  ]
       ]
       [ text roleName ]
  where
    roleName        = maybe "unknown role"    _.name      roleInfoMaybe
    backgroundColor = maybe (CSSColor "#ddd") _.bgColor   roleInfoMaybe
    textColor       = maybe (CSSColor "#666") _.textColor roleInfoMaybe
    roleInfoMaybe   = Map.lookup role roleInfosDict
