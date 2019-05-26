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
import Halogen.HTML as HH
import Halogen.HTML (HTML, div, span, text, a, br, hr, button, input, textarea, select, option, label, fieldset, legend)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events (input_, onClick, onChecked, onValueInput, onValueChange)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (classes, disabled, src, width, height, type_, value, rows, placeholder, InputType(..), checked, name)

import Data.Auth (Role(..), Roles(..), Privilege(..), RoleInfo, rolesElem, isPrivileged, toPrivilege, CSSColor(..))
import Data.Auth as Auth
import View.Petrinet.Model (TransitionQueryF(..), Typedef(..))
import View.Common (styleStr, classesWithNames)

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
  titledPanel "Transition properties" $
    formContainer
      [ fieldContainer "name" "grid-label-name" $
          input [ classesWithNames inputClasses1
                , value (maybe "" (_.label) mm)
                , maybe (disabled true)
                        (\tid -> onValueChange (HE.input (UpdateTransitionName tid)))
                        (mm <#> _.tid)
                ]
      , fieldContainer "type"  "grid-label-type" $
          input [ classesWithNames inputClasses1
                , value (maybe "" (un Typedef <<< _.typedef) mm)
                , maybe (disabled true)
                        (\tid -> onValueChange (HE.input (UpdateTransitionType tid <<< Typedef)))
                        (mm <#> _.tid)
                ]
      , fieldContainer "roles" "grid-label-roles" $
          div [] (authCheckboxes <<< foldMap _.auths $ mm)
      ]
  where
    titledPanel title content =
      div [ classesWithNames [ "mb-2", "border-solid", "border-grey-light", "rounded", "border", "shadow-sm" ] ]
          [ div [ classesWithNames [ "bg-grey-lighter", "px-2", "py-3", "border-solid", "border-grey-light", "border-b", "text-grey-darker" ] ]
                [ text title ]
          , content
          ]

    formContainer formContent =
      div [ classesWithNames [ "bg-white", "rounded", "flex", "flex-col", "px-4", "pt-6" ] ]
          formContent

    fieldContainer :: String -> String -> HTML _ _ -> HTML _ _
    fieldContainer labelText forInputId content =
      div [ classesWithNames [ "-mx-3", "md:flex", "mb-6" ] ]
          [ div [ classesWithNames [ "md:w-full", "px-3" ] ]
                [ label1 forInputId labelText
                , content
                ]
          ]

    label1 forInputId labelText  =
      HH.label [ classesWithNames [ "block", "uppercase", "tracking-wide", "text-grey-darker", "text-xs", "font-bold", "mb-2" ]
               , HP.for forInputId
               ]
               [ text labelText  ]

    inputClasses1 =
      [ "appearance-none", "block", "w-full", "bg-grey-lightest", "text-grey-darker", "border", "border-grey-lighter", "rounded", "py-2", "px-3" ]

    -- TODO regenerating this every time is expensive; perhaps store in State like this instead of as an Array
    allRoleInfosDict :: Map Role RoleInfo
    allRoleInfosDict = Map.fromFoldable <<< map (\role -> role.id /\ role) $ allRoleInfos

    authCheckboxes :: Roles -> Array (HTML _ _)
    authCheckboxes roles = checkboxContainer <<< (\roleInfo -> roleCheckbox allRoleInfosDict roleInfo $ priv roles roleInfo.id) <$> allRoleInfos
      where
        checkboxContainer html = div [ classesWithNames [ "mt-4", "mb-4" ] ] [ html ]

    priv :: Roles -> Role -> Privilege
    priv privilegedRoles role = toPrivilege <<< rolesElem role $ privilegedRoles

roleCheckbox :: Map Role RoleInfo -> RoleInfo -> Privilege -> HTML _ _
roleCheckbox allRoleInfosDict roleInfo privilege =
  label [ classes [ ClassName "radio" ] ]
        [ input [ classes [ ClassName "mr-2" ]
                , type_ InputCheckbox
                , checked (isPrivileged privilege)
                ]
        , roleTagHtml allRoleInfosDict roleInfo.id
        ]

roleTagHtml :: Map Role RoleInfo -> Role -> HTML _ _
roleTagHtml roleInfosDict role =
  span [ classes [ ClassName $ "py-1 px-2 no-underline rounded-full text-white font-sans font-semibold text-sm btn-primary focus:outline-none active:shadow-none mr-2"
                                <> " bg-blue border-blue hover:text-white hover:bg-blue-light"
                 ]
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
