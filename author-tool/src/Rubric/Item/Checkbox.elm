module Rubric.Item.Checkbox (..) where
import Common exposing (..)

import Rubric.Item.Utils exposing (textbox, textarea)
import Rubric.Item.Utils as Utils
import Editable exposing (..)
import Html
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import StartApp
import Html.Events as Events
import Signal exposing  (Address)
import List
import String

type alias Type = { label : Editable String }

new : Type
new = { label = editable "" }

render : Activator m -> Wrapper Type m -> Type -> Html
render activate wrap checkbox =
  Html.div [ Attributes.class "checkbox" ] 
           [ 
             Html.div [ Attributes.class "checkbox-body" ] [ text activate wrap checkbox ]
           ]

text : Activator m -> Wrapper Type m -> Type -> Html
text activate wrap checkbox =
  Html.div [ Attributes.class "checkbox-text" ]
           [ Html.input [ Attributes.type' "checkbox" ] []
           , textbox activate (\label m -> wrap { checkbox | label <- label } m) checkbox.label "Set Label"
           ]


style = """

.checkbox {
  padding: 5px;
}

.checkbox textarea {
  margin:5px 5px 5px 5px;
  min-height: 100px;
  width: calc(100% - 15px);
}

.checkbox-text textarea {
  width: calc(100% - 5px);
}

.checkbox-body {
  border-style: solid;
  border-width: 1px;
  border-radius: 5px;
  overflow: hidden;
}

.checkbox-id {
  padding: 3px;
  width: 200px;
  overflow: visible;
  border-style: solid;
  border-width: 1px 1px 0px 1px;
  border-radius: 5px 5px 0px 0px;
}


.checkbox-text {
  padding: 5px;
}
"""


update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text Utils.script ], Html.node "style" [] [ Html.text (style ++ Utils.style) ], render (\event -> event address) (\t m -> t) model ]

main = StartApp.start { model = new, update = update, view = view }
