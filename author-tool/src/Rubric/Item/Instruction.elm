module Rubric.Item.Instruction (..) where
import Common exposing (..)

import Rubric.Item.Utils exposing (textbox, textarea, sanitize)
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

toYAML : Int -> Type -> String
toYAML indent instruction =
  let indent' = String.repeat indent " "
  in indent' ++ "- instruction: \"" ++ (sanitize instruction.label.value) ++ "\""

render : Activator m -> Wrapper Type m -> Type -> Html
render activate wrap instruction =
  Html.div [ Attributes.class "instruction" ] 
           [ 
             Html.div [ Attributes.class "instruction-body" ] [ text activate wrap instruction ]
           ]

text : Activator m -> Wrapper Type m -> Type -> Html
text activate wrap instruction =
  Html.div [ Attributes.class "instruction-text" ]
           [ textbox activate (\label m -> wrap { instruction | label <- label } m) instruction.label "Set Instructions"
           ]


style = """

.instruction {
  padding: 5px;
}

.instruction textarea {
  margin:5px 5px 5px 5px;
  min-height: 100px;
  width: calc(100% - 15px);
}

.instruction-text textarea {
  width: calc(100% - 5px);
}

.instruction-body {
  border-style: solid;
  border-width: 1px;
  border-radius: 5px;
  overflow: hidden;
}

.instruction-id {
  padding: 3px;
  width: 200px;
  overflow: visible;
  border-style: solid;
  border-width: 1px 1px 0px 1px;
  border-radius: 5px 5px 0px 0px;
}


.instruction-text {
  padding: 5px;
}
"""


update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text Utils.script ], Html.node "style" [] [ Html.text (style ++ Utils.style) ], render (\event -> event address) (\t m -> t) model ]

main = StartApp.start { model = new, update = update, view = view }
