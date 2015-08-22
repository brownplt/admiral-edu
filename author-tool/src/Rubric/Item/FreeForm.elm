module Rubric.Item.FreeForm (..) where
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

type alias Type = { id : Editable String
                  , text : Editable String
                  }

new : Type
new = { id = editable "", text = editable "" }


render : Activator m -> Wrapper Type m -> Type -> Html
render activate wrap likert =
  let activate' = (\event -> activate event)
  in
  Html.div [ Attributes.class "freeform" ] 
           [ 
           Html.div [ Attributes.class "freeform-body" ] [ text activate' wrap likert
                                                         , Html.textarea [ Attributes.disabled True ] []
                                                         ]
           ]

id : Activator m -> Wrapper Type m -> Type -> Html
id activate wrap likert = 
  let wrap' = (\id m -> wrap { likert | id <- id } m) in
  Html.div [ Attributes.class "freeform-id" ] [ textbox activate wrap' likert.id "click to set id"]

text : Activator m -> Wrapper Type m -> Type -> Html
text activate wrap likert =
  let wrap' = (\text m -> wrap { likert | text <- text } m) in
  Html.div [ Attributes.class "freeform-text" ] [ textbox activate wrap' likert.text "Set prompt" ]

style = """

.freeform {
  padding: 5px;
}

.freeform textarea {
  margin:5px 5px 5px 5px;
  min-height: 100px;
  width: calc(100% - 15px);
}

.freeform-text textarea {
  width: calc(100% - 5px);
}

.freeform-body {
  border-style: solid;
  border-width: 1px;
  border-radius: 5px 5px 5px 5px;
  overflow: hidden;
}

.freeform-id {
  padding: 3px;
  width: 200px;
  overflow: visible;
  border-style: solid;
  border-width: 1px 1px 0px 1px;
  border-radius: 5px 5px 0px 0px;
}


.freeform-text {
  padding: 5px;
  background: #eee;
  border-style: solid;
  border-width: 0px 0px 1px 0px;
}
"""


update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text Utils.script ], Html.node "style" [] [ Html.text (style ++ Utils.style) ], render (\event -> event address) (\t m -> t) model ]

main = StartApp.start { model = new, update = update, view = view }
