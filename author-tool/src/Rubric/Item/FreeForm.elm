module Rubric.Item.FreeForm (..) where

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

render : ((Address (m -> m) -> Attribute) -> Attribute) -> (Type -> m -> m) -> Type -> Html
render activate wrap likert =
  let activate' = (\event -> activate event)
  in
  Html.div [ Attributes.class "freeform" ] 
           [ 
             id activate' wrap likert
           , Html.div [ Attributes.class "freeform-body" ] [ text activate' wrap likert
                                                           , Html.textarea [ Attributes.disabled True ] []
                                                           ]
           ]

id : ((Address (m -> m) -> Attribute) -> Attribute) -> (Type -> m -> m) -> Type -> Html
id activate wrap likert = 
  let wrap' = (\id m -> wrap { likert | id <- id } m) in
  Html.div [ Attributes.class "freeform-id" ] [ textbox activate wrap' likert.id "click to set id"]

text : ((Address (m -> m) -> Attribute) -> Attribute) -> (Type -> m -> m) -> Type -> Html
text activate wrap likert =
  let wrap' = (\text m -> wrap { likert | text <- text } m) in
  Html.div [ Attributes.class "freeform-text" ] [ textarea activate wrap' likert.text "Set prompt" ]

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
  border-radius: 0px 5px 5px 5px;
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

