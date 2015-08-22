module Rubric.Item.Likert (..) where
import Common exposing (..)

import Rubric.Item.Utils exposing (textbox, textarea)
import Rubric.Item.Utils as Utils
import Rubric.Item.Utils exposing (..)
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
                  , minLabel : Editable String
                  , maxLabel : Editable String
                  , granularity : Editable Int
                  }

new : Type
new = { id = editable ""
      , text = editable ""
      , minLabel = editable ""
      , maxLabel = editable ""
      , granularity = editable 6
      }


render : Activator m -> Wrapper Type m -> Type -> Html
render activate wrap likert =
  let activate' = (\event -> activate event)
  in
  Html.div [ Attributes.class "likert" ] 
           [ 
           Html.div [ Attributes.class "likert-body" ] [ text activate' wrap likert
                                                       , Html.div [ Attributes.class "likert-scale" ] 
                                                               [ minLabel activate' wrap likert
                                                               , granularity activate' wrap likert
                                                               , maxLabel activate' wrap likert
                                                               ]
                                                       ]
           ]

id : Activator m -> Wrapper Type m -> Type -> Html
id activate wrap likert = 
  let wrap' = (\id m -> wrap { likert | id <- id } m ) in 
  Html.div [ Attributes.class "likert-id" ] [ textbox activate wrap' likert.id "click to set id"]

text : Activator m -> Wrapper Type m -> Type -> Html
text activate wrap likert =
  let wrap' = (\text m -> wrap { likert | text <- text } m ) in
  Html.div [ Attributes.class "likert-text" ] [ textbox activate wrap' likert.text "Set prompt" ]

minLabel : Activator m -> Wrapper Type m -> Type -> Html
minLabel activate wrap likert =
  let wrap' = (\minLabel m -> wrap { likert | minLabel <- minLabel } m) in
  Html.div [ Attributes.class "likert-minlabel" ] [ textbox activate wrap' likert.minLabel "Set Label" ]

maxLabel : Activator m -> Wrapper Type m -> Type -> Html
maxLabel activate wrap likert =
  let wrap' = (\maxLabel m -> wrap { likert | maxLabel <- maxLabel } m) in
  Html.div [ Attributes.class "likert-maxlabel" ] [ textbox activate wrap' likert.maxLabel "Set Label" ]

granularity : Activator m -> Wrapper Type m -> Type -> Html
granularity activate wrap likert =
  Html.div [ Attributes.class "likert-granularity" ]
        [ decreaseButton activate wrap likert
        , Html.span [] (List.repeat likert.granularity.value (Html.input [Attributes.type' "radio", Attributes.disabled True] []))
        , increaseButton activate wrap likert
        ]

decreaseButton : Activator m -> Wrapper Type m -> Type -> Html
decreaseButton activate wrap likert = 
  let wrap' = (\gran m -> wrap { likert | granularity <- editable (gran - 1) } m) in
  if | likert.granularity.value <= 2 -> Html.span [] []
     | otherwise -> Html.button [ activate (flip Events.onClick (wrap' likert.granularity.value)) ] [Html.text "-"]
 
increaseButton : Activator m -> Wrapper Type m -> Type -> Html
increaseButton activate wrap likert = 
  let wrap' = (\gran m -> wrap { likert | granularity <- editable (gran + 1) } m) in
     Html.button [ activate (flip Events.onClick (wrap' likert.granularity.value)) ] [Html.text "+"]

style = """

.likert {
  padding: 5px;
}

.likert-text textarea {
  width: calc(100% - 5px);
}

.likert-body {
  border-style: solid;
  border-width: 1px;
  border-radius: 5px 5px 5px 5px;
  overflow: hidden;
}

.id input {
  width: 100px;
}

.likert-id {
  padding: 3px;
  width: 200px;
  overflow: visible;
  border-style: solid;
  border-width: 1px 1px 0px 1px;
  border-radius: 5px 5px 0px 0px;
}

.likert-scale {
  padding: 5px;
  display: flex;
  justify-content: space-between;
}

.likert-text {
  padding: 5px;
  background: #eee;
  border-style: solid;
  border-width: 0px 0px 1px 0px;
}

.likert-granularity button {
  margin: 0px 5px 0px 5px;
}

.likert-maxlabel {
  text-align: right;
}

"""
