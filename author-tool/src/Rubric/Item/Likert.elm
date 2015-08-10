module Rubric.Item.Likert (..) where

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

render activate wrap likert =
  let activate' = (\event -> activate event)
  in
  Html.div [ Attributes.class "likert" ] 
           [ 
             id activate' wrap likert.id 
           , Html.div [ Attributes.class "likert-body" ] [ text activate' wrap likert.text
                                                         , Html.div [ Attributes.class "likert-scale" ] [ minLabel activate' wrap likert.minLabel
                                                                                                        , granularity activate' (\granularity m -> wrap { m | granularity <- granularity } ) likert.granularity
                                                                                                        , maxLabel activate' wrap likert.maxLabel
                                                                                                        ]
                                                         ]
           ]

id activate wrap id = 
  Html.div [ Attributes.class "likert-id" ] [ textbox activate (\id m -> wrap { m | id <- id }) id "click to set id"]

text activate wrap text =
  Html.div [ Attributes.class "likert-text" ] [ textarea activate (\text m -> wrap { m | text <- text }) text "Set prompt" ]

minLabel activate wrap minLabel =
  Html.div [ Attributes.class "likert-minlabel" ] [ textbox activate (\minLabel m -> wrap { m | minLabel <- minLabel }) minLabel "Set Label" ]

maxLabel activate wrap maxLabel =
  Html.div [ Attributes.class "likert-maxlabel" ] [ textbox activate (\maxLabel m -> wrap { m | maxLabel <- maxLabel }) maxLabel "Set Label" ]

granularity activate wrap granularity =
  Html.div [ Attributes.class "likert-granularity" ]
        [ decreaseButton activate wrap granularity
        , Html.span [] (List.repeat granularity.value (Html.input [Attributes.type' "radio", Attributes.disabled True] []))
        , increaseButton activate wrap granularity
        ]

decreaseButton activate wrap granularity = 
  if | granularity.value <= 2 -> Html.span [] []
     | otherwise -> Html.button [ activate (flip Events.onClick (wrap { granularity | value <- granularity.value - 1 })) ] [Html.text "-"]

increaseButton activate wrap granularity = 
     Html.button [ activate (flip Events.onClick (wrap { granularity | value <- granularity.value + 1 })) ] [Html.text "+"]

model = { id = editable "new-likert-id"
        , text = editable ""
        , minLabel = editable ""
        , maxLabel = editable ""
        , granularity = editable 5
        }

update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text script ], Html.node "style" [] [ Html.text (style ++ style') ], render (\event -> event address) (\id -> id) model ]

main = StartApp.start { model = model, update = update, view = view }

style = """

.likert {
  width: 500px;
  padding: 5px;
}

.likert-text textarea {
  width: calc(100% - 5px);
}

.likert-body {
  border-style: solid;
  border-width: 1px;
  border-radius: 0px 5px 5px 5px;
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


style' = """

.focus {
  animation-name: set-focus;
  animation-duration: 0.001s;
  -webkit-animation-name: set-focus;
  -webkit-animation-duration: 0.001s;
}

@-webkit-keyframes set-focus {
    0%   {color: #fff}
}

keyframes set-focus {
    0%   {color: #fff}
}

"""

script = """
var insertListener = function(event){
 if (event.animationName == "set-focus") {
   event.target.focus();
 }               
}
document.addEventListener("animationstart", insertListener, false); // standard + firefox
document.addEventListener("MSAnimationStart", insertListener, false); // IE
document.addEventListener("webkitAnimationStart", insertListener, false); // Chrome + Safari
"""
