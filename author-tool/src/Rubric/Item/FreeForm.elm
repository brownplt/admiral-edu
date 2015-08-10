module Rubric.Item.FreeForm (..) where

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
                  }

render activate wrap likert =
  let activate' = (\event -> activate event)
  in
  Html.div [ Attributes.class "freeform" ] 
           [ 
             id activate' wrap likert.id 
           , Html.div [ Attributes.class "freeform-body" ] [ text activate' wrap likert.text
                                                           , Html.textarea [ Attributes.disabled True ] []
                                                           ]
           ]

id activate wrap id = 
  Html.div [ Attributes.class "freeform-id" ] [ textbox activate (\id m -> wrap { m | id <- id }) id "click to set id"]

text activate wrap text =
  Html.div [ Attributes.class "freeform-text" ] [ textarea activate (\text m -> wrap { m | text <- text }) text "Set prompt" ]


model = { id = editable "new-freeform-id"
        , text = editable ""
        }

update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text script ], Html.node "style" [] [ Html.text (style ++ style') ], render (\event -> event address) (\id -> id) model ]

main = StartApp.start { model = model, update = update, view = view }

style = """

.freeform {
  width: 500px;
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
