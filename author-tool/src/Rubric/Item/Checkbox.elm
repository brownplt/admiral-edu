module Rubric.Item.Checkbox (..) where

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

type alias Type = { label : Editable String
                  }

render activate wrap checkbox =
  let activate' = (\event -> activate event)
  in
  Html.div [ Attributes.class "checkbox" ] 
           [ 
             Html.div [ Attributes.class "checkbox-body" ] [ text activate' wrap checkbox.label ]
           ]
text activate wrap label =
  Html.div [ Attributes.class "checkbox-text" ] [ Html.input [ Attributes.type' "checkbox" ] []
                                                , textbox activate (\label m -> wrap { m | label <- label }) label "Set Label" ]


model = { id = editable "new-checkbox-id"
        , label = editable ""
        }

update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text script ], Html.node "style" [] [ Html.text (style ++ style') ], render (\event -> event address) (\id -> id) model ]

main = StartApp.start { model = model, update = update, view = view }

style = """

.checkbox {
  width: 500px;
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
