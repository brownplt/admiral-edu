module Rubric.Item.Select (..) where

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
import Array exposing (Array)
import Array

type alias Type = { id : Editable String
                  , text : Editable String
                  , options :  Array (Editable String)
                  , new : Editable String
                  }

render activate wrap select =
  let activate' = (\event -> activate event)
  in
  Html.div [ Attributes.class "select" ] 
           [ 
             id activate' wrap select.id 
           , Html.div [ Attributes.class "select-body" ] 
                      [ text activate' wrap select.text
                      , Html.div [ Attributes.class "select-options" ] 
                                 (options activate' wrap select)
                      ]
           ]

options activate wrap select = 
  [Html.div [] 
            ((Array.toList (Array.indexedMap (option select.id.value activate wrap select.options) select.options)) ++  [newoption activate wrap select])
  ]

id activate wrap id = 
  Html.div [ Attributes.class "select-id" ] 
           [ textbox activate (\id m -> wrap { m | id <- id }) id "click to set id"]

text activate wrap text =
  Html.div [ Attributes.class "select-text" ] 
           [ textarea activate (\text m -> wrap { m | text <- text }) text "Set prompt" ]

option id activate wrap options ix label = 
  Html.div [] 
           [ Html.input [ Attributes.type' "radio", Attributes.name id ] []
           , textbox activate 
                     (\label m -> wrap (if | not label.editing && label.value == "" -> { m | options <- removeIndex ix options }
                                           | otherwise -> { m | options <- Array.set ix label options })
                     ) 
                     label
                     ""
           ]

newoption activate wrap select = 
  Html.div [ Attributes.class "select-new" ] 
           [ Html.input [ Attributes.type' "radio", Attributes.disabled True ] []
           , textbox activate 
                     (\option m -> wrap (if | option.editing -> { m | new <- option }
                                            | option.value == "" -> m
                                            | otherwise -> { m | options <- Array.push (editable option.value) select.options
                                                           , new <- editable ""
                                                           }
                                        )
                     )
                     select.new 
                     "Add Label"
           ]

removeIndex ix array = 
  let front = Array.slice 0 ix array
      back = Array.slice (ix+1) (Array.length array) array
  in Array.append front back

update n m = n m

view address model = 
  Html.div [] 
           [ Html.node "script" [] [ Html.text script ]
           , Html.node "style" [] [ Html.text (style ++ style') ]
           , render (\event -> event address) (\id -> id) model 
           ]

model : Type
model = { id = editable "new-select-id"
        , text = editable ""
        , options = Array.empty
        , new = editable ""
        }

main = StartApp.start { model = model, update = update, view = view }

style = """

.select {
  width: 500px;
  padding: 5px;
}

.select-text textarea {
  width: calc(100% - 5px);
}

.select-body {
  border-style: solid;
  border-width: 1px;
  border-radius: 0px 5px 5px 5px;
  overflow: hidden;
}

.id input {
  width: 100px;
}

.select-id {
  padding: 3px;
  width: 200px;
  overflow: visible;
  border-style: solid;
  border-width: 1px 1px 0px 1px;
  border-radius: 5px 5px 0px 0px;
}

.select-options {
  padding: 5px;
  display: flex;
  justify-content: space-between;
}

.select-text {
  padding: 5px;
  background: #eee;
  border-style: solid;
  border-width: 0px 0px 1px 0px;
}

.select-granularity button {
  margin: 0px 5px 0px 5px;
}

.select-maxlabel {
  text-align: right;
}

.select-new {
  font-style: italic; 
  color: #666;
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
