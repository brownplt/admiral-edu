module Rubric.Item.Select (..) where

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
import Array exposing (Array)
import Array

type alias Type = { id : Editable String
                  , text : Editable String
                  , options :  Array (Editable String)
                  , new : Editable String
                  }

new : Type
new = { id = editable ""
      , text = editable ""
      , options = Array.empty
      , new = editable ""
      }

render : ((Address (m -> m) -> Attribute) -> Attribute) -> (Type -> m -> m) -> Type -> Html
render activate wrap select =
  let activate' = (\event -> activate event)
  in
  Html.div [ Attributes.class "select" ] 
           [ Html.div [ Attributes.class "select-body" ] 
                   [ text activate' wrap select
                   , Html.div [ Attributes.class "select-options" ] 
                           [options activate' wrap select]
                   ]
           ]

options : ((Address (m -> m) -> Attribute) -> Attribute) -> (Type -> m -> m) -> Type -> Html
options activate wrap select = 
  Html.div [] 
            ((Array.toList 
                     (Array.indexedMap 
                             (option select.id.value activate wrap select) select.options)) 
             ++ [newoption activate wrap select])
  


id : ((Address (m -> m) -> Attribute) -> Attribute) -> (Type -> m -> m) -> Type -> Html
id activate wrap select = 
  let wrap' = (\id m -> wrap { select | id <- id } m ) in
  Html.div [ Attributes.class "select-id" ] 
           [ textbox activate wrap' select.id "click to set id"]


text : ((Address (m -> m) -> Attribute) -> Attribute) -> (Type -> m -> m) -> Type -> Html
text activate wrap select =
  let wrap' = (\text m -> wrap { select | text <- text } m ) in
  Html.div [ Attributes.class "select-text" ] 
           [ textarea activate wrap' select.text "Set prompt" ]


option : String -> ((Address (m -> m) -> Attribute) -> Attribute) -> (Type -> m -> m) -> Type -> Int -> Editable String -> Html
option id activate wrap select ix label = 
  let wrap' = (\label m -> wrap (if | not label.editing && label.value == "" -> { select | options <- removeIndex ix select.options }
                                    | otherwise -> { select | options <- Array.set ix label select.options }) m)
  in
  Html.div [] 
           [ Html.input [ Attributes.type' "radio", Attributes.name id ] []
           , textbox activate wrap'
                     label
                     ""
           ]


newoption : ((Address (m -> m) -> Attribute) -> Attribute) -> (Type -> m -> m) -> Type -> Html
newoption activate wrap select = 
  let wrap' = (\option m -> wrap (if | option.editing -> { select | new <- option }
                                     | option.value == "" -> select
                                     | otherwise -> { select | options <- Array.push (editable option.value) select.options
                                                             , new <- editable ""
                                                    }) m)
  in
  Html.div [ Attributes.class "select-new" ] 
           [ Html.input [ Attributes.type' "radio", Attributes.disabled True ] []
           , textbox activate wrap' select.new "Add Label"
           ]

removeIndex ix array = 
  let front = Array.slice 0 ix array
      back = Array.slice (ix+1) (Array.length array) array
  in Array.append front back


style = """

.select {
  padding: 5px;
}

.select-text textarea {
  width: calc(100% - 5px);
}

.select-body {
  border-style: solid;
  border-width: 1px;
  border-radius: 5px 5px 5px 5px;
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

