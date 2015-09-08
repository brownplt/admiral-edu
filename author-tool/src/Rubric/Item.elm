module Rubric.Item (Type, new, render, style, script, toYAML) where
import Common exposing (..)

import Array
import Array exposing (Array)

import Html
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events

import Signal exposing (Address)

import Rubric.Item.Checkbox as Checkbox
import Rubric.Item.FreeForm as FreeForm
import Rubric.Item.Instruction as Instruction
import Rubric.Item.Likert as Likert
import Rubric.Item.Select as Select
import Rubric.Item.Utils as Utils

import StartApp

type alias Type = { types : Array Item
                  , selected : Int
                  }

new : Type
new = { types = Array.fromList [ FreeForm FreeForm.new
                               , Likert Likert.new
                               , Instruction Instruction.new
--                               , Checkbox Checkbox.new
--                               , Select Select.new
                               ]
      , selected = 0
      }

toYAML : Int -> Type -> String
toYAML indent item =
  case (Array.get item.selected item.types) of
    Just (FreeForm freeform) -> FreeForm.toYAML indent freeform
    Just (Likert likert) -> Likert.toYAML indent likert
    Just (Instruction instruction) -> Instruction.toYAML indent instruction

type Item = Checkbox Checkbox.Type
          | FreeForm FreeForm.Type
          | Instruction Instruction.Type
          | Likert Likert.Type
          | Select Select.Type

render : Activator m -> Wrapper Type m -> List Html -> Type -> Html
render activate wrap controls item =
  Html.div [ Attributes.class "item" ]
      [ Html.div [ Attributes.class "item-body" ] [ renderSelected activate wrap item ]
      , Html.div [ Attributes.class "item-controls item-hidden" ] ([ rotate (-1) activate wrap item ] ++ controls)
      ]

rotate : Int -> Activator m -> Wrapper Type m -> Type -> Html
rotate direction activate wrap item =
  let len = Array.length item.types
      next = (item.selected + direction) % len
      item' = {item | selected <- next}
  in Html.input [ Attributes.type' "button"
                , Attributes.class "next-button item-button"
                , Attributes.title "Change item type"
                , activate (flip Events.onClick (\m -> wrap item' m)) ] []

renderSelected : Activator m -> Wrapper Type m -> Type -> Html
renderSelected activate wrap item = 
  let wrap' constructor = (\i m -> wrap { item | types <- Array.set item.selected (constructor i) item.types } m)
  in
  case (Array.get item.selected item.types) of
    Nothing -> Html.div [] [ Html.text "An error occurred." ]
    Just (Checkbox checkbox) -> Checkbox.render activate (wrap' Checkbox) checkbox
    Just (FreeForm freeform) -> FreeForm.render activate (wrap' FreeForm) freeform
    Just (Select select) -> Select.render activate (wrap' Select) select
    Just (Likert likert) -> Likert.render activate (wrap' Likert) likert
    Just (Instruction instruction) -> Instruction.render activate (wrap' Instruction) instruction


style = style' ++
        Checkbox.style ++ 
        FreeForm.style ++ 
        Likert.style ++
        Select.style ++
        Instruction.style ++
        Utils.style

style' = """
.item {
  display: flex;
  width: 600px;
}

.item-hidden {
  opacity: 0;
  animation-name: item-hidden;
  animation-duration: 2s;
  -webkit-animation-name: item-hidden;
  -webkit-animation-duration: 2s;
  -moz-animation-name: item-hidden;
  -moz-animation-duration: 2s;
}

.item-hidden:hover {
  opacity: 1.0;
  animation-name: item-reveal;
  animation-duration: 0.5s;
  -webkit-animation-name: item-reveal;
  -webkit-animation-duration: 0.5s;
  -moz-animation-name: item-reveal;
  -moz-animation-duration: 0.5s;
}

.item-controls {
  width: 30px;
  order: 2;
}

.item-button {
  width: 20px;
  height: 20px;
}

.item-body {
  margin-left: 5px;
  width: calc(100% - 60px); 
  order: 1;
}

@-moz-keyframes item-hidden {
    0%   {opacity: 1.0; }
  100%   {opacity: 0.0; }
}

@-webkit-keyframes item-hidden {
    0%   {opacity: 1.0; }
  100%   {opacity: 0.0; }
}

keyframes item-hidden {
    0%   {opacity: 1.0; }
  100%   {opacity: 0.0; }
}

keyframes item-reveal {
    0%   {opacity: 0.0 }
  100%   {opacity: 1.0 }
}

@-webkit-keyframes item-reveal {
    0%   {opacity: 0.0 }
  100%   {opacity: 1.0 }
}

@-moz-keyframes item-reveal {
    0%   {opacity: 0.0 }
  100%   {opacity: 1.0 }
}

"""

script = Utils.script

update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text script ], Html.node "style" [] [ Html.text (style) ], render (\event -> event address) (\t m -> t) [] model ]

main = StartApp.start { model = new, update = update, view = view }
