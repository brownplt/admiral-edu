module Rubric where

import Array
import Array exposing (Array)
import Editable exposing (..)
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes

import Signal exposing (Address)
import StartApp

import Rubric.Item.Checkbox as Checkbox
import Rubric.Item.FreeForm as FreeForm
import Rubric.Item.Likert as Likert
import Rubric.Item.Select as Select
import Rubric.Item.Utils as Utils
import Rubric.Item as Item
import Rubric.Item exposing (Item(..))

type alias Action m = m -> m
type alias Activator m = (Address (Action m) -> Attribute) -> Attribute
type alias Wrapper m = Type -> Action m
type alias Type = { items : Array Item }

render : Activator m -> Wrapper m -> Type -> Html
render activate wrap rubric =
  Array.indexedMap (renderIx activate wrap rubric) rubric.items |>
  Array.toList |>
  (\items -> items ++ [newItem activate wrap rubric]) |>
  Html.div [ Attributes.class "rubric" ]

renderIx : Activator m -> Wrapper m -> Type -> Int -> Item -> Html
renderIx activate wrap rubric ix item =
  let wrap' constructor = (\item m -> wrap { rubric | items <- Array.set ix (constructor item) rubric.items } m )
  in case item of
    Checkbox checkbox -> Checkbox.render activate (wrap' Checkbox) checkbox
    FreeForm freeform -> FreeForm.render activate (wrap' FreeForm) freeform
    Select select -> Select.render activate (wrap' Select) select
    Likert likert -> Likert.render activate (wrap' Likert) likert

newItem : Activator m -> Wrapper m -> Type -> Html
newItem activate wrap rubric =
  Html.div [ Attributes.class "rubric-insert-item" ] 
           [] 

model : Type
model = { items = Array.fromList [ Checkbox { label = editable "Foo" }
                                 , Checkbox { label = editable "Bar" }
                                 , FreeForm { id = editable "explain"
                                            , text = editable "Explain"
                                            }
                                 , Select { id = editable "new-select-id"
                                          , text = editable ""
                                          , options = Array.empty
                                          , new = editable ""
                                          }
                                 , Checkbox { label = editable "Gak" }
                                 , Likert { id = editable "new-likert-id"
                                          , text = editable ""
                                          , minLabel = editable ""
                                          , maxLabel = editable ""
                                          , granularity = editable 5
                                          }
                                 ]
        }

update : (Type -> Type) -> Type -> Type
update f m = f m

view : Address (Type -> Type) -> Type -> Html
view address model = 
  Html.div [] 
           [ Html.node "script" [] [ Html.text Utils.script ]
           , Html.node "style" [] [ Html.text (style ++ Utils.style)]
           , render (\event -> event address) (\t _ -> t) model
           ]
           


style = style' ++
        Checkbox.style ++ 
        FreeForm.style ++ 
        Likert.style ++
        Select.style

style' = """
.rubric {
  width: 500px;
  margin: auto;
  background: #eee;
}

div.rubric-insert-item {
  width: calc(100% - 10px);
  margin: 0 5px 0 5px;
  height: 5px;
  border: 0px;
}

div.rubric-insert-item:hover {
  border: solid black 1px;  
  height: 15px;
  animation-name: insert-hover;
  animation-duration: 0.5;
  -webkit-animation-name: insert-hover;
  -webkit-animation-duration: 0.5s;  
}

@-webkit-keyframes insert-hover {
    0%   {height: 5px}
  100%   {height: 15px}
}

keyframes insert-hover {
    0%   {height: 5px}
  100%   {height: 15px}
}



"""

main = StartApp.start { model = model, update = update, view = view }

