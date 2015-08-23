module Rubric(Type, new, render, script, style) where
import Common exposing (..)

import Array
import Array exposing (Array)
import Array.Extra
import Editable exposing (..)
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events
import List

import Signal exposing (Address)

import Rubric.Item as Item

import StartApp

type alias Type = { items : Array Item.Type }

render : Activator m -> Wrapper Type m -> Type -> Html
render activate wrap rubric =
  let len = Array.length rubric.items in
  Array.indexedMap (renderIx activate wrap rubric) rubric.items |>
  Array.toList |>
  Html.div [ Attributes.class "rubric" ]

renderIx : Activator m -> Wrapper Type m -> Type -> Int -> Item.Type -> Html
renderIx activate wrap rubric ix item = 
  let wrap' = (\item m -> wrap { rubric | items <- Array.set ix item rubric.items } m )
      delete = if | (Array.length rubric.items) > 1 -> deleteButton activate wrap rubric ix
                  | otherwise -> Html.div [] []
      above = if | ix == 0 -> insertButton activate wrap rubric ix
                 | otherwise -> Html.div [] []
      below = insertButton activate wrap rubric (ix + 1)
  in Html.div [ Attributes.class "rubric-container" ] [ above
                 , Item.render activate wrap' [delete] item
                 , below
                 ]

insertButton : Activator m -> Wrapper Type m -> Type -> Int -> Html
insertButton activator wrap rubric ix =
  let rubric' = { rubric | items <- insertAt ix Item.new rubric.items }
  in Html.div [ Attributes.class "rubric-insert item-hidden" ] [
    Html.input [ Attributes.type' "button"
               , Attributes.title "Insert new item"
               , activator (flip Events.onClick (\m -> wrap rubric' m))
               ] []
       ]

deleteButton : Activator m -> Wrapper Type m -> Type -> Int -> Html
deleteButton activator wrap rubric ix = 
  let rubric' = { rubric | items <- Array.Extra.removeAt ix rubric.items }
  in
    Html.input [ Attributes.type' "button"
               , Attributes.title "Delete this item."
               , Attributes.class "delete-button item-button"
               , activator (flip Events.onClick (\m -> wrap rubric' m))
               ] []


new : Type
new = { items = Array.fromList [ Item.new ] }



style = style' ++
        Item.style

style' = """
.rubric {
  width: 600px;
  margin: auto;
}

.rubric-container {

}

.rubric-insert {
  width: calc(100% - 50px);
  text-align: center;
}

.rubric-insert input {
  width: 20px;
  height: 20px;
  background:url(/images/add-button.png) no-repeat;
  border:0px;
}

div.rubric-insert-item {
  width: calc(100% - 10px);
  margin: 0 5px 0px 5px;
  height: 5px;
  border-top: dashed #999 1px;
  overflow: hidden;
}

div.rubric-insert-item:hover {
  border: dashed #999 1px;  
  height: 100px;
  animation-name: insert-hover;
  animation-duration: 0.5s;
  animation-delay: 0.5s;
  animation-fill-mode: both;
  -webkit-animation-name: insert-hover;
  -webkit-animation-duration: 0.5s;  
  -webkit-animation-delay: 0.5s;
  -webkit-animation-fill-mode: both;
}

.rubric-insert-item select {
  margin: 5px;
}

@-webkit-keyframes insert-hover {
    0%   {height: 5px; 
          border-top: dashed #999 1px; 
          border-left: dashed #fff 1px; 
          border-right: dashed #fff 1px; 
          border-bottom: dashed #fff 1px; 
         }
  100%   {height: 100px; border: dashed #999 1px;  }
}

keyframes insert-hover {
    0%   {height: 5px; 
          border-top: dashed #999 1px; 
          border: dashed #eee 1px; }
  100%   {height: 100px; border: dashed #999 1px;  }
}

"""


script = Item.script

update f m = f m

view address model = 
  Html.div [] 
           [ Html.node "script" [] [ Html.text script ]
           , Html.node "style" [] [ Html.text style]
           , render (\event -> event address) (\t _ -> t) model
           ]
           

main = StartApp.start { model = new, update = update, view = view }
