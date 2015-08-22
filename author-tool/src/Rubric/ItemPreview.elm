module Rubric.Item.ItemPreview where

import Html
import Rubric.Item as Item
import Rubric.Item exposing (style, script, render)
import Rubric.Item.Utils as Utils

import StartApp

model = Item.new

update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text script ], Html.node "style" [] [ Html.text (style) ], render (\event -> event address) (\t m -> t) [] model ]

main = StartApp.start { model = model, update = update, view = view }
