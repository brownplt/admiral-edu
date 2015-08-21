module Rubric.Item.LikertPreview where

import Rubric.Item.Likert exposing (..)
import Editable exposing (..)
import Html
import StartApp
import Rubric.Item.Utils as Utils

model = { id = editable "new-likert-id"
        , text = editable ""
        , minLabel = editable ""
        , maxLabel = editable ""
        , granularity = editable 5
        }

update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text Utils.script ], Html.node "style" [] [ Html.text (style ++ Utils.style) ], render (\event -> event address) (\t m -> t) model ]

main = StartApp.start { model = model, update = update, view = view }
