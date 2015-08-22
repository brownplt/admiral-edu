module RubricPreview where

import StartApp
import Html
import Rubric exposing (Type, render, new, style, script)

model = new

update f m = f m

view address model = 
  Html.div [] 
           [ Html.node "script" [] [ Html.text script ]
           , Html.node "style" [] [ Html.text style]
           , render (\event -> event address) (\t _ -> t) model
           ]
           

main = StartApp.start { model = model, update = update, view = view }

