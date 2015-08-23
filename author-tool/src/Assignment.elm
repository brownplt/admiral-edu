module Assignment where
import Common exposing (..)

import Array
import Array exposing (Array)
import Array.Extra

import Editable
import Editable exposing (Editable)

import Html
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events

import Milestone

import Rubric 

import StartApp

import Rubric.Item.Utils as Utils

type alias Type = { title : Editable String
                  , instructions : Editable String
                  , milestones : Array Milestone.Type
                  }

new : Type
new = { title = Editable.new ""
      , instructions = Editable.new ""
      , milestones = Array.fromList [ Milestone.new ]
      }

render : Activator m -> Wrapper Type m -> List Html -> Type -> Html
render activate wrap controls assignment =
  Html.div [] [ title activate wrap controls assignment 
              , Html.div [ Attributes.class "assignment-body" ]
                         [ instructions activate wrap assignment
                         , milestones activate wrap assignment ]
              ]

title : Activator m -> Wrapper Type m -> List Html -> Type -> Html
title activate wrap controls assignment =
  let wrap' = (\title m -> wrap { assignment | title <- title} m)
      title = Utils.textbox activate wrap' assignment.title "Assignment Name"
  in
    [ Html.h1 [ Attributes.class "assignment-title" ] [ title ] ] ++ controls |>
  Html.div []

instructions : Activator m -> Wrapper Type m -> Type -> Html
instructions activate wrap assignment =
  let wrap' = (\instructions m -> wrap { assignment | instructions <- instructions } m)
  in Html.div [ ] 
              [ Html.span [ Attributes.class "assignment-label" ] [ Html.text "Instructions" ]
              , Html.div [ Attributes.class "assignment-instructions" ]
                         [ Utils.markdown activate wrap' assignment.instructions "Add markdown" ]
              ]

milestones : Activator m -> Wrapper Type m -> Type -> Html
milestones activate wrap assignment =
  let len = Array.length assignment.milestones
      label = Html.span [ Attributes.class "assignment-label" ] [ Html.text "Milestones" ]
  in Array.indexedMap (renderIx activate wrap assignment) assignment.milestones |>
     Array.toList |>
     (\milestones ->
        Html.div [] [ label
                    , Html.div [ Attributes.class "assignment-milestones" ] 
                               ( milestones ++ 
                               [ insertButton "assignment-insert-last item-hidden" activate wrap len assignment ]
                               )
                    ]
     )

insertButton : String -> Activator m -> Wrapper Type m -> Int -> Type -> Html
insertButton class activate wrap ix assignment =
  let assignment' = { assignment | milestones <- insertAt ix Milestone.new assignment.milestones }
  in Html.input [ Attributes.type' "button"
                , Attributes.title "Insert new milestone"
                , Attributes.class class
                , activate (flip Events.onClick (\m -> wrap assignment' m))
                ] []

renderIx : Activator m -> Wrapper Type m -> Type -> Int -> Milestone.Type -> Html
renderIx activate wrap assignment ix milestone = 
  let len = Array.length assignment.milestones
      delete = if | len > 1 -> deleteButton activate wrap assignment ix
                  | otherwise -> Html.div [] []
      controls = [ delete, insertButton "small-button add-button" activate wrap ix assignment ]
      wrap' = (\milestone m -> wrap { assignment | milestones <- Array.set ix milestone assignment.milestones } m)
  in [ Milestone.render activate wrap' controls milestone ] |>
     Html.div [ Attributes.class "assignment-milestone" ]

deleteButton : Activator m -> Wrapper Type m -> Type -> Int -> Html
deleteButton activate wrap assignment ix =
  Html.input [ Attributes.class "delete-button small-button" 
             , Attributes.type' "button"
             , Attributes.title "Remove this milestone"
             , activate <| flip Events.onClick (\m -> wrap { assignment | milestones <- Array.Extra.removeAt ix assignment.milestones } m)
             ] []


style = """

input.assignment-insert-last {
  width: 300px;
  min-height:20px;
  border:0px;
  background: url(/images/add-button.png) no-repeat;
}

input.small-button  {
  width: 20px;
  height: 20px;
  float: right;
  opacity:0.8;
}

input.small-button:hover {
  opacity:1.0;
}

input.delete-button {
  background:url(/images/close-button.png) no-repeat;
  border:0px;
}

input.add-button {
  background:url(/images/add-button.png) no-repeat;
  border:0px;
}

input.next-button {
  background:url(/images/next-button.png) no-repeat;
  border:0px;
}

input.prev-button {
  background:url(/images/prev-button.png) no-repeat;
  border:0px;
}


h1.assignment-title {
  font-weight: bold;
  font-size: 20px;
  padding: 5px;
  margin: 0px;
}

.assignment-title input {
  font-weight: bold;
  font-size: 20px;
}

.assignment-body {
  padding: 5px;
}

.assignment-label {
  text-decoration: underline;
}

.assignment-instructions textarea {
  width: 600px;
  height: 100px;
}

.assignment-instructions {
  padding: 5px;
}

.assignment-milestones {
  padding: 5px;
}
           
"""

style' = Milestone.style' ++
         style


update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text Rubric.script ], Html.node "style" [] [ Html.text style' ], render (\event -> event address) (\t m -> t) [] model ]

main = StartApp.start { model = new, update = update, view = view }
