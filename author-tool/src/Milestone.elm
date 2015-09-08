module Milestone where
import Common exposing (..)

import Array
import Array exposing (Array)
import Array.Extra

import Editable exposing (..)

import Html
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events

import Milestone.Task as Task

import Rubric
import Rubric.Item.Utils as Utils

import StartApp

import String

type alias Type = { title : Editable String
                  , text : Editable String
                  , tasks : Array Task.Type
                  }

new : Type
new = { title = Editable.new ""
      , text = Editable.new ""
      , tasks = Array.empty
      }

toYAML : Int -> Type -> String
toYAML indent milestone =
  let indent' = String.repeat indent " "
      id = String.split " " milestone.title.value |>
           String.join "-" |>
           String.toLower
      text = Utils.sanitize milestone.text.value
      tasks = if | Array.isEmpty milestone.tasks -> ""
                 | otherwise -> yamlTasks (indent + 2) milestone.tasks
  in indent' ++ "- id: " ++ id ++ "\n"  ++
     indent' ++ "  instructions: \"" ++ text ++ "\"\n" ++
     tasks

yamlTasks : Int -> Array Task.Type -> String
yamlTasks indent tasks =
  let indent' = String.repeat indent " "
      tasks' = Array.indexedMap (yamlTasks' (indent + 2)) tasks |>
               Array.toList |>
               String.join "\n"
  in indent' ++ "reviews:\n" ++
     tasks'

yamlTasks' : Int -> Int -> Task.Type -> String
yamlTasks' indent ix task =
  let id = "task-" ++ (toString ix)
  in Task.toYAML id indent task

render : Activator m -> Wrapper Type m -> List Html -> Type -> Html
render activate wrap controls milestone =
  Html.div [ Attributes.class "milestone" ] 
           [ Html.div [ Attributes.class "milestone-title" ] [title activate wrap controls milestone]
           , Html.div [ Attributes.class "milestone-body" ]
                      [ text activate wrap milestone
                      , tasks activate wrap milestone 
                      ]
           ]

title : Activator m -> Wrapper Type m -> List Html -> Type -> Html
title activate wrap controls milestone = 
  let wrap' = (\title m -> wrap { milestone | title <- title} m)
      title = Utils.textbox activate wrap' milestone.title "Set Title"
  in
  controls ++ [ Html.h1 [ ] [ title ] ] |>
  Html.div []

text : Activator m -> Wrapper Type m -> Type -> Html
text activate wrap milestone =
  let wrap' = (\text m -> wrap { milestone | text <- text } m ) 
  in
  Html.div [ ] 
           [ Html.span [ Attributes.class "milestone-label" ] [ Html.text "Instructions" ]
           , Html.div [ Attributes.class "milestone-instructions" ] 
                      [ Utils.markdown activate wrap' milestone.text "Add markdown" ]
           ]

tasks : Activator m -> Wrapper Type m -> Type -> Html
tasks activate wrap milestone =
  let len = Array.length milestone.tasks 
      class = if | len > 0 -> "milestone-insert item-hidden"
                 | otherwise -> "milestone-insert small-button"
      label = Html.span [ Attributes.class "milestone-label" ] [ Html.text "Tasks" ]
  in
  Array.indexedMap (renderIx activate wrap milestone) milestone.tasks |>
  Array.toList |>
  (\tasks -> (tasks ++ [ Html.div [ Attributes.class class ] [insertButton "" activate wrap len milestone] ])) |>
  (\tasks ->
  Html.div [ ]
           [ label
           , Html.div [ Attributes.class "milestone-tasks" ] tasks
           ]
  )

renderIx : Activator m -> Wrapper Type m -> Type -> Int -> Task.Type -> Html
renderIx activate wrap milestone ix task = 
  let controls = [ deleteTask activate wrap ix milestone, insertButton "small-button add-button" activate wrap ix milestone ]
      wrap' = (\task m -> wrap { milestone | tasks <- Array.set ix task milestone.tasks } m)
  in
  [ Task.render activate wrap' controls task ] |>
  Html.div [ Attributes.class "milestone-task" ]

deleteTask : Activator m -> Wrapper Type m -> Int -> Type -> Html
deleteTask activate wrap ix milestone =
  let milestone' = { milestone | tasks <- Array.Extra.removeAt ix milestone.tasks }
  in Html.input [ Attributes.type' "button"
                , Attributes.class "small-button delete-button"
                , Attributes.title "Remove this task"
                , activate (flip Events.onClick (\m -> wrap milestone' m))
                ] []

insertButton : String -> Activator m -> Wrapper Type m -> Int -> Type -> Html
insertButton class activator wrap ix milestone =
  let milestone' = { milestone | tasks <- insertAt ix Task.new milestone.tasks }
  in Html.input [ Attributes.type' "button"
                , Attributes.title "Insert new task"
                , Attributes.class class
                , activator (flip Events.onClick (\m -> wrap milestone' m))
                ] []


style = """

.milestone {
  width: 650px;
  margin: 5px;
}

.milestone-title {
  border-top: solid black 1px;
  border-right: solid black 1px;
  border-left: solid black 1px;
  border-radius: 5px 5px 0px 0px;
  padding: 5px;
  width: 300px;
  display:inline-block;
}


.milestone-title h1 {
  font-weight: bold;
  font-size: 18px;
  margin: 0;
  padding: 0;
}

.milestone-title input {
  font-weight: bold;
  font-size: 18px;
}

.milestone-body {
  margin: 0px;
  border-radius: 0px 5px 5px 5px;
  border: solid black 1px;
  padding: 5px;
}

.milestone-insert {
  margin: 5px 0px 5px 0px;
}

.milestone-insert input {
  border: 0px;
  width: 20px;
  height: 20px;
  background:url(/images/add-button.png) no-repeat;
  opacity:0.8;
  overflow:visible;
}

.milestone-insert input:hover {
  opacity:1.0;
}

.milestone-button {
  width: 20px;
  height: 20px;
  float: right;
  opacity: 0.8;
}

.milestone-button:hover {
  opacity: 1.0;
}

.milestone-task {
  margin-bottom: 5px;
}

.milestone-tasks {
  padding: 5px;
}

.milestone-label {
  text-decoration: underline;
}

.milestone-instructions {
  padding: 5px;
}

.milestone-instructions textarea {
  width: 100%;
  height: 100px;
}


.milestone-instructions p {
  margin: 2px;
}
"""

style' = Task.style ++
         Rubric.style ++
         style


update n m = n m

view address model = Html.div [] [ Html.node "script" [] [ Html.text Rubric.script ], Html.node "style" [] [ Html.text style' ], render (\event -> event address) (\t m -> t) [] model ]

main = StartApp.start { model = new, update = update, view = view }

