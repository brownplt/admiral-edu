module Milestone.Task where
import Common exposing (..)

import Array
import Array exposing (Array)

import Html
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events

import Rubric

import StartApp

import Milestone.Task.PeerReview as PeerReview
import Milestone.Task.ControlledReview as ControlledReview
import Milestone.Task.Survey as Survey

type alias Type = { types : Array Task
                  , selected : Int
                  }

type Task = PeerReview PeerReview.Type
          | ControlledReview ControlledReview.Type
          | Survey Survey.Type

new : Type
new = { types = Array.fromList [ PeerReview PeerReview.new
                               , ControlledReview ControlledReview.new
                               , Survey Survey.new
                               ]
      , selected = 0
      }

render : Activator m -> Wrapper Type m -> List Html -> Type -> Html
render activate wrap controls task =
  Html.div [  ]
           [ renderSelected activate wrap controls task ]

renderSelected : Activator m -> Wrapper Type m -> List Html -> Type -> Html
renderSelected activate wrap controls task =
  let wrap' constructor = (\t m -> wrap { task | types <- Array.set task.selected (constructor t) task.types } m )
      controls' = controls ++ [nextTask activate wrap task, prevTask activate wrap task]
  in case (Array.get task.selected task.types) of
       Nothing -> Html.div [] [ Html.text "An error occurred." ]
       Just (PeerReview review) -> PeerReview.render activate (wrap' PeerReview) controls' review
       Just (ControlledReview review) -> ControlledReview.render activate (wrap' ControlledReview) controls' review
       Just (Survey survey) -> Survey.render activate (wrap' Survey) controls' survey

nextTask = updateSelected "small-button next-button" "Next Task" 1
prevTask = updateSelected "small-button prev-button" "Prev Task" (-1)

updateSelected : String -> String -> Int -> Activator m -> Wrapper Type m -> Type -> Html
updateSelected class tip diff activate wrap task =
  let selected' = (task.selected + diff) % Array.length task.types
      task' = { task | selected <- selected' }
  in Html.input [ Attributes.class class 
                , Attributes.type' "button"
                , Attributes.title tip
                , activate (flip Events.onClick (\m -> wrap task' m))
                ] []

style = PeerReview.style ++
        ControlledReview.style ++
        Survey.style 
