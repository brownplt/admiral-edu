module Test where
import Common exposing (..)

import Array
import Array exposing (Array)

import Html
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes

import Rubric

import StartApp

import Task.PeerReview as PeerReview
import Task.ControlledReview as ControlledReview
import Task.Survey as Survey

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
           [ Html.text "test" ]

renderSelected : Activator m -> Wrapper Type m -> List Html -> Type -> Html
renderSelected activate wrap controls task =
  let wrap' constructor = (\t m -> wrap { task | types <- Array.set task.selected (constructor t) task.types } m )
  in case (Array.get task.selected task.types) of
       Nothing -> Html.div [] [ Html.text "An error occurred." ]
       Just (PeerReview review) -> PeerReview.render activate (wrap' PeerReview) controls review
       Just (ControlledReview review) -> ControlledReview.render activate (wrap' ControlledReview) controls review
       Just (Survey survey) -> Survey.render activate (wrap' Survey) controls survey



main = StartApp.start { model = 5, update = (\id _ -> id), view = (\_ _ -> Html.div [] [Html.text "test"]) }


