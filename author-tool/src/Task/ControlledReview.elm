module Task.PeerReview where
import Common exposing (..)

import Editable exposing (..)

import Html
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events


import Rubric

import StartApp

import String

type alias Type = { rubric : Editable Rubric.Type
                  }

new : Type
new = { rubric = { editing = True
                 , value = Rubric.new
                 }
      }

render : Activator m -> Wrapper Type m -> Type -> Html
render activate wrap review =
  let content = if | review.rubric.editing -> Html.div [ Attributes.class "controlled-review-body" ] [rubric activate wrap review]
                   | otherwise -> Html.div [] []
  in
  Html.div [ Attributes.class "controlled-review" ] 
           [ Html.div [ Attributes.class <| String.append "controlled-review-header"  <| if | review.rubric.editing -> ""
                                                                                            | otherwise -> " controlled-review-border-bottom" ]
                      [ title activate wrap review  ]
           , content
           ]

title : Activator m -> Wrapper Type m -> Type -> Html
title activate wrap review =
  Html.p [ ]  
          [ Html.span [ Attributes.class "controlled-review-title" ] [ Html.text "Controlled Review" ]
          , if | review.rubric.editing -> hiderubric activate wrap review
               | otherwise -> showrubric activate wrap review
          ]

rubric : Activator m -> Wrapper Type m -> Type -> Html
rubric activate wrap review =
  let wrap' = (\rubric m -> wrap { review | rubric <- { editing = True, value = rubric } } m)
  in Rubric.render activate wrap' review.rubric.value 

hiderubric = updaterubric "(Hide)" False
showrubric = updaterubric "(Show)" True                                   

updaterubric : String -> Bool -> Activator m -> Wrapper Type m -> Type -> Html
updaterubric label val activate wrap review =
  let r = review.rubric
      r' = { r | editing <- val }
      review' = { review | rubric <- r' }
  in
  Html.input [ Attributes.class "controlled-review-show"
             , Attributes.type' "button"
             , Attributes.value label
             , activate (flip Events.onClick (\m -> wrap review' m))
             ] []
                                                        

style' = Rubric.style ++ style

style = """

.controlled-review {
  width: 600px;
  margin: auto;
}

.controlled-review-button {
  margin: 0px 5px 0px 5px;
}

input.controlled-review-show {
  text-decoration: underline;
  background: none;
  border: none;
  padding: 0;
  color: black;
}

input.controlled-review-show:hover {
  font-weight: bold;
}

span.controlled-review-title {
  font-weight: bold;
  margin: 5px;
}

.controlled-review-header {
  border-top: solid black 1px;
  border-left: solid black 1px;
  border-right: solid black 1px;
  border-radius: 5px 5px 0px 0px;
  width: 300px;
  padding: 5px;
}

.controlled-review-border-bottom {
  border: solid black 1px;
  border-radius: 5px;
}

.controlled-review-header p {
  margin: 0px;
}

.controlled-review-body p {
  margin:0px;
}

.controlled-review-body {
  border: solid black 1px;
  border-radius: 0px 5px 5px 5px;
  padding: 5px;
}


"""
script = Rubric.script

model = new

update f m = f m

view address model = 
  Html.div [] 
           [ Html.node "script" [] [ Html.text script ]
           , Html.node "style" [] [ Html.text style' ]
           , render (\event -> event address) (\t _ -> t) model
           ]
           

main = StartApp.start { model = model, update = update, view = view }
