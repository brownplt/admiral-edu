module Task.PeerReview where
import Common exposing (..)

import Editable exposing (..)

import Html
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events


import Rubric

import StartApp

type alias Type = { amount : Int
                  , rubric : Editable Rubric.Type
                  }

new : Type
new = { amount = 3
      , rubric = { editing = True
                 , value = Rubric.new
                 }
      }

render : Activator m -> Wrapper Type m -> Type -> Html
render activate wrap review =
  Html.div [ Attributes.class "peer-review" ] 
           [ Html.div [ Attributes.class "peer-review-header" ]
                      [ title activate wrap review  ]
           , Html.div [ Attributes.class "peer-review-body" ] 
                      [ amount activate wrap review
                      , rubric activate wrap review 
                      ]
           ]

title : Activator m -> Wrapper Type m -> Type -> Html
title activate wrap review =
  Html.p [ ] 
          [ Html.span [ Attributes.class "peer-review-title" ] [ Html.text "Peer Review" ]
          ]

rubric : Activator m -> Wrapper Type m -> Type -> Html
rubric activate wrap review =
  let wrap' = (\rubric m -> wrap { review | rubric <- { editing = True, value = rubric } } m)
      label =  if | review.rubric.editing -> hiderubric activate wrap review
                  | otherwise -> showrubric activate wrap review
      content = if | review.rubric.editing -> Rubric.render activate wrap' review.rubric.value
                   | otherwise -> Html.div [] []
  in Html.div [ Attributes.class "peer-review-rubric" ] [ label, content ]

hiderubric = updaterubric "Rubric (Hide)" False
showrubric = updaterubric "Rubric (Show)" True                                   

updaterubric : String -> Bool -> Activator m -> Wrapper Type m -> Type -> Html
updaterubric label val activate wrap review =
  let r = review.rubric
      r' = { r | editing <- val }
      review' = { review | rubric <- r' }
  in
  Html.input [ Attributes.class "peer-review-show"
             , Attributes.type' "button"
             , Attributes.value label
             , activate (flip Events.onClick (\m -> wrap review' m))
             ] []
                                                        

amount : Activator m -> Wrapper Type m -> Type -> Html
amount activate wrap review =
  Html.div [] [ Html.p [] [ Html.span [ Attributes.class "peer-review-label" ] 
                                      [ Html.text "Reviews" ]
                          , Html.text ": "
                          , decreaseButton activate wrap review
                          , toString review.amount  |> Html.text
                          , increaseButton activate wrap review
                          ]
              ]

decreaseButton = changeAmountButton "-" (-1)
increaseButton = changeAmountButton "+" 1

changeAmountButton : String -> Int -> Activator m -> Wrapper Type m -> Type -> Html
changeAmountButton label diff activate wrap review =
  let amount = max (review.amount + diff) 1
      review' = { review | amount <- amount }
  in Html.input [ Attributes.class "peer-review-button"
                , Attributes.type' "button"
                , Attributes.value label
                , activate (flip Events.onClick (\m -> wrap review' m))
                ]
                []


style = Rubric.style ++ style'

style' = """

.peer-review {
  width: 600px;
  margin: auto;
}

.peer-review-button {
  margin: 0px 5px 0px 5px;
}

input.peer-review-show {
  text-decoration: underline;
  background: none;
  border: none;
  padding: 0;
  color: black;
}

input.peer-review-show:hover {
  font-weight: bold;
}

span.peer-review-title {
  font-weight: bold;
  margin: 5px;
}

.peer-review-header {
  border-top: solid black 1px;
  border-left: solid black 1px;
  border-right: solid black 1px;
  border-radius: 5px 5px 0px 0px;
  width: 300px;
  padding: 5px;
}

.peer-review-header p {
  margin: 0px;
}

.peer-review-body p {
  margin:0px;
}

.peer-review-body {
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
           , Html.node "style" [] [ Html.text style]
           , render (\event -> event address) (\t _ -> t) model
           ]
           

main = StartApp.start { model = model, update = update, view = view }
