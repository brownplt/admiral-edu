module Rubric.Item.Utils (textbox, textarea) where

import String
import Signal exposing (Address)
import Html exposing (Html, Attribute)
import Html
import Html.Events as Events
import Html.Attributes as Attributes
import Editable exposing (..)

textbox : ((Address a -> Attribute) -> Attribute) -> (Editable String -> a) -> Editable String -> String -> Html
textbox activate wrap text default = 
  if | text.editing -> Html.input [ Attributes.type' "text"
                                  , Attributes.value text.value
                                  , Attributes.size (max (String.length text.value) 10)
                                  , activate (flip Events.onBlur (wrap { text | editing <- False }))
                                  , activate (onInput (\str -> wrap { text | value <- str }))
                                  , activate (flip Events.onKeyPress (\n -> if | n == 13 -> wrap { text | editing <- False }
                                                                               | otherwise -> wrap text ))
                                  , Attributes.class "focus"
                                  ] []
                                                    

     | otherwise -> Html.span [ activate (flip Events.onClick (wrap { text | editing <- True } )) ]
                              [ Html.text (if | text.value == "" -> default
                                              | otherwise -> text.value
                                          )
                              ]

textarea : ((Address a -> Attribute) -> Attribute) -> (Editable String -> a) -> Editable String -> String -> Html
textarea activate wrap text default =
  if | text.editing -> Html.textarea [ activate <| flip Events.onBlur (wrap { text | editing <- False })
                                     , activate <| onInput (\str -> wrap { text | value <- str })
                                     , Attributes.class "focus"
                                     ] [ Html.text text.value ]
                                                    

     | otherwise -> Html.span [ activate (flip Events.onClick (wrap { text | editing <- True } )) ]
                              [ Html.text (if | text.value == "" -> default
                                              | otherwise -> text.value
                                          )
                              ]
onInput contentToValue address =
    Events.on "input" Events.targetValue (\str -> Signal.message address (contentToValue str))
