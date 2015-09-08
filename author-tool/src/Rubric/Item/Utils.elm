module Rubric.Item.Utils (textbox, textarea, sanitize, style, script, markdown) where

import String
import Signal exposing (Address)
import Html exposing (Html, Attribute)
import Html
import Html.Events as Events
import Html.Attributes as Attributes
import Editable exposing (..)

import Markdown

sanitize : String -> String
sanitize s =
  let s' = String.join "\\n" (String.split "\n" s)
      s'' = String.join "\\\"" (String.split "\"" s')
  in s''


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

markdown : ((Address a -> Attribute) -> Attribute) -> (Editable String -> a) -> Editable String -> String -> Html
markdown activate wrap text default =
  if | text.editing -> Html.textarea [ activate <| flip Events.onBlur (wrap { text | editing <- False })
                                     , activate <| onInput (\str -> wrap { text | value <- str })
                                     , Attributes.class "focus"
                                     ] [ Html.text text.value ]
                                                    

     | otherwise -> Html.span [ activate (flip Events.onClick (wrap { text | editing <- True } )) ]
                              [ Markdown.toHtml (if | text.value == "" -> default
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

style = """

.focus {
  animation-name: set-focus;
  animation-duration: 0.001s;
  -webkit-animation-name: set-focus;
  -webkit-animation-duration: 0.001s;
  -moz-animation-name: set-focus;
  -moz-animation-duration: 0.001s;  
}

@-moz-keyframes set-focus {
    0%   {color: #fff}
}

@-webkit-keyframes set-focus {
    0%   {color: #fff}
}

keyframes set-focus {
    0%   {color: #fff}
}

"""

script = """
var insertListener = function(event){
 if (event.animationName == "set-focus") {
   event.target.focus();
 } 
 if (event.animationName == "set-export-hack") {
   alert("test")
 }
}
document.addEventListener("animationstart", insertListener, false); // standard + firefox
document.addEventListener("MSAnimationStart", insertListener, false); // IE
document.addEventListener("webkitAnimationStart", insertListener, false); // Chrome + Safari
document.addEventListener("mozAnimationStart", insertListener, false); // Chrome + Safari
"""
