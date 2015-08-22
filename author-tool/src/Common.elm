module Common where

import Array
import Array.Extra

import Signal exposing (Address)
import Html exposing (Attribute)

type alias Action m = m -> m
type alias Activator m = (Address (Action m) -> Attribute) -> Attribute
type alias Wrapper t m = t -> Action m

insertAt ix x xs =
  let (left, right) = Array.Extra.splitAt ix xs
  in Array.push x left  |> flip Array.append right
