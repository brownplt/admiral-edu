module Common where
import Signal exposing (Address)
import Html exposing (Attribute)

type alias Action m = m -> m
type alias Activator m = (Address (Action m) -> Attribute) -> Attribute
type alias Wrapper t m = t -> Action m
