module Editable (..) where

{-| An Editable is a wrapper for a value which
    also contains an editing Boolean field.
 -}
type alias Editable a = { value : a
                        , editing : Bool 
                        }

editable = new

{-| Creates an Editable with the specified value and editing set to false. -}
new : a -> Editable a
new n = Editable n False
