module Editable (..) where

{-| An Editable is a wrapper for a value which
    also contains an editing Boolean field.
 -}
type alias Editable a = { value : a
                        , editing : Bool 
                        }

{-| Creates an Editable with the specified value and editing set to false. -}
editable : a -> Editable a
editable n = Editable n False
