module Encoders exposing (newItemEncoder)

import Json.Encode exposing (Value, object, string)
import Models exposing (Item)


newItemEncoder : Item -> Value
newItemEncoder item =
    object
        [ ( "title", string item.title )
        , ( "type", string item.itemType )
        ]
